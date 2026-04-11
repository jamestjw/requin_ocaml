open Base
module BB = Bitboard.Bitboard
module P = Position.Position
module M = Movegen.MoveGen
module T = Types.Types
module K = Killer
module TT = Transposition_table
module Eval = Evaluation
module Task = Domainslib.Task
module History = History

let parallel = 8

(* Also known as R *)
(* TODO: make this dynamic *)
let null_move_reduction = 3
let qsearch_max_depth = 4
let qsearch_check_depth = 2
let qsearch_delta_margin = T.queen_value + T.pawn_value
let lmr_depth_threshold = 3
let lmr_move_threshold = 3

let lmr_reduction remaining_depth move_index =
  let depth_bonus = Int.max 0 ((remaining_depth - lmr_depth_threshold) / 2) in
  let move_bonus = Int.max 0 ((move_index - lmr_move_threshold) / 4) in
  Int.min (remaining_depth - 1) (1 + depth_bonus + move_bonus)
;;

let lmp_move_threshold = function
  | 1 -> 4
  | 2 -> 8
  | _ -> Int.max_value
;;

(* TODO: Tune aspiration window size based on fail rate. *)
let aspiration_window = 100
let initial_alpha = -T.value_mate - 1
let initial_beta = T.value_mate + 1
let generate_moves pos = M.generate_legal pos

let score_is_resolved_mate score completed_depth =
  let mate_distance = T.value_mate - Int.abs score in
  Int.abs score > T.value_mate_in_max_ply && mate_distance <= completed_depth
;;

let capture_order_score pos move =
  let victim_value =
    match P.piece_on pos (T.move_dst move) with
    | Some piece -> T.piece_value piece
    | None -> 0
  in
  let attacker_value = P.piece_on_exn pos (T.move_src move) |> T.piece_value in
  let promotion_bonus = if T.is_promotion move then T.queen_value else 0 in
  (victim_value * 16) - attacker_value + promotion_bonus
;;

let pick_next_move scored_moves start_idx =
  let len = Array.length scored_moves in
  let rec loop idx best_idx best_score =
    if idx >= len
    then best_idx
    else (
      let _, score = scored_moves.(idx) in
      if score > best_score
      then loop (idx + 1) idx score
      else loop (idx + 1) best_idx best_score)
  in
  let _, start_score = scored_moves.(start_idx) in
  let best_idx = loop (start_idx + 1) start_idx start_score in
  if best_idx <> start_idx
  then (
    let tmp = scored_moves.(start_idx) in
    scored_moves.(start_idx) <- scored_moves.(best_idx);
    scored_moves.(best_idx) <- tmp)
;;

let ordered_moves_by_score moves ~score =
  List.map moves ~f:(fun move -> move, score move)
  |> Array.of_list
  |> fun scored_moves ->
  let rec loop idx acc =
    if idx >= Array.length scored_moves
    then List.rev acc
    else (
      pick_next_move scored_moves idx;
      let move, _ = scored_moves.(idx) in
      loop (idx + 1) (move :: acc))
  in
  loop 0 []
;;

type stats =
  { mutable nodes : int
  ; mutable qnodes : int
  ; mutable cutoffs : int
  ; mutable fail_high : int
  ; mutable fail_low : int
  ; mutable tt_probes : int
  ; mutable tt_hits : int
  ; mutable tt_exact : int
  ; mutable tt_lower : int
  ; mutable tt_upper : int
  ; mutable tt_cutoffs : int
  ; mutable first_move_cutoffs : int
  ; mutable cutoff_index_sum : int
  ; mutable cutoff_count : int
  }

type info =
  { depth : int
  ; score : int
  ; nodes : int
  ; nps : int
  ; tthit : int
  ; cut : int
  }

type pv_info =
  { depth : int
  ; pv : T.move list
  }

type instrumentation =
  { on_info : info -> unit
  ; on_pv : pv_info -> unit
  }

type move_stage =
  | Stage_hash
  | Stage_evasions
  | Stage_good_captures
  | Stage_killers
  | Stage_quiets
  | Stage_bad_captures
  | Stage_done

type move_picker =
  { pos : P.t
  ; in_check : bool
  ; history_tbl : History.t
  ; hash_move : T.move option
  ; killer_moves : T.move list
  ; mutable stage : move_stage
  ; mutable evasions : T.move list option
  ; mutable good_captures : T.move list option
  ; mutable quiets : T.move list option
  ; mutable bad_captures : T.move list option
  ; mutable stage_moves : T.move list
  }

let mk_stats () =
  { nodes = 0
  ; qnodes = 0
  ; cutoffs = 0
  ; fail_high = 0
  ; fail_low = 0
  ; tt_probes = 0
  ; tt_hits = 0
  ; tt_exact = 0
  ; tt_lower = 0
  ; tt_upper = 0
  ; tt_cutoffs = 0
  ; first_move_cutoffs = 0
  ; cutoff_index_sum = 0
  ; cutoff_count = 0
  }
;;

let merge_stats (acc : stats) (s : stats) =
  acc.nodes <- acc.nodes + s.nodes;
  acc.qnodes <- acc.qnodes + s.qnodes;
  acc.cutoffs <- acc.cutoffs + s.cutoffs;
  acc.fail_high <- acc.fail_high + s.fail_high;
  acc.fail_low <- acc.fail_low + s.fail_low;
  acc.tt_probes <- acc.tt_probes + s.tt_probes;
  acc.tt_hits <- acc.tt_hits + s.tt_hits;
  acc.tt_exact <- acc.tt_exact + s.tt_exact;
  acc.tt_lower <- acc.tt_lower + s.tt_lower;
  acc.tt_upper <- acc.tt_upper + s.tt_upper;
  acc.tt_cutoffs <- acc.tt_cutoffs + s.tt_cutoffs;
  acc.first_move_cutoffs <- acc.first_move_cutoffs + s.first_move_cutoffs;
  acc.cutoff_index_sum <- acc.cutoff_index_sum + s.cutoff_index_sum;
  acc.cutoff_count <- acc.cutoff_count + s.cutoff_count;
  acc
;;

let default_instrumentation =
  { on_info = (fun (_ : info) -> ()); on_pv = (fun (_ : pv_info) -> ()) }
;;

let killer_is_usable pos hash_move move =
  T.move_is_ok move
  && (not (Option.value_map hash_move ~default:false ~f:(T.equal_move move)))
  && (not (P.is_capture pos move))
  && (not (T.is_promotion move))
  && P.pseudo_legal pos move
  && P.legal pos move
;;

let move_in_list moves target = List.exists moves ~f:(T.equal_move target)

let is_legal_generated_move pos =
  let us = P.side_to_move pos in
  let pinned = P.blockers_for_king pos us |> BB.bb_and @@ P.pieces_of_colour pos us in
  let king_sq = P.square_of_pt_and_colour pos T.KING us in
  fun move ->
    let src_sq = T.move_src move in
    not
    @@ (((BB.bb_not_zero @@ BB.sq_and_bb src_sq pinned)
         || T.equal_square src_sq king_sq
         || T.equal_move_type (T.get_move_type move) T.EN_PASSANT)
        && not (P.legal pos move))
;;

let generated_capture_moves pos =
  let is_legal = is_legal_generated_move pos in
  assert (not (BB.bb_not_zero @@ P.checkers pos));
  M.generate M.CAPTURES pos |> List.filter ~f:is_legal
;;

let generated_quiet_moves pos =
  generate_moves pos |> List.filter ~f:(fun move -> not (P.is_capture pos move))
;;

let get_evasions picker =
  match picker.evasions with
  | Some moves -> moves
  | None ->
    let moves =
      generate_moves picker.pos
      |> List.filter ~f:(fun move ->
        not (Option.value_map picker.hash_move ~default:false ~f:(T.equal_move move)))
      |> ordered_moves_by_score ~score:(fun move ->
        if P.is_capture picker.pos move
        then
          if T.is_promotion move || P.see_ge picker.pos move 1
          then 2000000 + capture_order_score picker.pos move
          else -2000000 + capture_order_score picker.pos move
        else History.get picker.history_tbl move (P.moved_piece_exn picker.pos move))
    in
    picker.evasions <- Some moves;
    moves
;;

let populate_capture_buckets picker =
  match picker.good_captures, picker.bad_captures with
  | Some _, Some _ -> ()
  | _ ->
    let good_captures, bad_captures =
      generated_capture_moves picker.pos
      |> List.fold ~init:([], []) ~f:(fun (good_acc, bad_acc) move ->
        if P.is_capture_stage picker.pos move
        then
          if T.is_promotion move || P.see_ge picker.pos move 1
          then move :: good_acc, bad_acc
          else good_acc, move :: bad_acc
        else good_acc, bad_acc)
    in
    picker.good_captures
    <- Some (ordered_moves_by_score good_captures ~score:(capture_order_score picker.pos));
    picker.bad_captures
    <- Some
         (ordered_moves_by_score bad_captures ~score:(fun move ->
            -2000000 + capture_order_score picker.pos move))
;;

let get_good_captures picker =
  populate_capture_buckets picker;
  Option.value_exn picker.good_captures
;;

let get_quiets picker =
  match picker.quiets with
  | Some moves -> moves
  | None ->
    let good_captures = get_good_captures picker in
    let moves =
      generated_quiet_moves picker.pos
      |> List.filter ~f:(fun move ->
        (not (move_in_list good_captures move))
        && (not (List.exists picker.killer_moves ~f:(T.equal_move move)))
        && not (Option.value_map picker.hash_move ~default:false ~f:(T.equal_move move)))
      |> ordered_moves_by_score ~score:(fun move ->
        History.get picker.history_tbl move (P.moved_piece_exn picker.pos move))
    in
    picker.quiets <- Some moves;
    moves
;;

let get_bad_captures picker =
  populate_capture_buckets picker;
  Option.value_exn picker.bad_captures
;;

let refill_move_picker picker =
  match picker.stage with
  | Stage_hash ->
    picker.stage <- (if picker.in_check then Stage_evasions else Stage_good_captures);
    picker.stage_moves <- Option.to_list picker.hash_move
  | Stage_evasions ->
    picker.stage <- Stage_done;
    picker.stage_moves <- get_evasions picker
  | Stage_good_captures ->
    picker.stage <- Stage_killers;
    picker.stage_moves <- get_good_captures picker
  | Stage_killers ->
    picker.stage <- Stage_quiets;
    picker.stage_moves
    <- List.filter picker.killer_moves ~f:(killer_is_usable picker.pos picker.hash_move)
  | Stage_quiets ->
    picker.stage <- Stage_bad_captures;
    picker.stage_moves <- get_quiets picker
  | Stage_bad_captures ->
    picker.stage <- Stage_done;
    picker.stage_moves <- get_bad_captures picker
  | Stage_done -> picker.stage_moves <- []
;;

let rec next_move picker =
  match picker.stage_moves with
  | move :: rest ->
    picker.stage_moves <- rest;
    Some move
  | [] ->
    if Poly.(picker.stage = Stage_done)
    then None
    else (
      refill_move_picker picker;
      next_move picker)
;;

let mk_move_picker pos ~history_tbl ~hash_move ~killer_moves =
  { pos
  ; in_check = P.is_in_check pos
  ; history_tbl
  ; hash_move
  ; killer_moves
  ; stage = Stage_hash
  ; evasions = None
  ; good_captures = None
  ; quiets = None
  ; bad_captures = None
  ; stage_moves = []
  }
;;

let pv_from_tt (pos : P.t) tt max_len =
  (* TODO: Build PV from search return instead of TT. *)
  let rec loop pos acc remaining =
    if remaining <= 0
    then List.rev acc
    else (
      match TT.probe tt (P.key pos) with
      | None -> List.rev acc
      | Some entry when T.move_not_none entry.move && P.legal pos entry.move ->
        loop (P.do_move' pos entry.move) (entry.move :: acc) (remaining - 1)
      | Some _ -> List.rev acc)
  in
  loop pos [] max_len
;;

let rec qsearch pos alpha beta is_white ply history ~stats ~qdepth =
  stats.qnodes <- stats.qnodes + 1;
  let in_check = P.is_in_check pos in
  let offset = if is_white then 1 else -1 in
  let stand_pat = offset * Eval.evaluate pos () in
  let alpha =
    if not in_check then if stand_pat > alpha then stand_pat else alpha else alpha
  in
  if (not in_check) && stand_pat >= beta
  then beta
  else if (not in_check) && stand_pat + qsearch_delta_margin <= alpha
  then alpha
  else (
    let moves =
      if in_check
      then generate_moves pos
      else
        generate_moves pos
        |> List.filter ~f:(fun move ->
          match T.get_ppt move with
          | Some T.QUEEN -> true
          | Some _ -> false
          | None -> P.is_capture pos move && P.see_ge pos move 1)
        |> List.filter ~f:(fun move ->
          let move_gain =
            match P.piece_on pos (T.move_dst move) with
            | Some p -> T.piece_value p
            | None -> if T.is_promotion move then T.queen_value else 0
          in
          stand_pat + move_gain + qsearch_delta_margin > alpha)
    in
    if List.is_empty moves
    then if in_check then -(T.value_mate - ply) else alpha
    else (
      let sorted_moves =
        ordered_moves_by_score moves ~score:(fun m ->
          if in_check
          then 2000000 + capture_order_score pos m
          else 2000000 + capture_order_score pos m)
      in
      let rec loop alpha = function
        | [] -> alpha
        | m :: rest ->
          let score =
            -qsearch
               (P.do_move' pos m)
               (-beta)
               (-alpha)
               (not is_white)
               (ply + 1)
               (m :: history)
               ~stats
               ~qdepth:(Int.max 0 (qdepth - 1))
          in
          if score >= beta
          then (
            stats.cutoffs <- stats.cutoffs + 1;
            stats.fail_high <- stats.fail_high + 1;
            beta)
          else loop (Int.max alpha score) rest
      in
      loop alpha sorted_moves))
;;

let rec pvSearch
          pos
          curr_ply
          max_depth
          alpha
          beta
          is_white
          ply
          history
          ~(stats : stats)
          ~may_prune
          ~tt
          ~killers
          ~is_null_window
          ~is_pv
          ~history_tbl
  =
  stats.nodes <- stats.nodes + 1;
  (* Mate scores are ply-relative (distance to mate). We normalize when storing
     so TT entries remain comparable across different search plies, and restore
     when reading. *)
  let value_to_tt value curr_ply =
    if value > T.value_mate_in_max_ply
    then value + curr_ply
    else if value < T.value_mated_in_max_ply
    then value - curr_ply
    else value
  in
  let value_from_tt value curr_ply =
    if value > T.value_mate_in_max_ply
    then value - curr_ply
    else if value < T.value_mated_in_max_ply
    then value + curr_ply
    else value
  in
  (* TODO: Can I add compile time constant for debugging? *)
  (* Stdlib.print_endline *)
  (* @@ String.concat ~sep:" " *)
  (* @@ List.rev *)
  (* @@ List.map ~f:T.show_move history; *)
  (* Whether or not we should attempt null move pruning *)

  (* Attempt null move pruning if we can, return an optional score if we get a cutoff *)
  let remaining_depth = max_depth - curr_ply in
  let offset = if is_white then 1 else -1 in
  let eval_value = offset * Eval.evaluate pos () in
  let maybe_attempt_nmp pos =
    let stm = P.side_to_move pos in
    let has_non_pawn_material =
      P.count_by_colour_and_pt pos stm T.KNIGHT
      + P.count_by_colour_and_pt pos stm T.BISHOP
      + P.count_by_colour_and_pt pos stm T.ROOK
      + P.count_by_colour_and_pt pos stm T.QUEEN
      > 0
    in
    let may_do_nmp =
      (* TODO: check for zugzwang, we could do something simple like check if there
       are only kings and pawns *)
      remaining_depth >= null_move_reduction
      && (not (P.is_in_check pos))
      && (not is_pv)
      && has_non_pawn_material
      && eval_value >= beta
      (* Add the following conditions
       - !is_cut_node, we are likely to get a cut off in, don't risk it
       - !(static eval > beta + 50 centipawns), because the position is so good, we are likely to get a cutoff anyway *)
    in
    if may_do_nmp
    then (
      let score =
        -1
        * pvSearch
            (P.do_null_move pos)
            (curr_ply + null_move_reduction)
            max_depth
            (-beta)
            (-beta + 1)
            (not is_white)
            (ply + 1)
            (T.null_move :: history)
            ~stats
            ~may_prune
            ~tt
            ~killers
            ~is_null_window
            ~is_pv:false
            ~history_tbl
      in
      if score >= beta then Some score else None)
    else None
  in
  let search_with_ply move alpha beta extra_ply ~is_null_window ~is_pv =
    -1
    * pvSearch
        (P.do_move' pos move)
        (curr_ply + 1 + extra_ply)
        max_depth
        (-beta)
        (-alpha)
        (not is_white)
        (ply + 1)
        (move :: history)
        ~stats
        ~may_prune:(not @@ P.is_capture pos move)
        ~tt
        ~killers
        ~is_null_window
        ~is_pv
        ~history_tbl
  in
  let search move alpha beta ~is_null_window ~is_pv =
    search_with_ply move alpha beta 0 ~is_null_window ~is_pv
  in
  let is_in_check = P.is_in_check pos in
  let alpha_orig = alpha in
  let do_move quiet_moves (alpha, best_move, is_first_move, idx) move =
    let is_quiet = not (P.is_capture pos move || T.is_promotion move) in
    let can_lmp =
      (* Skip very late quiet moves in shallow non-PV null-window nodes 
         because they are unlikely to raise alpha. *)
      is_null_window
      && may_prune
      && (not is_pv)
      && (not is_in_check)
      && is_quiet
      && remaining_depth = 2
      && idx >= lmp_move_threshold remaining_depth
    in
    if can_lmp
    then Continue_or_stop.Continue (alpha, best_move, false, idx + 1)
    else (
      let score =
        if is_first_move
        then search move alpha beta ~is_null_window:false ~is_pv
        else (
          (* Search with null window, i.e. with [alpha , alpha + 1] *)
          (* Since this isn't the first move, by definition it isn't in the PV *)
          let can_lmr =
            may_prune
            && (not is_pv)
            && (not is_in_check)
            && remaining_depth >= lmr_depth_threshold
            && idx >= lmr_move_threshold
            && is_quiet
          in
          let score =
            if can_lmr
            then (
              let reduced_score =
                search_with_ply
                  move
                  alpha
                  (alpha + 1)
                  (lmr_reduction remaining_depth idx)
                  ~is_null_window:true
                  ~is_pv:false
              in
              if reduced_score > alpha
              then search move alpha (alpha + 1) ~is_null_window:true ~is_pv:false
              else reduced_score)
            else search move alpha (alpha + 1) ~is_null_window:true ~is_pv:false
          in
          if score > alpha && beta - alpha > 1
          then
            (* re-search with full window *)
            search move alpha beta ~is_null_window:false ~is_pv:false
          else score)
      in
      if score >= beta
      then (
        stats.cutoffs <- stats.cutoffs + 1;
        stats.fail_high <- stats.fail_high + 1;
        stats.cutoff_index_sum <- stats.cutoff_index_sum + idx;
        stats.cutoff_count <- stats.cutoff_count + 1;
        if is_first_move then stats.first_move_cutoffs <- stats.first_move_cutoffs + 1;
        if T.move_is_ok move && is_quiet
        then (
          K.add_killer killers ply move;
          History.update history_tbl move remaining_depth (P.moved_piece_exn pos move);
          List.iter quiet_moves ~f:(fun quiet_move ->
            History.penalize history_tbl quiet_move remaining_depth));
        ignore
        @@ TT.store
             tt
             ~key:(P.key pos)
             ~m:move
             ~depth:remaining_depth
             ~eval_value
             ~value:(value_to_tt score curr_ply)
             ~bound:TT.BOUND_LOWER;
        Continue_or_stop.Stop (beta, best_move, true, idx))
      else if score > alpha
      then Continue_or_stop.Continue (score, Some move, false, idx + 1)
      else if not is_quiet
      then Continue_or_stop.Continue (alpha, best_move, false, idx + 1)
      else Continue_or_stop.Continue (alpha, best_move, false, idx + 1))
  in
  let do_move' alpha move ~is_first_move ~idx =
    match do_move [] (alpha, None, is_first_move, idx) move with
    | Continue_or_stop.Continue (score, m, _, _) -> score, m, false
    | Continue_or_stop.Stop (score, m, cut, _) -> score, m, cut
  in
  (* Try to get an early exit score from the TT entry, also evaluates the hash
    move if it exists *)
  let process_tt_entry depth (tt_entry : TT.entry option) alpha =
    match tt_entry with
    | Some tt_entry ->
      stats.tt_hits <- stats.tt_hits + 1;
      if tt_entry.depth >= depth
      then (
        match tt_entry.bound with
        | TT.BOUND_EXACT -> stats.tt_exact <- stats.tt_exact + 1
        | TT.BOUND_LOWER -> stats.tt_lower <- stats.tt_lower + 1
        | TT.BOUND_UPPER -> stats.tt_upper <- stats.tt_upper + 1
        | TT.BOUND_NONE -> ())
      else ();
      let tt_value = value_from_tt tt_entry.value curr_ply in
      let score =
        match tt_entry.depth >= depth, tt_entry.bound with
        | true, TT.BOUND_EXACT ->
          (* TODO: Check if we are at a Pv node before returning this *)
          Some tt_value
        | true, TT.BOUND_LOWER when tt_value >= beta -> Some tt_value
        | true, TT.BOUND_UPPER when tt_value <= alpha -> Some tt_value
        | _, _ -> None
      in
      (match score with
       | Some _ -> stats.tt_cutoffs <- stats.tt_cutoffs + 1
       | None -> ());
      let alpha =
        match tt_entry.depth >= depth, tt_entry.bound with
        | true, TT.BOUND_LOWER -> Int.max alpha tt_value
        | _ -> alpha
      in
      (match score, tt_entry.bound with
       | Some _, _ -> score, alpha, false
       | _, (TT.BOUND_EXACT | TT.BOUND_LOWER)
         when T.move_not_none tt_entry.move && P.legal pos tt_entry.move ->
         (* Search hash move *)
         let score, _, is_cut = do_move' alpha tt_entry.move ~is_first_move:true ~idx:0 in
         if is_cut then Some score, alpha, true else None, score, true
       | _, _ -> score, alpha, false)
    | None -> None, alpha, false
  in
  (* This takes into account the 50 move rule and threehold repetition *)
  if P.is_draw pos ply
  then T.value_draw
  else if curr_ply = T.max_ply
  then eval_value
  else if remaining_depth <= 0
  then qsearch pos alpha beta is_white ply history ~stats ~qdepth:qsearch_max_depth
  else if
    false
    && (not is_in_check)
    && (not is_null_window)
    && remaining_depth = 1
    && may_prune
    && eval_value + T.futility_margin_1 < alpha
  then
    (* If a move proves to be futile, we just return alpha since *)
    (* further continuations are unlikely to raise alpha *)
    alpha
  else if
    (not is_in_check)
    && (not is_null_window)
    && remaining_depth = 2
    && may_prune
    && eval_value + T.futility_margin_2 < alpha
  then
    (* Same as above *)
    alpha
  else (
    stats.tt_probes <- stats.tt_probes + 1;
    let tt_entry = TT.probe tt (P.key pos) in
    match process_tt_entry remaining_depth tt_entry alpha with
    | Some score, _, _ -> score
    | _, alpha, found_hash_move ->
      (match maybe_attempt_nmp pos with
       | Some score -> score
       | _ ->
         let hash_move = None in
         let move_picker =
           mk_move_picker
             pos
             ~history_tbl
             ~hash_move
             ~killer_moves:(K.get_killers killers ply)
         in
         (match next_move move_picker with
          | None ->
            (* Either draw or mate *)
            if P.is_in_check pos then -(T.value_mate - curr_ply) else T.value_draw
          | Some first_move ->
            let rec loop_moves acc current_move =
              match do_move [] acc current_move with
              | Continue_or_stop.Continue next_acc ->
                (match next_move move_picker with
                 | None ->
                   let a, b, _, i = next_acc in
                   a, b, false, i
                 | Some move -> loop_moves next_acc move)
              | Continue_or_stop.Stop (score, best_move, _, next_idx) ->
                score, best_move, true, next_idx
            in
            let score, best_move, is_cut, _ =
              loop_moves
                (alpha, None, not found_hash_move, if found_hash_move then 1 else 0)
                first_move
            in
            if (not is_cut) && score <= alpha_orig
            then stats.fail_low <- stats.fail_low + 1;
            if not is_cut
            then (
              match best_move, is_null_window with
              | Some m, false ->
                ignore
                @@ TT.store
                     tt
                     ~key:(P.key pos)
                     ~m
                     ~depth:remaining_depth
                     ~eval_value
                     ~value:(value_to_tt score curr_ply)
                     ~bound:TT.BOUND_EXACT
              | None, _ ->
                (* It's fine to set an upper bound even if we are doing a null move search *)
                ignore
                @@ TT.store
                     tt
                     ~key:(P.key pos)
                     ~m:T.none_move
                     ~depth:remaining_depth
                     ~eval_value
                     ~value:(value_to_tt score curr_ply)
                     ~bound:TT.BOUND_UPPER
              | _ -> ());
            score)))
;;

let get_best_move ?(instrumentation = default_instrumentation) (pos : P.t) max_depth
  : T.move
  =
  let rec iterative_deepening curr_depth moves tt killers history_tbl prev_score =
    if curr_depth < max_depth
    then (
      History.decay history_tbl;
      let start_time = Stdlib.Sys.time () in
      let alpha, beta =
        match prev_score with
        | None -> initial_alpha, initial_beta
        | Some score ->
          let a = score - aspiration_window in
          let b = score + aspiration_window in
          a, b
      in
      let root_search_move move alpha beta ~is_null_window ~is_pv =
        let stats = mk_stats () in
        let score =
          -pvSearch
             (P.do_move' pos move)
             0
             curr_depth
             (-beta)
             (-alpha)
             (not (P.is_white_to_move pos))
             (P.game_ply pos + 1)
             [ move ]
             ~stats
             ~may_prune:(not @@ P.is_capture pos move)
             ~tt
             ~killers
             ~is_null_window
             ~history_tbl
             ~is_pv
        in
        score, stats
      in
      let first_move = List.hd_exn moves in
      let first_score, first_stats =
        root_search_move first_move alpha beta ~is_null_window:false ~is_pv:true
      in
      let stats = merge_stats (mk_stats ()) first_stats in
      let rest_moves = List.tl moves |> Option.value ~default:[] in
      let rec search_root_moves alpha best_move best_score move_scores = function
        | [] -> List.rev move_scores, best_move, best_score
        | move :: rest ->
          let probe_score, probe_stats =
            root_search_move move alpha (alpha + 1) ~is_null_window:true ~is_pv:false
          in
          let score, extra_stats =
            if probe_score >= alpha && beta - alpha > 1
            then (
              let full_score, full_stats =
                root_search_move move alpha beta ~is_null_window:false ~is_pv:true
              in
              full_score, merge_stats probe_stats full_stats)
            else probe_score, probe_stats
          in
          ignore (merge_stats stats extra_stats);
          if score > best_score
          then search_root_moves score move score ((move, score) :: move_scores) rest
          else
            search_root_moves
              alpha
              best_move
              best_score
              ((move, score) :: move_scores)
              rest
      in
      let move_scores, best_move, best_score =
        search_root_moves
          first_score
          first_move
          first_score
          [ first_move, first_score ]
          rest_moves
      in
      let sorted_moves =
        List.stable_sort move_scores ~compare:(fun (_, s1) (_, s2) -> compare s2 s1)
      in
      if Option.is_some prev_score && (best_score <= alpha || best_score >= beta)
      then
        (* Aspiration window failed; re-search with full window. *)
        iterative_deepening curr_depth moves tt killers history_tbl None
      else (
        let elapsed = Float.max 0.001 (Stdlib.Sys.time () -. start_time) in
        let total_nodes = stats.nodes + stats.qnodes in
        let nps = Int.of_float (Float.of_int total_nodes /. elapsed) in
        let tthit =
          if stats.tt_probes = 0 then 0 else stats.tt_hits * 100 / stats.tt_probes
        in
        let cut = if total_nodes = 0 then 0 else stats.cutoffs * 100 / total_nodes in
        instrumentation.on_info
          { depth = curr_depth + 1
          ; score = best_score
          ; nodes = total_nodes
          ; nps
          ; tthit
          ; cut
          };
        (* Seed PV with the root best move since TT entries at the root can be
           missing (e.g., upper-bound stores use none_move). *)
        let pv = best_move :: pv_from_tt (P.do_move' pos best_move) tt curr_depth in
        instrumentation.on_pv { depth = curr_depth + 1; pv };
        let sorted_moves = sorted_moves |> List.map ~f:fst in
        if score_is_resolved_mate best_score (curr_depth + 1)
        then best_move
        else
          iterative_deepening
            (curr_depth + 1)
            sorted_moves
            tt
            killers
            history_tbl
            (Some best_score)))
    else List.hd_exn moves
  in
  let moves = generate_moves pos in
  if List.is_empty moves then failwith "no legal moves";
  if not (max_depth > 0) then failwith "depth needs to be > 0";
  iterative_deepening 0 moves (TT.mk ()) (Killer.mk ()) (History.mk ()) None
;;

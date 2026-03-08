open Base
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
let lmr_reduction = 1

(* TODO: Tune aspiration window size based on fail rate. *)
let aspiration_window = 100
let initial_alpha = -T.value_mate - 1
let initial_beta = T.value_mate + 1
let generate_moves pos = M.generate_legal pos

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
    let allow_checks = qdepth > qsearch_max_depth - qsearch_check_depth in
    let moves =
      if in_check
      then List.map (M.generate_legal pos) ~f:(fun m -> m, true, true, false)
      else
        (* TODO: Avoid generate+filter; directly generate captures/checks for qsearch. *)
        M.generate_legal pos
        |> List.filter_map ~f:(fun m ->
          let is_capture = P.is_capture pos m in
          let gives_check = allow_checks && P.gives_check pos m in
          if is_capture
          then (
            (* SEE prune losing captures unless they give check. *)
            let is_good_capture = P.see_ge pos m 1 in
            if is_good_capture || gives_check
            then (
              let capture_value =
                match P.piece_on pos (T.move_dst m) with
                | Some p -> T.piece_value p
                | None -> 0
              in
              if stand_pat + capture_value + qsearch_delta_margin <= alpha
              then None
              else Some (m, true, is_good_capture, gives_check))
            else None)
          else if gives_check
          then Some (m, false, false, true)
          else None)
    in
    if List.is_empty moves
    then if in_check then -(T.value_mate - ply) else alpha
    else (
      let sorted_moves =
        List.map moves ~f:(fun (m, is_capture, is_good_capture, gives_check) ->
          let score =
            if is_capture
            then if is_good_capture then 2000000 else -2000000
            else if gives_check
            then 1000000
            else 0
          in
          m, score)
        |> List.sort ~compare:(fun (_, v1) (_, v2) -> compare v2 v1)
        |> List.map ~f:fst
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
  let maybe_attempt_nmp pos =
    let may_do_nmp =
      (* TODO: check for zugzwang, we could do something simple like check if there
       are only kings and pawns *)
      remaining_depth >= null_move_reduction && (not (P.is_in_check pos)) && not is_pv
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
  let offset = if is_white then 1 else -1 in
  let eval_value = offset * Eval.evaluate pos () in
  let is_in_check = P.is_in_check pos in
  let alpha_orig = alpha in
  let do_move (alpha, best_move, is_first_move, idx) move =
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
          && not (P.is_capture pos move || T.is_promotion move)
        in
        let score =
          if can_lmr
          then
            search_with_ply
              move
              alpha
              (alpha + 1)
              lmr_reduction
              ~is_null_window:true
              ~is_pv:false
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
      if T.move_is_ok move && not (P.is_capture pos move || T.is_promotion move)
      then (
        K.add_killer killers ply move;
        History.update history_tbl move remaining_depth (P.moved_piece_exn pos move));
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
    else Continue_or_stop.Continue (alpha, best_move, false, idx + 1)
  in
  let do_move' alpha move ~is_first_move ~idx =
    match do_move (alpha, None, is_first_move, idx) move with
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
    (not is_in_check)
    && remaining_depth = 1
    && may_prune
    && eval_value + T.futility_margin_1 < alpha
  then
    (* If a move proves to be futile, we just return alpha since *)
    (* further continuations are unlikely to raise alpha *)
    alpha
  else if
    (not is_in_check)
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
         let legal_moves = M.generate_legal pos in
         if List.is_empty legal_moves
         then
           (* Either draw or mate *)
           if P.is_in_check pos then -(T.value_mate - curr_ply) else T.value_draw
         else (
           (* Move ordering (hash move would already have been tested before move generation) *)
           (* 1. Good captures - score 2000000 *)
           (* 2. Killer moves - 1500000  *)
           (* 4. Quiet moves - from history *)
           (* 3. Bad captures - -2000000 *)
           let killer_moves = K.get_killers killers ply in
           let sorted_moves =
             List.map legal_moves ~f:(fun m ->
               let is_capture = P.is_capture pos m in
               let score =
                 if is_capture
                 then
                   if P.see_ge pos m 1
                   then
                     (* TODO: maybe some captures are better than others? *)
                     2000000
                   else -2000000 (* Bad captures last *)
                 else if List.find killer_moves ~f:(T.equal_move m) |> Option.is_some
                 then 1500000
                 else
                   (* History score for quiet moves *)
                   History.get history_tbl m (P.moved_piece_exn pos m)
               in
               m, score)
             |> List.sort ~compare:(fun (_, v1) (_, v2) -> compare v2 v1)
             |> List.map ~f:fst
           in
           let score, best_move, is_cut, _ =
             List.fold_until
               sorted_moves
               (* If we didn't a hash move, then we are processing the first move here *)
               ~init:(alpha, None, not found_hash_move, 0)
               ~f:do_move (* No cutoff if we finish *)
               ~finish:(fun (a, b, _, idx) -> a, b, false, idx)
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
  let rec iterative_deepening pool curr_depth moves tt killers history_tbl prev_score =
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
             alpha
             beta
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
      let rest_promises =
        List.map rest_moves ~f:(fun move ->
          Task.async pool (fun _ ->
            let score, move_stats =
              root_search_move move alpha beta ~is_null_window:false ~is_pv:false
            in
            move, score, move_stats))
      in
      let rest_results = List.map rest_promises ~f:(Task.await pool) in
      let move_scores =
        List.fold
          rest_results
          ~init:[ first_move, first_score ]
          ~f:(fun acc (move, score, s) ->
            ignore (merge_stats stats s);
            (move, score) :: acc)
      in
      let sorted_moves =
        List.stable_sort move_scores ~compare:(fun (_, s1) (_, s2) -> compare s2 s1)
      in
      let best_move, best_score = List.hd_exn sorted_moves in
      if Option.is_some prev_score && (best_score <= alpha || best_score >= beta)
      then
        (* Aspiration window failed; re-search with full window. *)
        iterative_deepening pool curr_depth moves tt killers history_tbl None
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
        iterative_deepening
          pool
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
  let pool = Task.setup_pool ~num_domains:parallel () in
  let res =
    Task.run pool (fun _ ->
      iterative_deepening pool 0 moves (TT.mk ()) (Killer.mk ()) (History.mk ()) None)
  in
  Task.teardown_pool pool;
  res
;;

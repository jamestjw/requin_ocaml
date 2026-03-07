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
let initial_alpha = -T.value_mate - 1
let initial_beta = T.value_mate + 1
let generate_moves pos = M.generate_legal pos

let rec pvSearch
          pos
          curr_ply
          max_depth
          alpha
          beta
          is_white
          ply
          history
          ~may_prune
          ~tt
          ~killers
          ~is_null_window
          ~is_pv
          ~history_tbl
  =
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
  let search move alpha beta ~is_null_window ~is_pv =
    -1
    * pvSearch
        (P.do_move' pos move)
        (curr_ply + 1)
        max_depth
        (-beta)
        (-alpha)
        (not is_white)
        (ply + 1)
        (move :: history)
        ~may_prune:(not @@ P.is_capture pos move)
        ~tt
        ~killers
        ~is_null_window
        ~is_pv
        ~history_tbl
  in
  let offset = if is_white then -1 else 1 in
  let eval_value = offset * Eval.evaluate pos () in
  let is_in_check = P.is_in_check pos in
  let do_move (alpha, best_move, is_first_move) move =
    let score =
      if is_first_move
      then search move alpha beta ~is_null_window:false ~is_pv
      else (
        (* Search with null window, i.e. with [alpha , alpha + 1] *)
        (* Since this isn't the first move, by definition it isn't in the PV *)
        let score = search move alpha (alpha + 1) ~is_null_window:true ~is_pv:false in
        if score > alpha && beta - alpha > 1
        then
          (* re-search with full window *)
          search move alpha beta ~is_null_window:false ~is_pv:false
        else score)
    in
    if score >= beta
    then (
      if T.move_is_ok move && not (P.is_capture pos move || T.is_promotion move)
      then (
        K.add_killer killers ply move;
        History.update history_tbl move remaining_depth (P.moved_piece_exn pos move));
      ignore
      @@ TT.store
           tt
           ~key:pos.st.key
           ~m:move
           ~depth:remaining_depth
           ~eval_value
           ~value:score
           ~bound:TT.BOUND_LOWER;
      Continue_or_stop.Stop (beta, best_move, true))
    else if score > alpha
    then Continue_or_stop.Continue (score, Some move, false)
    else Continue_or_stop.Continue (alpha, best_move, false)
  in
  let do_move' alpha move ~is_first_move =
    match do_move (alpha, None, is_first_move) move with
    | Continue_or_stop.Continue (score, m, _) -> score, m, false
    | Continue_or_stop.Stop (score, m, cut) -> score, m, cut
  in
  (* Try to get an early exit score from the TT entry, also evaluates the hash
    move if it exists *)
  let process_tt_entry depth (tt_entry : TT.entry option) alpha =
    match tt_entry with
    | Some tt_entry ->
      let score =
        match tt_entry.depth >= depth, tt_entry.bound with
        | true, TT.BOUND_EXACT ->
          (* TODO: Check if we are at a Pv node before returning this *)
          Some tt_entry.value
        | true, TT.BOUND_LOWER when tt_entry.value >= beta -> Some tt_entry.value
        | true, TT.BOUND_UPPER when tt_entry.value <= alpha -> Some tt_entry.value
        | _, _ -> None
      in
      let alpha =
        match tt_entry.depth >= depth, tt_entry.bound with
        | true, TT.BOUND_LOWER -> Int.max alpha tt_entry.value
        | _ -> alpha
      in
      (match score, tt_entry.bound with
       | Some _, _ -> score, alpha, false
       | _, (TT.BOUND_EXACT | TT.BOUND_LOWER)
         when T.move_not_none tt_entry.move && P.legal pos tt_entry.move ->
         (* Search hash move *)
         let score, _, is_cut = do_move' alpha tt_entry.move ~is_first_move:true in
         if is_cut then Some score, alpha, true else None, score, true
       | _, _ -> score, alpha, false)
    | None -> None, alpha, false
  in
  (* This takes into account the 50 move rule and threehold repetition *)
  if P.is_draw pos ply
  then T.value_draw
  else if (not is_in_check) && (remaining_depth <= 0 || curr_ply = T.max_ply)
  then
    (* TODO: quiescence search *)
    eval_value
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
    let tt_entry = TT.probe tt pos.st.key in
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
           let score, best_move, is_cut =
             List.fold_until
               sorted_moves
               (* If we didn't a hash move, then we are processing the first move here *)
               ~init:(alpha, None, not found_hash_move)
               ~f:do_move (* No cutoff if we finish *)
               ~finish:(fun (a, b, _) -> a, b, false)
           in
           if not is_cut
           then (
             match best_move, is_null_window with
             | Some m, false ->
               ignore
               @@ TT.store
                    tt
                    ~key:pos.st.key
                    ~m
                    ~depth:remaining_depth
                    ~eval_value
                    ~value:score
                    ~bound:TT.BOUND_EXACT
             | None, _ ->
               (* It's fine to set an upper bound even if we are doing a null move search *)
               ignore
               @@ TT.store
                    tt
                    ~key:pos.st.key
                    ~m:T.none_move
                    ~depth:remaining_depth
                    ~eval_value
                    ~value:score
                    ~bound:TT.BOUND_UPPER
             | _ -> ());
           score)))
;;

let get_best_move (pos : P.t) max_depth : T.move =
  let rec iterative_deepening pool curr_depth moves tt killers history_tbl =
    if curr_depth < max_depth
    then
      (History.decay history_tbl;
       let promises =
         List.mapi moves ~f:(fun i move ->
           (* TODO: alpha beta from one move should be used to tighten subsequent searches *)
           Task.async pool (fun _ ->
             ( move
             , -pvSearch
                  (P.do_move' pos move)
                  0
                  curr_depth
                  initial_alpha
                  initial_beta
                  (P.is_white_to_move pos)
                  (P.game_ply pos + 1)
                  [ move ]
                  ~may_prune:(not @@ P.is_capture pos move)
                  ~tt
                  ~killers
                  ~is_null_window:false
                  ~history_tbl
                  ~is_pv:(i = 0) )))
       in
       let move_scores = List.map ~f:(Task.await pool) promises in
       (* Single core search for easier debugging *)
       (* (let move_scores = *)
       (*    List.map moves ~f:(fun move -> *)
       (*      ( move *)
       (*      , -pvSearch *)
       (*           (P.do_move' pos move) *)
       (*           0 *)
       (*           curr_depth *)
       (*           initial_alpha *)
       (*           initial_beta *)
       (*           (P.is_white_to_move pos) *)
       (*           (P.game_ply pos + 1) *)
       (*           [ move ] *)
       (*           ~may_prune:(not @@ P.is_capture pos move) *)
       (*           ~tt *)
       (*           ~is_null_window:false )) *)
       (*  in *)
       (* TODO: add transposition table entry *)
       let sorted_moves =
         List.stable_sort move_scores ~compare:(fun (_, s1) (_, s2) -> compare s2 s1)
       in
       let sorted_moves = sorted_moves |> List.map ~f:fst in
       iterative_deepening pool (curr_depth + 1) sorted_moves)
        tt
        killers
        history_tbl
    else List.hd_exn moves
  in
  let moves = generate_moves pos in
  if List.is_empty moves then failwith "no legal moves";
  if not (max_depth > 0) then failwith "depth needs to be > 0";
  let pool = Task.setup_pool ~num_domains:parallel () in
  let res =
    Task.run pool (fun _ ->
      iterative_deepening pool 0 moves (TT.mk ()) (Killer.mk ()) (History.mk ()))
  in
  Task.teardown_pool pool;
  res
;;

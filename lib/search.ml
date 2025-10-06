open Base
module P = Position.Position
module M = Movegen.MoveGen
module T = Types.Types
module TT = Transposition_table
module Eval = Evaluation
module Task = Domainslib.Task

let parallel = 8
let initial_alpha = -T.value_mate - 1
let initial_beta = T.value_mate + 1
let generate_moves pos = M.generate_legal pos

let rec pvSearch
          pos
          curr_depth
          max_depth
          alpha
          beta
          is_white
          ply
          history
          ~may_prune
          ~tt
          ~is_null_window
  =
  let search move alpha beta ~is_null_window =
    -1
    * pvSearch
        (P.do_move' pos move)
        (curr_depth + 1)
        max_depth
        (-beta)
        (-alpha)
        (not is_white)
        (ply + 1)
        (move :: history)
        ~may_prune:(not @@ P.is_capture pos move)
        ~tt
        ~is_null_window
  in
  let remaining_depth = max_depth - curr_depth in
  let offset = if is_white then -1 else 1 in
  let eval_value = offset * Eval.evaluate pos () in
  let is_in_check = P.is_in_check pos in
  let do_move (alpha, best_move, is_first_move) move =
    let score =
      if is_first_move
      then search move alpha beta ~is_null_window:false
      else (
        (* Search with null window, i.e. with [alpha , alpha + 1] *)
        let score = search move alpha (alpha + 1) ~is_null_window:true in
        if score > alpha && beta - alpha > 1
        then
          (* re-search with full window *)
          search move alpha beta ~is_null_window:false
        else score)
    in
    if score >= beta
    then (
      (* TODO: store killer moves *)
      if not is_null_window
      then
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
  else if (not is_in_check) && (remaining_depth <= 0 || curr_depth = T.max_ply)
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
      let legal_moves = M.generate_legal pos in
      if List.is_empty legal_moves
      then
        (* Either draw or mate *)
        if P.is_in_check pos then -(T.value_mate - curr_depth) else T.value_draw
      else (
        (* Move ordering *)
        (* 1. Good captures *)
        (* 2. Bad captures *)
        (* 3. Non-captures *)
        let sorted_moves =
          List.map legal_moves ~f:(fun m ->
            let score =
              if P.is_capture pos m
              then
                if P.see_ge pos m 1
                then
                  (* TODO: maybe some captures are better than others? *)
                  10
                else 0
              else Int.min_value
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
        score))
;;

let get_best_move (pos : P.t) max_depth ply : T.move =
  let rec iterative_deepening pool curr_depth moves tt =
    if curr_depth < max_depth
    then
      (let promises =
         List.map moves ~f:(fun move ->
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
                  (ply + 1)
                  [ move ]
                  ~may_prune:(not @@ P.is_capture pos move)
                  ~tt
                  ~is_null_window:false )))
       in
       let move_scores = List.map ~f:(Task.await pool) promises in
       (* TODO: add transposition table entry *)
       let sorted_moves =
         List.stable_sort move_scores ~compare:(fun (_, s1) (_, s2) -> compare s2 s1)
       in
       let sorted_moves = sorted_moves |> List.map ~f:fst in
       iterative_deepening pool (curr_depth + 1) sorted_moves)
        tt
    else List.hd_exn moves
  in
  let moves = generate_moves pos in
  if List.is_empty moves then failwith "no legal moves";
  if not (max_depth > 0) then failwith "depth needs to be > 0";
  let pool = Task.setup_pool ~num_domains:parallel () in
  let res = Task.run pool (fun _ -> iterative_deepening pool 0 moves (TT.mk ())) in
  Task.teardown_pool pool;
  res
;;

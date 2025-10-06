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

let rec alpha_beta
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
  let remaining_depth = max_depth - curr_depth in
  let offset = if is_white then -1 else 1 in
  let eval_value = offset * Eval.evaluate pos () in
  let is_in_check = P.is_in_check pos in
  let do_move (alpha, best_move) move =
    let score =
      -1
      * alpha_beta
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
    then Continue_or_stop.Continue (score, Some move)
    else Continue_or_stop.Continue (alpha, best_move)
  in
  let do_move' alpha move =
    match do_move (alpha, None) move with
    | Continue_or_stop.Continue (score, m) -> score, m, false
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
        | true, TT.BOUND_UPPER when tt_entry.value < alpha -> Some tt_entry.value
        | _, _ -> None
      in
      (match score, tt_entry.bound with
       | Some _, _ -> score, alpha
       | _, (TT.BOUND_EXACT | TT.BOUND_LOWER)
         when T.move_not_none tt_entry.move && P.legal pos tt_entry.move ->
         let score, _, is_cut = do_move' alpha tt_entry.move in
         if is_cut then Some score, alpha else None, score
       | _, _ -> score, alpha)
    | None -> None, alpha
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
    | Some score, _ -> score
    | _, alpha ->
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
          (* no cutoff if we finish *)
          List.fold_until
            sorted_moves
            ~init:(alpha, None)
            ~f:do_move
            ~finish:(fun (a, b) -> a, b, false)
        in
        if not is_cut
        then
          if not is_null_window
          then
            ignore
            @@ TT.store
                 tt
                 ~key:pos.st.key
                 ~m:(Stdlib.Option.value best_move ~default:T.none_move)
                 ~depth:remaining_depth
                 ~eval_value
                 ~value:score
                 ~bound:
                   (if Option.is_some best_move then TT.BOUND_EXACT else TT.BOUND_UPPER);
        score))
;;

let get_best_move (pos : P.t) max_depth ply : T.move =
  let rec iterative_deepening pool curr_depth moves tt =
    let handle_move move alpha =
      let new_pos = P.do_move' pos move in
      let null_window_score =
        -alpha_beta
           new_pos
           0
           curr_depth
           (-alpha - 1)
           (-alpha)
           (P.is_white_to_move pos)
           (ply + 1)
           [ move ]
           ~may_prune:(not @@ P.is_capture pos move)
           ~tt
           ~is_null_window:true
      in
      Stdlib.Printf.printf "null score: %d alpha: %d\n" null_window_score alpha;
      if null_window_score > alpha
      then
        (* search with full window *)
        -alpha_beta
           new_pos
           0
           curr_depth
           initial_alpha
           initial_beta
           (P.is_white_to_move pos)
           (ply + 1)
           [ move ]
           ~may_prune:(not @@ P.is_capture pos move)
           ~tt
           ~is_null_window:false
      else null_window_score
    in
    if curr_depth < max_depth
    then
      (match moves with
       | fst_move :: rest ->
         (* Derive alpha from the score of the 'best' move *)
         let fst_move_score =
           -alpha_beta
              (P.do_move' pos fst_move)
              0
              curr_depth
              initial_alpha
              initial_beta
              (P.is_white_to_move pos)
              (ply + 1)
              [ fst_move ]
              ~may_prune:(not @@ P.is_capture pos fst_move)
              ~tt
              ~is_null_window:false
         in
         let promises =
           List.map rest ~f:(fun move ->
             (* TODO: alpha beta from one move should be used to tighten subsequent searches *)
             Task.async pool (fun _ -> move, handle_move move fst_move_score))
         in
         let move_scores = List.map ~f:(Task.await pool) promises in
         (* TODO: add transposition table entry *)
         let sorted_moves =
           List.stable_sort move_scores ~compare:(fun (_, s1) (_, s2) -> compare s2 s1)
         in
         let sorted_moves =
           match sorted_moves with
           | (m, s) :: rest ->
             if fst_move_score >= s
             then (fst_move, fst_move_score) :: (m, s)  :: rest
             else (m, s) :: (fst_move, fst_move_score) :: rest
           | _ -> failwith "impossible"
         in
         let sorted_moves = sorted_moves |> List.map ~f:fst in
         iterative_deepening pool (curr_depth + 1) sorted_moves
       | _ -> failwith "impossible")
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

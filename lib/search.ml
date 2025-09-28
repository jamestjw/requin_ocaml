open Base
module P = Position.Position
module M = Movegen.MoveGen
module T = Types.Types
module Eval = Evaluation

let initial_alpha = -T.value_mate - 1
let initial_beta = T.value_mate + 1

let rec alpha_beta pos curr_depth max_depth alpha beta ply history =
  let do_move (alpha, best_move) move =
    (* Stdlib.Printf.printf "doing move %s\n" (T.show_move move); *)
    let score =
      -1
      * alpha_beta
          (P.do_move' (P.copy pos) move)
          (curr_depth + 1)
          max_depth
          (-beta)
          (-alpha)
          (ply + 1)
          (move :: history)
    in
    if score >= beta
    then
      (* TODO: store killer moves *)
      Continue_or_stop.Stop beta
    else if score > alpha
    then Continue_or_stop.Continue (score, Some move)
    else Continue_or_stop.Continue (alpha, best_move)
  in
  let legal_moves = M.generate_legal pos in
  let remaining_depth = max_depth - curr_depth in
  let offset = if P.is_white_to_move pos then 1 else -1 in
  (* This takes into account the 50 move rule and threehold repetition *)
  if P.is_draw pos ply
  then T.value_draw
  else if List.is_empty legal_moves
  then
    (* Either draw or mate *)
    if P.is_in_check pos then offset * (T.value_mate - curr_depth) else T.value_draw
  else if remaining_depth = 0 || curr_depth = T.max_ply
  then
    (* TODO: quiescence search *)
    Eval.evaluate pos 0
  else (
    (* TODO: check TT *)
    (* TODO: move ordering  *)
    let score = List.fold_until legal_moves ~init:(alpha, None) ~f:do_move ~finish:fst in
    score)
;;

let get_best_move (pos : P.t) max_depth ply : T.move =
  let rec iterative_deepening curr_depth moves alpha beta =
    if curr_depth < max_depth
    then (
      let move_scores =
        List.map moves ~f:(fun move ->
          (* TODO: alpha beta from one move should be used to tighten subsequent searches *)
          ( move
          , -alpha_beta
               (P.do_move' (P.copy pos) move)
               curr_depth
               max_depth
               alpha
               beta
               (ply + 1)
               (move :: []) ))
      in
      (* TODO: add transposition table entry *)
      let sorted_moves =
        List.stable_sort move_scores ~compare:(fun (_, s1) (_, s2) -> compare s2 s1)
        |> List.map ~f:fst
      in
      iterative_deepening (curr_depth + 1) sorted_moves alpha beta)
    else
      (* Stdlib.print_endline @@ String.concat ~sep:"," (List.map ~f:T.show_move moves); *)
      List.hd_exn moves
  in
  let moves = M.generate_legal pos in
  if List.is_empty moves then failwith "no legal moves";
  if not (max_depth > 0) then failwith "depth needs to be > 0";
  iterative_deepening 0 moves initial_alpha initial_beta
;;

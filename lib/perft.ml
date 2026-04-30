open Base
module P = Position.Position
module M = Movegen.MoveGen
module T = Types

(* Counts the number of leaf nodes reachable from [pos] at exactly [depth]
   plies, using legal move generation. This is the standard "perft" metric
   for validating move generation. *)
let rec perft pos depth =
  if depth <= 0
  then 1
  else (
    let moves = M.generate_legal pos in
    if depth = 1
    then List.length moves
    else
      List.fold moves ~init:0 ~f:(fun acc m ->
        let pos' = P.do_move' pos m in
        acc + perft pos' (depth - 1)))
;;

(* Per-root-move node counts. Useful for bisecting move-generation bugs:
   compare against a reference engine (e.g. Stockfish `go perft N`) and the
   first move whose count diverges localizes the bug. *)
let divide pos depth =
  if depth <= 0
  then [], 1
  else (
    let moves = M.generate_legal pos in
    let per_move =
      List.map moves ~f:(fun m ->
        let pos' = P.do_move' pos m in
        let nodes = perft pos' (depth - 1) in
        T.Types.show_move m, nodes)
    in
    let per_move =
      List.sort per_move ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    let total = List.fold per_move ~init:0 ~f:(fun acc (_, n) -> acc + n) in
    per_move, total)
;;

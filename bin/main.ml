open Base
open Requin
open Types
module P = Position.Position
module S = Search

(* TODO: make UCI server *)
let _ =
  let pos = P.from_fen "7k/4K1pp/7N/8/8/8/8/B7 w - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 5 0 in
    Stdlib.Printf.printf "best move is %s\n" (Types.show_move best_move)
;;

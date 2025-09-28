open Base
open Requin
open Types
module P = Position.Position
module S = Search

(* EASY: Scholar's mate *)
(* TODO: make UCI server *)
let _ =
  let pos =
    P.from_fen "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
  in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 2 0 in
    Stdlib.Printf.printf "best move is %s\n" (Types.show_move best_move)
;;

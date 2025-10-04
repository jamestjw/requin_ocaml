open Base
open Requin
open Types
module P = Position.Position
module S = Search

(* TODO: make UCI server *)
let _ =
  let pos = P.from_fen "8/2b2r1P/2P4k/1pK3n1/1N1R1N2/nqp5/8/8 b - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 5 0 in
    Stdlib.Printf.printf "best move is %s\n" (Types.show_move best_move)
;;

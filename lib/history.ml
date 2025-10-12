open Base
module T = Types.Types
module P = Position.Position

type key =
  { piece : T.piece
  ; sq : T.square
  }
[@@deriving ord, sexp, hash]

module Key = struct
  type t = key [@@deriving ord, sexp]

  let hash = hash_key
end

type t = (Key.t, int) Hashtbl.t

let mk () : t = Hashtbl.create (module Key) ~size:1024

(* TODO: add this condition before adding *)
(* (* Only update for quiet moves that actually move a piece *) *)
(* if (not (P.is_capture move)) && not (T.is_none_move move) *)
let update (history_table : t) (move : T.move) (depth : int) (piece : T.piece) =
  let key = { piece; sq = T.move_dst move } in
  let bonus = depth * depth in
  (* Cap to prevent overflow *)
  Hashtbl.update history_table key ~f:(fun v ->
    min (bonus + Option.value ~default:0 v) 1000000)
;;

let get (history_table : t) (move : T.move) (piece : T.piece) : int =
  let key = { piece; sq = T.move_dst move } in
  Hashtbl.find history_table key |> Option.value ~default:0
;;

let decay (history_table : t) =
  Hashtbl.map_inplace history_table ~f:(fun score -> score / 2)
;;

open Base
module T = Types.Types
module P = Position.Position

type key =
  { src : T.square
  ; dst : T.square
  }
[@@deriving ord, sexp, hash]

module Key = struct
  type t = key [@@deriving ord, sexp]

  let hash = hash_key
end

type t = (Key.t, int) Hashtbl.t

let mk () : t = Hashtbl.create (module Key) ~size:1024
let limit = 16000

let apply_bonus score bonus =
  let adjusted = score + bonus - (score * bonus / limit) in
  Int.min limit adjusted
;;

let apply_malus score bonus =
  let adjusted = score - bonus - (score * bonus / limit) in
  Int.max (-limit) adjusted
;;

(* TODO: add this condition before adding *)
(* (* Only update for quiet moves that actually move a piece *) *)
(* if (not (P.is_capture move)) && not (T.is_none_move move) *)
let update (history_table : t) (move : T.move) (depth : int) (_piece : T.piece) =
  let key = { src = T.move_src move; dst = T.move_dst move } in
  let bonus = depth * depth in
  Hashtbl.update history_table key ~f:(fun v ->
    apply_bonus (Option.value ~default:0 v) bonus)
;;

let penalize (history_table : t) (move : T.move) (depth : int) =
  let key = { src = T.move_src move; dst = T.move_dst move } in
  let bonus = depth * depth in
  Hashtbl.update history_table key ~f:(fun v ->
    apply_malus (Option.value ~default:0 v) bonus)
;;

let get (history_table : t) (move : T.move) (_piece : T.piece) : int =
  let key = { src = T.move_src move; dst = T.move_dst move } in
  Hashtbl.find history_table key |> Option.value ~default:0
;;

let decay (history_table : t) =
  Hashtbl.map_inplace history_table ~f:(fun score -> score / 2)
;;

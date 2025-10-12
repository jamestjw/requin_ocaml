open Base
module T = Types.Types

type t = { data : (T.move option * T.move option) array }

let mk () = { data = Array.create ~len:T.max_ply (None, None) }

let get_killers { data } ply =
  if ply < Array.length data
  then (
    match Array.get data ply with
    | Some k1, Some k2 -> [ k2; k1 ]
    | Some k, None -> [ k ]
    | _ -> [])
  else []
;;

let add_killer { data } ply move =
  if ply < Array.length data
  then (
    let k1, k2 = data.(ply) in
    if Option.is_none k2 then data.(ply) <- Some move, k1)
;;

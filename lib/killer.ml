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
    match k1, k2 with
    | Some m, _ when T.equal_move m move -> ()
    | _, Some m when T.equal_move m move -> ()
    | _ -> data.(ply) <- Some move, k1)
;;

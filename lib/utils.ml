open Base

let matrix_get matrix i j = Array.get (Array.get matrix i) j
let matrix_set matrix i j value = Array.set (Array.get matrix i) j value

(*
   * # 1--2;;
 * - : int list = [1; 2]
 * # 1--5;;
 * - : int list = [1; 2; 3; 4; 5]
 * # 5--10;;
 * - : int list = [5; 6; 7; 8; 9; 10]
*)
let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []
;;

let uncurry f (x, y) = f x y
let list_last_exn l = List.rev l |> List.hd_exn

type ('a, 'b) continue_or_stop =
  | Continue of 'a
  | Stop of 'b

let fold_until l ~init ~f ~finish =
  let rec helper l acc =
    match l with
    | [] -> finish acc, []
    | x :: xs ->
      (match f acc x with
       | Continue acc -> helper xs acc
       | Stop res -> res, xs)
  in
  helper l init
;;

module Char = struct
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let char_to_int_exn c =
    (* TODO: Make more robust *)
    Stdlib.Char.code c - Stdlib.Char.code '0'
  ;;

  let is_lower = function
    | 'a' .. 'z' -> true
    | _ -> false
  ;;
end

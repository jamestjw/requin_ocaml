open Base
module P = Position.Position
module T = Types.Types
module S = Search

type t =
  { pos : P.t
  ; max_depth : int
  }

let send_response msg =
  Stdlib.print_endline msg;
  Stdlib.flush Stdlib.stdout
;;

let handle_uci () =
  send_response "id name requin";
  send_response "id author James Tan";
  send_response "uciok"
;;

let handle_isready () = send_response "readyok"
let handle_ucinewgame state = { state with pos = P.mk () }

let handle_position state args =
  let parse_move pos m =
    let piece_type_of_char = function
      | 'q' -> Ok T.QUEEN
      | 'r' -> Ok T.ROOK
      | 'b' -> Ok T.BISHOP
      | 'n' -> Ok T.KNIGHT
      | _ -> Error "Invalid piece promotion piece type"
    in
    let len = String.length m in
    if len < 4 || len > 5
    then Error "Invalid move string length"
    else
      let open Result.Let_syntax in
      let from_sq = String.sub m ~pos:0 ~len:2 in
      let to_sq = String.sub m ~pos:2 ~len:2 in
      let%bind from_sq = T.parse_algrebraic_sq from_sq in
      let%bind to_sq = T.parse_algrebraic_sq to_sq in
      let%bind ppt =
        if len = 5
        then (
          let%bind ppt = piece_type_of_char m.[4] in
          Ok (Some ppt))
        else Ok None
      in
      let%bind move_type =
        match P.piece_on pos from_sq, P.piece_on pos to_sq with
        | None, _ -> Error "missing piece on source square"
        | (Some W_PAWN | Some B_PAWN), None
          when not @@ T.equal_file (T.file_of_sq from_sq) (T.file_of_sq to_sq) ->
          Ok T.EN_PASSANT
        | (Some W_KING | Some B_KING), None when T.distance_by_file from_sq to_sq > 1 ->
          Ok T.CASTLING
        | (Some W_PAWN | Some B_PAWN), _ when Option.is_some ppt -> Ok T.PROMOTION
        | _ -> Ok T.NORMAL
      in
      return @@ T.mk_move ~move_type ~ppt to_sq from_sq
  in
  let parse_and_make_move pos move_str =
    match parse_move pos move_str with
    | Ok move -> P.do_move' pos move
    | Error e -> failwith e
  in
  let parse_and_make_moves (pos : P.t) move_strs =
    List.fold ~init:pos ~f:parse_and_make_move move_strs
  in
  let pos =
    match args with
    | "startpos" :: "moves" :: moves -> parse_and_make_moves P.from_start_pos moves
    | "fen" :: fen :: "moves" :: moves ->
      parse_and_make_moves (P.from_fen fen |> Stdlib.Result.get_ok) moves
    | _ ->
      Printf.failwithf
        "invalid arguments to `position`: %s"
        (String.concat ~sep:" " args)
        ()
  in
  { state with pos }
;;

let handle_go { pos; max_depth } _args =
  (* TODO: handle the arguments *)
  let best_move = S.get_best_move pos max_depth in
  send_response (Printf.sprintf "bestmove %s" (T.show_move best_move))
;;

open Base
module P = Position.Position
module T = Types.Types
module S = Search

type t =
  { pos : P.t
  ; max_depth : int
  }

type go_config = { max_depth : int }

let send_response msg =
  Stdlib.print_endline msg;
  Stdlib.flush Stdlib.stdout
;;

let handle_uci () =
  send_response "id name requin";
  send_response "id author James Tan";
  send_response "option name Move Overhead type spin default 30 min 0 max 1000";
  send_response "option name Threads type spin default 1 min 1 max 8";
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

let handle_setoption state args =
  let rec parse_option current_name current_value = function
    | [] ->
      (match current_name with
       | Some "Move Overhead" ->
         (* You could store current_value in state if you wanted,
           e.g., state.move_overhead = Some (int_of_string current_value) *)
         ()
       | Some "Threads" -> ()
       | Some _ ->
         (* Other options, ignore for now *)
         ()
       | None -> ())
    | "name" :: name_parts ->
      (* Found "name", so gather all parts until "value" *)
      let rec get_name_and_value_parts name_acc = function
        | [] -> name_acc, [] (* No value found, error or end *)
        | "value" :: value_parts -> name_acc, value_parts
        | part :: rest -> get_name_and_value_parts (name_acc ^ " " ^ part) rest
      in
      let name_str, value_parts =
        get_name_and_value_parts (List.hd_exn name_parts) (List.tl_exn name_parts)
      in
      (match value_parts with
       | v :: rest -> parse_option (Some name_str) (Some v) rest
       | [] -> parse_option (Some name_str) None [])
    | _ :: rest ->
      parse_option
        current_name
        current_value
        rest (* Should not happen with well-formed UCI *)
  in
  parse_option None None args;
  state
;;

let handle_go { pos; max_depth } args =
  let rec parse_args config args =
    match args with
    (* TODO: handle other arguments *)
    | "depth" :: depth :: rest ->
      parse_args { max_depth = Stdlib.int_of_string depth } rest
    | _ :: rest -> parse_args config rest
    | _ -> config
  in
  let go_config = parse_args { max_depth } args in
  let best_move = S.get_best_move pos go_config.max_depth in
  send_response (Printf.sprintf "bestmove %s" (T.show_move best_move))
;;

let handle_line state line =
  let parts = String.split line ~on:' ' in
  match parts with
  | "uci" :: _ ->
    handle_uci ();
    state
  | "isready" :: _ ->
    handle_isready ();
    state
  | "ucinewgame" :: _ -> handle_ucinewgame state
  | "position" :: args -> handle_position state args
  | "go" :: args ->
    handle_go state args;
    state
  | "setoption" :: args -> handle_setoption state args
  | "quit" :: _ -> Stdlib.exit 0
  | _ ->
    (* Silently ignore unknown commands as per UCI spec *)
    state
;;

let run () =
  let initial_state = { pos = P.from_start_pos; max_depth = 5 } in
  let rec loop state =
    match Stdlib.read_line () with
    | line ->
      let new_state = handle_line state line in
      loop new_state
    | exception End_of_file -> Stdlib.exit 0
  in
  loop initial_state
;;

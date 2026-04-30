open Base
open Requin
module P = Position.Position

let startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

(* Standard perft test suite from the Chess Programming Wiki.
   Node counts are well-known and any deviation indicates a move-gen bug. *)
let suite =
  [ ( "startpos"
    , startpos
    , [ 1, 20; 2, 400; 3, 8902; 4, 197281; 5, 4865609; 6, 119060324 ] )
  ; ( "kiwipete"
    , "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    , [ 1, 48; 2, 2039; 3, 97862; 4, 4085603; 5, 193690690 ] )
  ; ( "position3"
    , "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
    , [ 1, 14; 2, 191; 3, 2812; 4, 43238; 5, 674624; 6, 11030083 ] )
  ; ( "position4"
    , "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
    , [ 1, 6; 2, 264; 3, 9467; 4, 422333; 5, 15833292 ] )
  ; ( "position4_mirror"
    , "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1"
    , [ 1, 6; 2, 264; 3, 9467; 4, 422333; 5, 15833292 ] )
  ; ( "position5"
    , "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
    , [ 1, 44; 2, 1486; 3, 62379; 4, 2103487; 5, 89941194 ] )
  ; ( "position6"
    , "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
    , [ 1, 46; 2, 2079; 3, 89890; 4, 3894594; 5, 164075551 ] )
  ]
;;

let parse_pos fen =
  match P.from_fen fen with
  | Ok pos -> pos
  | Error msg -> Printf.failwithf "invalid FEN %S: %s" fen msg ()
;;

let run_one fen depth =
  let pos = parse_pos fen in
  let start = Stdlib.Sys.time () in
  let nodes = Perft.perft pos depth in
  let elapsed = Float.max 0.000001 (Stdlib.Sys.time () -. start) in
  let nps = Float.of_int nodes /. elapsed |> Int.of_float in
  Stdlib.Printf.printf
    "depth=%d nodes=%d time=%.3fs nps=%d\n"
    depth
    nodes
    elapsed
    nps;
  nodes
;;

let run_divide fen depth =
  let pos = parse_pos fen in
  let start = Stdlib.Sys.time () in
  let per_move, total = Perft.divide pos depth in
  let elapsed = Float.max 0.000001 (Stdlib.Sys.time () -. start) in
  List.iter per_move ~f:(fun (m, n) -> Stdlib.Printf.printf "%s: %d\n" m n);
  Stdlib.Printf.printf "\ntotal: %d  time=%.3fs\n" total elapsed
;;

let run_suite max_depth =
  let failures = ref 0 in
  let total_nodes = ref 0 in
  let total_time = ref 0. in
  List.iter suite ~f:(fun (name, fen, expected) ->
    let pos = parse_pos fen in
    List.iter expected ~f:(fun (depth, want) ->
      if depth <= max_depth
      then (
        let start = Stdlib.Sys.time () in
        let got = Perft.perft pos depth in
        let elapsed = Stdlib.Sys.time () -. start in
        total_nodes := !total_nodes + got;
        total_time := !total_time +. elapsed;
        if got = want
        then
          Stdlib.Printf.printf
            "OK    %-18s d=%d nodes=%d time=%.3fs\n"
            name
            depth
            got
            elapsed
        else (
          Int.incr failures;
          Stdlib.Printf.printf
            "FAIL  %-18s d=%d got=%d want=%d time=%.3fs\n"
            name
            depth
            got
            want
            elapsed))));
  Stdlib.Printf.printf
    "\ntotal nodes=%d time=%.3fs failures=%d\n"
    !total_nodes
    !total_time
    !failures;
  if !failures > 0 then Stdlib.exit 1
;;

let usage () =
  Stdlib.print_endline
    "Usage:\n\
    \  perft                          - run default suite up to depth 4\n\
    \  perft suite [MAX_DEPTH]        - run suite up to MAX_DEPTH (default 4)\n\
    \  perft DEPTH [FEN]              - perft DEPTH on FEN (default startpos)\n\
    \  perft divide DEPTH [FEN]       - per-move counts at DEPTH";
  Stdlib.exit 2
;;

let () =
  match Array.to_list (Stdlib.Sys.argv) |> List.tl_exn with
  | [] -> run_suite 4
  | [ "suite" ] -> run_suite 4
  | [ "suite"; d ] -> run_suite (Int.of_string d)
  | [ "divide"; d ] -> run_divide startpos (Int.of_string d)
  | [ "divide"; d; fen ] -> run_divide fen (Int.of_string d)
  | [ d ] ->
    let _ = run_one startpos (Int.of_string d) in
    ()
  | [ d; fen ] ->
    let _ = run_one fen (Int.of_string d) in
    ()
  | _ -> usage ()
;;

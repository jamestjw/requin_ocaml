open Base
open Requin
module P = Position.Position
module S = Search

type bench_case =
  { name : string
  ; fen : string
  ; depth : int
  }

let bench_cases =
  [ { name = "startpos_d5"
    ; fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    ; depth = 5
    }
  ; { name = "mate_in_3_black"
    ; fen = "5rk1/pp1Q3p/6p1/3pr3/2p5/2P3P1/PP2q1PK/R4N2 b - - 5 31"
    ; depth = 7
    }
  ; { name = "white_mate_in_2"
    ; fen = "8/2p2p2/1nR4p/4k2K/1N2p2N/3p1n2/3Q2b1/b4RB1 w - - 0 1"
    ; depth = 5
    }
  ; { name = "black_mate_in_2"
    ; fen = "3r2k1/2P2p1p/4p1p1/8/3b1Q2/8/3p1RPP/3q1BK1 b - - 0 1"
    ; depth = 5
    }
  ; { name = "italian_center"
    ; fen = "r1bq1rk1/ppp2ppp/2n2n2/3pp3/2BPP3/2P2N2/PP3PPP/RNBQ1RK1 w - - 0 8"
    ; depth = 5
    }
  ; { name = "queens_gambit"
    ; fen = "r2q1rk1/pp2bppp/2np1n2/2p1p3/2P1P3/2NP1N1P/PP2BPP1/R1BQ1RK1 w - - 0 10"
    ; depth = 5
    }
  ; { name = "rook_endgame"
    ; fen = "8/5pk1/3r2p1/1p1Pp2p/pP2P2P/P3K1P1/5P2/3R4 w - - 0 40"
    ; depth = 6
    }
  ; { name = "minor_piece_endgame"
    ; fen = "8/2k5/2p2pp1/3p4/3P1P2/2P1P1P1/2K5/8 w - - 0 50"
    ; depth = 6
    }
  ]
;;

let run_case { name; fen; depth } =
  let pos = P.from_fen fen |> Stdlib.Result.get_ok in
  let last_info : S.info option ref = ref None in
  let instrumentation =
    { S.on_info = (fun info -> last_info := Some info)
    ; on_pv = (fun (_ : S.pv_info) -> ())
    }
  in
  let start = Stdlib.Sys.time () in
  let best_move = S.get_best_move ~instrumentation pos depth in
  let elapsed = Float.max 0.001 (Stdlib.Sys.time () -. start) in
  match !last_info with
  | Some info ->
    Stdlib.Printf.printf
      "%s depth=%d best=%s score=%d nodes=%d nps=%d lmr=%d lmr_re=%d time=%.3f\n"
      name
      depth
      (Types.Types.show_move best_move)
      info.score
      info.nodes
      info.nps
      info.lmr
      info.lmr_re
      elapsed;
    info.nodes, elapsed
  | None -> failwith "benchmark run produced no search info"
;;

let () =
  let total_nodes, total_time =
    List.fold bench_cases ~init:(0, 0.) ~f:(fun (nodes_acc, time_acc) case ->
      let nodes, elapsed = run_case case in
      nodes_acc + nodes, time_acc +. elapsed)
  in
  Stdlib.Printf.printf "total nodes=%d total_time=%.3f\n" total_nodes total_time
;;

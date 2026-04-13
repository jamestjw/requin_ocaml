open Base
open Requin
module P = Position.Position
module S = Search

type bench_case =
  { name : string
  ; fen : string
  ; depth : int
  }

let format_counts labels counts =
  Array.to_list counts
  |> List.mapi ~f:(fun idx count -> idx, count)
  |> List.filter ~f:(fun (_, count) -> count > 0)
  |> List.map ~f:(fun (idx, count) -> Printf.sprintf "%s=%d" (labels idx) count)
  |> String.concat ~sep:","
;;

let format_depth_counts counts =
  Array.to_list counts
  |> List.mapi ~f:(fun depth count -> depth, count)
  |> List.filter ~f:(fun (depth, count) -> depth > 0 && count > 0)
  |> List.map ~f:(fun (depth, count) -> Printf.sprintf "d%d=%d" depth count)
  |> String.concat ~sep:","
;;

let copy_counts counts = Array.init (Array.length counts) ~f:(fun idx -> counts.(idx))

let add_counts_into acc counts =
  Array.iteri counts ~f:(fun idx count -> acc.(idx) <- acc.(idx) + count)
;;

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
  ; { name = "kiwipete"
    ; fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    ; depth = 5
    }
  ; { name = "in_check_evasion"; fen = "4k3/8/8/8/8/8/4r3/4K3 w - - 0 1"; depth = 5 }
  ; { name = "promotion_race"; fen = "8/P7/8/8/8/8/7p/4K2k w - - 0 1"; depth = 6 }
  ; { name = "closed_center"
    ; fen = "r1bq1rk1/pp2bppp/2n1pn2/2pp4/3PPN2/2P1B3/PP1N1PPP/R2QKB1R w KQ - 0 8"
    ; depth = 5
    }
  ; { name = "queenless_rooks"
    ; fen = "8/2r2pk1/3p1np1/1p1Pp2p/1P2P2P/2R3P1/5PK1/8 w - - 0 35"
    ; depth = 6
    }
  ; { name = "opposite_castling"
    ; fen = "r3k2r/pppq1ppp/2npbn2/4p3/2B1P3/2NP1N2/PPP2PPP/R1BQ1RK1 w kq - 2 9"
    ; depth = 5
    }
  ; { name = "sicilian_pressure"
    ; fen = "r2q1rk1/pp2bppp/2npbn2/2p1p3/2P1P3/2NPBN2/PPQ2PPP/R4RK1 w - - 0 11"
    ; depth = 5
    }
  ; { name = "kingside_attack"
    ; fen = "r1bq1rk1/pp1n1ppp/2pbpn2/3p4/3P4/2NBPN2/PPQ2PPP/R1B2RK1 w - - 0 9"
    ; depth = 5
    }
  ; { name = "iqp_middlegame"
    ; fen = "r2q1rk1/pp2bppp/2n2n2/2pp4/3P4/2P1PN2/PP1N1PPP/R1BQ1RK1 w - - 0 10"
    ; depth = 5
    }
  ; { name = "heavy_piece_tactics"
    ; fen = "2r2rk1/pp1n1ppp/2p1pn2/q2p4/3P4/2NBPN2/PPQ2PPP/2RR2K1 w - - 0 14"
    ; depth = 5
    }
  ; { name = "carlsbad_structure"
    ; fen = "2r2rk1/pp1b1pp1/2n1pn1p/q1bp4/3P4/2NBPN2/PPQ2PPP/2RR2K1 w - - 0 12"
    ; depth = 5
    }
  ; { name = "open_center_tension"
    ; fen = "r2q1rk1/pp2bppp/2n1bn2/2pp4/3P4/2PBPN2/PPQN1PPP/R1B2RK1 w - - 0 9"
    ; depth = 5
    }
  ; { name = "pawn_race_endgame"
    ; fen = "8/2k5/1p3pp1/pPp1p3/P1P1P3/2K3P1/5P2/8 w - - 0 45"
    ; depth = 6
    }
  ; { name = "rook_activity_endgame"
    ; fen = "8/5pk1/1p4p1/1P1r3p/p2P1P1P/P3R1P1/5K2/8 w - - 0 45"
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
    let fh_stage =
      format_counts
        (fun idx ->
           S.move_stage_name
           @@
           match idx with
           | 0 -> S.Stage_hash
           | 1 -> S.Stage_evasions
           | 2 -> S.Stage_good_captures
           | 3 -> S.Stage_killers
           | 4 -> S.Stage_quiets
           | 5 -> S.Stage_bad_captures
           | _ -> invalid_arg "unknown stage index")
        info.fh_stage
    in
    let fh_index = format_counts S.fail_high_index_bucket_name info.fh_index in
    let fh_depth = format_depth_counts info.fh_depth in
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
    Stdlib.Printf.printf "  fh_stage: %s\n" fh_stage;
    Stdlib.Printf.printf "  fh_index: %s\n" fh_index;
    Stdlib.Printf.printf "  fh_depth: %s\n" fh_depth;
    info, elapsed
  | None -> failwith "benchmark run produced no search info"
;;

let () =
  let total_nodes, total_time, total_fh_stage, total_fh_index, total_fh_depth =
    List.fold bench_cases ~init:(0, 0., [||], [||], [||]) ~f:(fun acc case ->
      let nodes_acc, time_acc, total_fh_stage, total_fh_index, total_fh_depth = acc in
      let info, elapsed = run_case case in
      let total_fh_stage =
        if Array.length total_fh_stage = 0
        then copy_counts info.fh_stage
        else (
          add_counts_into total_fh_stage info.fh_stage;
          total_fh_stage)
      in
      let total_fh_index =
        if Array.length total_fh_index = 0
        then copy_counts info.fh_index
        else (
          add_counts_into total_fh_index info.fh_index;
          total_fh_index)
      in
      let total_fh_depth =
        if Array.length total_fh_depth = 0
        then copy_counts info.fh_depth
        else (
          add_counts_into total_fh_depth info.fh_depth;
          total_fh_depth)
      in
      ( nodes_acc + info.nodes
      , time_acc +. elapsed
      , total_fh_stage
      , total_fh_index
      , total_fh_depth ))
  in
  Stdlib.Printf.printf "total nodes=%d total_time=%.3f\n" total_nodes total_time;
  Stdlib.Printf.printf
    "total fh_stage: %s\n"
    (format_counts
       (fun idx ->
          S.move_stage_name
          @@
          match idx with
          | 0 -> S.Stage_hash
          | 1 -> S.Stage_evasions
          | 2 -> S.Stage_good_captures
          | 3 -> S.Stage_killers
          | 4 -> S.Stage_quiets
          | 5 -> S.Stage_bad_captures
          | _ -> invalid_arg "unknown stage index")
       total_fh_stage);
  Stdlib.Printf.printf
    "total fh_index: %s\n"
    (format_counts S.fail_high_index_bucket_name total_fh_index);
  Stdlib.Printf.printf "total fh_depth: %s\n" (format_depth_counts total_fh_depth)
;;

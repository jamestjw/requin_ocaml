(lang dune 3.15)

(name requin_ocaml)

(generate_opam_files true)

(source
 (github jamestjw/requin_ocaml))

(maintainers "James Tan")

(license GPL-3.0)

(package
 (name requin_ocaml)
 (synopsis "A rewrite of Requin in OCaml")
 (description "Requin is a chess engine I originally made in Rust.")
 (depends
   ocaml
   dune
   integers
   ppx_jane
   ppx_deriving
   ppx_let
   domainslib)
 (tags
  (chess-engine uci chess requin)))

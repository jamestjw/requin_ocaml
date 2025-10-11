FROM --platform=$BUILDPLATFORM ocaml/opam:debian-11-ocaml-4.14 AS ocaml-builder

RUN opam install -y dune && opam clean -a

WORKDIR /work

COPY . .

RUN opam exec -- dune build @install --profile=release

FROM --platform=$BUILDPLATFORM lichessbotdevs/lichess-bot:latest

# Use the same directory as the base lichess-bot image
WORKDIR /lichess-bot

# Copy the compiled OCaml executable from the ocaml-builder stage
# The path below assumes dune builds to _build/default/bin/main.exe
COPY --from=ocaml-builder /work/_build/default/bin/main.exe /app/engines/requin

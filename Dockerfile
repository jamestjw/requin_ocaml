FROM ocaml/opam:debian-11-ocaml-5.1 AS ocaml-builder

RUN opam install -y dune && opam clean -a

WORKDIR /work

COPY dune-project ./
COPY requin_ocaml.opam ./

RUN opam install . --deps-only

COPY . .

RUN opam exec -- dune build @install --profile=release

FROM python:3.10-slim-bullseye

RUN apt-get update && apt-get install -y --no-install-recommends git

WORKDIR /app

ENV LICHESS_BOT_DOCKER="true"
ENV PYTHONDONTWRITEBYTECODE=1

RUN mkdir -p /app/engines
COPY --from=ocaml-builder /work/_build/default/bin/main.exe /app/engines/requin

RUN git clone https://github.com/jamestjw/lichess-bot.git

WORKDIR /app/lichess-bot

RUN git checkout master

RUN python3 -m pip install --no-cache-dir -r requirements.txt

CMD python3 lichess-bot.py --disable_auto_logging --config /lichess-bot/config/config.yml -u

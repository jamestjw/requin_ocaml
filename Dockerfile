FROM ocaml/opam:debian-11-ocaml-5.1 AS ocaml-builder

WORKDIR /work

COPY dune-project requin_ocaml.opam ./

RUN opam update && opam install -y dune . --deps-only

COPY . .

RUN opam exec -- dune build @install --profile=release

FROM python:3.10-slim-bullseye

RUN apt-get update \
 && apt-get install -y --no-install-recommends git \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

ENV LICHESS_BOT_DOCKER="true"
ENV PYTHONDONTWRITEBYTECODE=1

RUN mkdir -p /app/engines
COPY --from=ocaml-builder /work/_build/default/bin/main.exe /app/engines/requin

ARG LICHESS_BOT_REF=master
RUN git clone --depth 1 --branch "$LICHESS_BOT_REF" https://github.com/jamestjw/lichess-bot.git /app/lichess-bot

WORKDIR /app/lichess-bot

RUN python3 -m pip install --no-cache-dir -r requirements.txt

CMD ["python3", "lichess-bot.py", "--disable_auto_logging", "--config", "/lichess-bot/config/config.yml", "-u"]

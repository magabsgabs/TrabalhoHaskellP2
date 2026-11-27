# ============================================================
# STAGE 1 — builder (tem GHC + Stack, cerca de 3GB)
# ============================================================
FROM haskell:9.2.8-slim AS builder

WORKDIR /app

# bibliotecas de sistema que o Yesod/Persistent precisa pra linkar
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev zlib1g-dev libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

# 1) primeiro copia so as declaracoes de deps pra aproveitar cache do docker
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack setup --install-ghc
RUN stack build --dependencies-only --no-nix

# 2) agora copia o resto do projeto e builda o binario
COPY . .
RUN stack build --copy-bins --local-bin-path /app/bin --no-nix


# ============================================================
# STAGE 2 — runtime (bem menor, so o que precisa pra rodar)
# ============================================================
FROM debian:bullseye-slim

# libs em tempo de execucao
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates libgmp10 libpq5 zlib1g netbase \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# binario e assets do projeto
COPY --from=builder /app/bin/ecommerce-yesod /app/
COPY --from=builder /app/config /app/config
COPY --from=builder /app/static /app/static

# config via env vars (Fly injeta PORT, mas o Yesod le YESOD_PORT)
ENV YESOD_HOST=*4
ENV YESOD_PORT=8080
# aponta o SQLite pro volume persistente (vai ser montado em /data pelo fly.toml)
ENV YESOD_SQLITE_DATABASE=/data/ecommerce-yesod.sqlite3

EXPOSE 8080

CMD ["/app/ecommerce-yesod"]

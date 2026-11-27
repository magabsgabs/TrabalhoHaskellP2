# ============================================================
# STAGE 1 — builder (tem GHC + Stack, cerca de 3GB)
# ============================================================
FROM haskell:9.2.8-slim AS builder

WORKDIR /app

# bibliotecas de sistema que o Yesod/Persistent precisa pra linkar
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev zlib1g-dev libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

# 1) copia so as declaracoes de deps pra aproveitar cache
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack setup --install-ghc
RUN stack build --dependencies-only --no-nix

# 2) copia o resto e builda o binario
COPY . .
RUN stack build --copy-bins --local-bin-path /app/bin --no-nix


# ============================================================
# STAGE 2 — runtime (bem menor, so o que precisa pra rodar)
# ============================================================
FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates libgmp10 libpq5 zlib1g netbase \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/bin/ecommerce-yesod /app/
COPY --from=builder /app/config /app/config
COPY --from=builder /app/static /app/static

ENV YESOD_HOST=*4
# Render passa a porta em $PORT; Fly passa em $YESOD_PORT.
# Fallback 8080 pra rodar local via `docker run`.
ENV YESOD_SQLITE_DATABASE=/data/ecommerce-yesod.sqlite3

EXPOSE 8080

# Entrypoint em shell: se $PORT vier (Render), usa ela; senao usa YESOD_PORT, senao 8080.
# Tambem cria /data se nao existir (fallback caso nao tenha volume).
CMD ["sh", "-c", "mkdir -p /data && export YESOD_PORT=${PORT:-${YESOD_PORT:-8080}} && exec /app/ecommerce-yesod"]

FROM elixir:1.15-alpine AS builder
RUN apk add --no-cache build-base git
ENV MIX_ENV=prod
WORKDIR /app
RUN mix local.hex --force && mix local.rebar --force
COPY mix.exs mix.lock ./
RUN mix deps.get --only prod
COPY lib ./lib
RUN mix compile && mix release

FROM alpine:3.18 AS runtime
RUN apk add --no-cache libstdc++ openssl ncurses-libs
WORKDIR /app
COPY --from=builder /app/_build/prod/rel/inventory ./
ENV HOME=/app PORT=4002
EXPOSE 4002
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:4002/health || exit 1
CMD ["bin/inventory", "start"]


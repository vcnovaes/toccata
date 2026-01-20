defmodule Payment.Router do
  @moduledoc """
  HTTP API router for the Payment service.

  Endpoints:
  - POST /try - Reserve funds
  - POST /confirm - Capture reserved funds
  - POST /cancel - Release reserved funds
  - GET /health - Health check
  - GET /metrics - Prometheus metrics
  - POST /accounts - Create account
  - GET /accounts/:id - Get account balance
  - POST /reset - Reset service state
  """

  use Plug.Router
  require Logger

  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  # Health check
  get "/health" do
    send_json(conn, 200, %{status: "ok", service: "payment"})
  end

  # Prometheus metrics
  get "/metrics" do
    # TODO: Integrate with prometheus_ex
    send_json(conn, 200, %{metrics: []})
  end

  # Create account
  post "/accounts" do
    %{"account_id" => account_id, "balance" => balance} = conn.body_params
    :ok = Payment.create_account(account_id, balance)
    send_json(conn, 201, %{status: "created", account_id: account_id, balance: balance})
  end

  # Get account balance
  get "/accounts/:id" do
    balance = Payment.get_balance(id)
    send_json(conn, 200, %{account_id: id, balance: balance})
  end

  # Try phase - Reserve funds
  post "/try" do
    start_time = System.monotonic_time(:millisecond)
    %{"account_id" => account_id, "amount" => amount} = conn.body_params
    opts = parse_opts(conn.body_params)

    result = Payment.try_reserve(account_id, amount, opts)
    duration = System.monotonic_time(:millisecond) - start_time

    case result do
      {:ok, reservation_id} ->
        Logger.info("Try succeeded for account #{account_id}, reservation: #{reservation_id}")

        send_json(conn, 200, %{
          status: "ok",
          reservation_id: reservation_id,
          duration_ms: duration
        })

      {:error, reason} ->
        Logger.warn("Try failed for account #{account_id}: #{inspect(reason)}")

        send_json(conn, 400, %{
          status: "error",
          reason: to_string(reason),
          duration_ms: duration
        })
    end
  end

  # Confirm phase - Capture funds
  post "/confirm" do
    start_time = System.monotonic_time(:millisecond)
    %{"reservation_id" => reservation_id} = conn.body_params
    opts = parse_opts(conn.body_params)

    result = Payment.confirm(reservation_id, opts)
    duration = System.monotonic_time(:millisecond) - start_time

    case result do
      :ok ->
        Logger.info("Confirm succeeded for reservation #{reservation_id}")
        send_json(conn, 200, %{status: "ok", duration_ms: duration})

      {:error, reason} ->
        Logger.warn("Confirm failed for reservation #{reservation_id}: #{inspect(reason)}")
        send_json(conn, 400, %{status: "error", reason: to_string(reason), duration_ms: duration})
    end
  end

  # Cancel phase - Release funds
  post "/cancel" do
    start_time = System.monotonic_time(:millisecond)
    %{"reservation_id" => reservation_id} = conn.body_params
    opts = parse_opts(conn.body_params)

    result = Payment.cancel(reservation_id, opts)
    duration = System.monotonic_time(:millisecond) - start_time

    case result do
      :ok ->
        Logger.info("Cancel succeeded for reservation #{reservation_id}")
        send_json(conn, 200, %{status: "ok", duration_ms: duration})

      {:error, reason} ->
        Logger.warn("Cancel failed for reservation #{reservation_id}: #{inspect(reason)}")
        send_json(conn, 400, %{status: "error", reason: to_string(reason), duration_ms: duration})
    end
  end

  # Reset state (for testing)
  post "/reset" do
    Payment.reset()
    send_json(conn, 200, %{status: "ok"})
  end

  # Catch-all
  match _ do
    send_json(conn, 404, %{error: "not_found"})
  end

  # Helpers

  defp send_json(conn, status, body) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(body))
  end

  defp parse_opts(params) do
    opts = []

    opts =
      case Map.get(params, "latency") do
        nil -> opts
        latency when is_integer(latency) -> Keyword.put(opts, :latency, latency)
        _ -> opts
      end

    opts =
      case Map.get(params, "failure_rate") do
        nil -> opts
        rate when is_float(rate) -> Keyword.put(opts, :failure_rate, rate)
        _ -> opts
      end

    opts
  end
end

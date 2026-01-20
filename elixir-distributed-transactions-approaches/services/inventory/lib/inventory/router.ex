defmodule Inventory.Router do
  use Plug.Router
  require Logger

  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  get "/health" do
    send_json(conn, 200, %{status: "ok", service: "inventory"})
  end

  get "/metrics" do
    send_json(conn, 200, %{metrics: []})
  end

  post "/products" do
    %{"product_id" => product_id, "quantity" => quantity} = conn.body_params
    :ok = Inventory.add_product(product_id, quantity)
    send_json(conn, 201, %{status: "created", product_id: product_id, quantity: quantity})
  end

  get "/products/:id" do
    stock = Inventory.get_stock(id)
    send_json(conn, 200, %{product_id: id, stock: stock})
  end

  post "/try" do
    start_time = System.monotonic_time(:millisecond)
    %{"product_id" => product_id, "quantity" => quantity} = conn.body_params
    opts = parse_opts(conn.body_params)

    result = Inventory.try_reserve(product_id, quantity, opts)
    duration = System.monotonic_time(:millisecond) - start_time

    case result do
      {:ok, reservation_id} ->
        send_json(conn, 200, %{status: "ok", reservation_id: reservation_id, duration_ms: duration})

      {:error, reason} ->
        send_json(conn, 400, %{status: "error", reason: to_string(reason), duration_ms: duration})
    end
  end

  post "/confirm" do
    start_time = System.monotonic_time(:millisecond)
    %{"reservation_id" => reservation_id} = conn.body_params
    opts = parse_opts(conn.body_params)

    result = Inventory.confirm(reservation_id, opts)
    duration = System.monotonic_time(:millisecond) - start_time

    case result do
      :ok -> send_json(conn, 200, %{status: "ok", duration_ms: duration})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason), duration_ms: duration})
    end
  end

  post "/cancel" do
    start_time = System.monotonic_time(:millisecond)
    %{"reservation_id" => reservation_id} = conn.body_params
    opts = parse_opts(conn.body_params)

    result = Inventory.cancel(reservation_id, opts)
    duration = System.monotonic_time(:millisecond) - start_time

    case result do
      :ok -> send_json(conn, 200, %{status: "ok", duration_ms: duration})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason), duration_ms: duration})
    end
  end

  post "/reset" do
    Inventory.reset()
    send_json(conn, 200, %{status: "ok"})
  end

  match _ do
    send_json(conn, 404, %{error: "not_found"})
  end

  defp send_json(conn, status, body) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(body))
  end

  defp parse_opts(params) do
    opts = []
    opts = if params["latency"], do: Keyword.put(opts, :latency, params["latency"]), else: opts
    opts = if params["failure_rate"], do: Keyword.put(opts, :failure_rate, params["failure_rate"]), else: opts
    opts
  end
end

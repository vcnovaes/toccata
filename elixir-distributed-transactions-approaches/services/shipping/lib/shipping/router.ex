defmodule Shipping.Router do
  use Plug.Router
  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  get "/health", do: send_json(conn, 200, %{status: "ok", service: "shipping"})
  get "/metrics", do: send_json(conn, 200, %{metrics: []})

  post "/try" do
    %{"address" => address, "items" => items} = conn.body_params
    opts = parse_opts(conn.body_params)
    case Shipping.try_arrange(address, items, opts) do
      {:ok, id} -> send_json(conn, 200, %{status: "ok", shipment_id: id})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/confirm" do
    %{"shipment_id" => id} = conn.body_params
    opts = parse_opts(conn.body_params)
    case Shipping.confirm(id, opts) do
      :ok -> send_json(conn, 200, %{status: "ok"})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/cancel" do
    %{"shipment_id" => id} = conn.body_params
    opts = parse_opts(conn.body_params)
    case Shipping.cancel(id, opts) do
      :ok -> send_json(conn, 200, %{status: "ok"})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/reset", do: (Shipping.reset(); send_json(conn, 200, %{status: "ok"}))
  match _, do: send_json(conn, 404, %{error: "not_found"})

  defp send_json(conn, status, body) do
    conn |> put_resp_content_type("application/json") |> send_resp(status, Jason.encode!(body))
  end

  defp parse_opts(params) do
    []
    |> then(&if params["latency"], do: Keyword.put(&1, :latency, params["latency"]), else: &1)
    |> then(&if params["failure_rate"], do: Keyword.put(&1, :failure_rate, params["failure_rate"]), else: &1)
  end
end

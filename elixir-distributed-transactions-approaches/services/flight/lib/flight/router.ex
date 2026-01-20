defmodule Flight.Router do
  use Plug.Router
  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  get "/health", do: send_json(conn, 200, %{status: "ok", service: "flight"})
  get "/metrics", do: send_json(conn, 200, %{metrics: []})

  post "/flights" do
    %{"flight_id" => flight_id, "seats" => seats} = conn.body_params
    :ok = Flight.add_flight(flight_id, seats)
    send_json(conn, 201, %{status: "created", flight_id: flight_id, seats: seats})
  end

  get "/flights/:id" do
    seats = Flight.get_available_seats(id)
    send_json(conn, 200, %{flight_id: id, available_seats: seats})
  end

  post "/try" do
    %{"flight_id" => flight_id, "seats" => seats} = conn.body_params
    opts = parse_opts(conn.body_params)
    case Flight.try_book(flight_id, seats, opts) do
      {:ok, id} -> send_json(conn, 200, %{status: "ok", booking_id: id})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/confirm" do
    %{"booking_id" => id} = conn.body_params
    opts = parse_opts(conn.body_params)
    case Flight.confirm(id, opts) do
      :ok -> send_json(conn, 200, %{status: "ok"})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/cancel" do
    %{"booking_id" => id} = conn.body_params
    opts = parse_opts(conn.body_params)
    case Flight.cancel(id, opts) do
      :ok -> send_json(conn, 200, %{status: "ok"})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/reset", do: (Flight.reset(); send_json(conn, 200, %{status: "ok"}))
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

defmodule Hotel.Router do
  use Plug.Router
  plug(Plug.Logger)
  plug(:match)
  plug(Plug.Parsers, parsers: [:json], json_decoder: Jason)
  plug(:dispatch)

  get "/health", do: send_json(conn, 200, %{status: "ok", service: "hotel"})
  get "/metrics", do: send_json(conn, 200, %{metrics: []})

  post "/rooms" do
    %{"room_id" => room_id, "available_dates" => dates} = conn.body_params
    dates = Enum.map(dates, &Date.from_iso8601!/1)
    :ok = Hotel.add_room(room_id, dates)
    send_json(conn, 201, %{status: "created", room_id: room_id})
  end

  post "/try" do
    %{"room_id" => room_id, "check_in" => check_in, "check_out" => check_out} = conn.body_params
    opts = parse_opts(conn.body_params)
    check_in = Date.from_iso8601!(check_in)
    check_out = Date.from_iso8601!(check_out)

    case Hotel.try_book(room_id, check_in, check_out, opts) do
      {:ok, id} -> send_json(conn, 200, %{status: "ok", booking_id: id})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/confirm" do
    %{"booking_id" => id} = conn.body_params
    opts = parse_opts(conn.body_params)
    case Hotel.confirm(id, opts) do
      :ok -> send_json(conn, 200, %{status: "ok"})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/cancel" do
    %{"booking_id" => id} = conn.body_params
    opts = parse_opts(conn.body_params)
    case Hotel.cancel(id, opts) do
      :ok -> send_json(conn, 200, %{status: "ok"})
      {:error, reason} -> send_json(conn, 400, %{status: "error", reason: to_string(reason)})
    end
  end

  post "/reset", do: (Hotel.reset(); send_json(conn, 200, %{status: "ok"}))
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

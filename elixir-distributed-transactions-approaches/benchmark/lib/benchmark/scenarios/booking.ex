defmodule Benchmark.Scenarios.Booking do
  @moduledoc """
  Travel booking scenario - complex distributed transaction.

  Services involved:
  - Payment (authorize payment)
  - Flight (book seats)
  - Hotel (book room)
  """

  @payment_url System.get_env("PAYMENT_URL", "http://localhost:4001")
  @flight_url System.get_env("FLIGHT_URL", "http://localhost:4005")
  @hotel_url System.get_env("HOTEL_URL", "http://localhost:4004")

  @doc """
  Run travel booking using Sage (Saga pattern).
  """
  def run_sage(opts \\ []) do
    amount = Keyword.get(opts, :amount, 2500)
    flight_id = Keyword.get(opts, :flight_id, "FL123")
    seats = Keyword.get(opts, :seats, 1)
    room_id = Keyword.get(opts, :room_id, "room101")
    check_in = Keyword.get(opts, :check_in, Date.utc_today() |> Date.to_iso8601())
    check_out = Keyword.get(opts, :check_out, Date.utc_today() |> Date.add(3) |> Date.to_iso8601())

    start_time = System.monotonic_time(:millisecond)

    result =
      Sage.new()
      |> Sage.run(:payment, fn _effects, _opts ->
        case authorize_payment("traveler", amount) do
          {:ok, id} -> {:ok, id}
          {:error, reason} -> {:error, reason}
        end
      end, fn payment_id, _effects, _opts ->
        cancel_payment(payment_id)
        :ok
      end)
      |> Sage.run(:flight, fn _effects, _opts ->
        case book_flight(flight_id, seats) do
          {:ok, id} -> {:ok, id}
          {:error, reason} -> {:error, reason}
        end
      end, fn flight_booking_id, _effects, _opts ->
        cancel_flight(flight_booking_id)
        :ok
      end)
      |> Sage.run(:hotel, fn _effects, _opts ->
        case book_hotel(room_id, check_in, check_out) do
          {:ok, id} -> {:ok, id}
          {:error, reason} -> {:error, reason}
        end
      end, fn hotel_booking_id, _effects, _opts ->
        cancel_hotel(hotel_booking_id)
        :ok
      end)
      |> Sage.run(:confirm_all, fn effects, _opts ->
        confirm_payment(effects.payment)
        confirm_flight(effects.flight)
        confirm_hotel(effects.hotel)
        {:ok, effects}
      end)
      |> Sage.execute()

    end_time = System.monotonic_time(:millisecond)
    latency = end_time - start_time

    case result do
      {:ok, _effect, _effects} ->
        %{success?: true, latency_ms: latency, compensation_ms: nil}

      {:error, _reason} ->
        %{success?: false, latency_ms: latency, compensation_ms: latency}
    end
  end

  @doc """
  Run travel booking using TCC pattern.
  """
  def run_tcc(opts \\ []) do
    amount = Keyword.get(opts, :amount, 2500)
    flight_id = Keyword.get(opts, :flight_id, "FL123")
    seats = Keyword.get(opts, :seats, 1)
    room_id = Keyword.get(opts, :room_id, "room101")
    check_in = Keyword.get(opts, :check_in, Date.utc_today() |> Date.to_iso8601())
    check_out = Keyword.get(opts, :check_out, Date.utc_today() |> Date.add(3) |> Date.to_iso8601())

    start_time = System.monotonic_time(:millisecond)

    result =
      TCC.new()
      |> TCC.run(
        :payment,
        fn effects, params ->
          case authorize_payment("traveler", amount) do
            {:ok, id} -> {:ok, Map.put(effects, :payment_id, id), params}
            {:error, reason} -> {:error, reason}
          end
        end,
        fn effects, params ->
          confirm_payment(effects.payment_id)
          {:ok, effects, params}
        end,
        fn effects, params ->
          if effects[:payment_id], do: cancel_payment(effects.payment_id)
          {:ok, effects, params}
        end
      )
      |> TCC.run(
        :flight,
        fn effects, params ->
          case book_flight(flight_id, seats) do
            {:ok, id} -> {:ok, Map.put(effects, :flight_id, id), params}
            {:error, reason} -> {:error, reason}
          end
        end,
        fn effects, params ->
          confirm_flight(effects.flight_id)
          {:ok, effects, params}
        end,
        fn effects, params ->
          if effects[:flight_id], do: cancel_flight(effects.flight_id)
          {:ok, effects, params}
        end
      )
      |> TCC.run(
        :hotel,
        fn effects, params ->
          case book_hotel(room_id, check_in, check_out) do
            {:ok, id} -> {:ok, Map.put(effects, :hotel_id, id), params}
            {:error, reason} -> {:error, reason}
          end
        end,
        fn effects, params ->
          confirm_hotel(effects.hotel_id)
          {:ok, effects, params}
        end,
        fn effects, params ->
          if effects[:hotel_id], do: cancel_hotel(effects.hotel_id)
          {:ok, effects, params}
        end
      )
      |> TCC.execute(%{amount: amount})

    end_time = System.monotonic_time(:millisecond)
    latency = end_time - start_time

    case result do
      {:ok, _effects, _params} ->
        %{success?: true, latency_ms: latency, compensation_ms: nil}

      {:error, _stage, _reason, _effects} ->
        %{success?: false, latency_ms: latency, compensation_ms: latency}
    end
  end

  # Payment service calls
  defp authorize_payment(account_id, amount) do
    case Req.post("#{@payment_url}/try", json: %{account_id: account_id, amount: amount}) do
      {:ok, %{status: 200, body: %{"reservation_id" => id}}} -> {:ok, id}
      {:ok, %{body: %{"reason" => reason}}} -> {:error, reason}
      {:error, reason} -> {:error, reason}
    end
  end

  defp confirm_payment(id), do: Req.post("#{@payment_url}/confirm", json: %{reservation_id: id})
  defp cancel_payment(id), do: Req.post("#{@payment_url}/cancel", json: %{reservation_id: id})

  # Flight service calls
  defp book_flight(flight_id, seats) do
    case Req.post("#{@flight_url}/try", json: %{flight_id: flight_id, seats: seats}) do
      {:ok, %{status: 200, body: %{"booking_id" => id}}} -> {:ok, id}
      {:ok, %{body: %{"reason" => reason}}} -> {:error, reason}
      {:error, reason} -> {:error, reason}
    end
  end

  defp confirm_flight(id), do: Req.post("#{@flight_url}/confirm", json: %{booking_id: id})
  defp cancel_flight(id), do: Req.post("#{@flight_url}/cancel", json: %{booking_id: id})

  # Hotel service calls
  defp book_hotel(room_id, check_in, check_out) do
    case Req.post("#{@hotel_url}/try", json: %{room_id: room_id, check_in: check_in, check_out: check_out}) do
      {:ok, %{status: 200, body: %{"booking_id" => id}}} -> {:ok, id}
      {:ok, %{body: %{"reason" => reason}}} -> {:error, reason}
      {:error, reason} -> {:error, reason}
    end
  end

  defp confirm_hotel(id), do: Req.post("#{@hotel_url}/confirm", json: %{booking_id: id})
  defp cancel_hotel(id), do: Req.post("#{@hotel_url}/cancel", json: %{booking_id: id})
end

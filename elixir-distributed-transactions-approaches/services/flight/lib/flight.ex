defmodule Flight do
  @moduledoc """
  Flight booking service for distributed transaction benchmarks.
  """

  use Agent

  def start_link(_opts \\ []) do
    Agent.start_link(fn -> %{flights: %{}, bookings: %{}, counter: 0} end, name: __MODULE__)
  end

  def add_flight(flight_id, seats) do
    Agent.update(__MODULE__, fn state ->
      put_in(state, [:flights, flight_id], seats)
    end)
  end

  def get_available_seats(flight_id) do
    Agent.get(__MODULE__, fn state -> Map.get(state.flights, flight_id, 0) end)
  end

  def try_book(flight_id, seats, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      available = Map.get(state.flights, flight_id, 0)

      if available >= seats do
        booking_id = "flight_#{state.counter + 1}"

        new_state =
          state
          |> put_in([:flights, flight_id], available - seats)
          |> put_in([:bookings, booking_id], %{flight_id: flight_id, seats: seats, status: :pending})
          |> update_in([:counter], &(&1 + 1))

        {{:ok, booking_id}, new_state}
      else
        {{:error, :no_seats_available}, state}
      end
    end)
  end

  def confirm(booking_id, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state.bookings, booking_id) do
        %{status: :pending} = booking ->
          new_state = put_in(state, [:bookings, booking_id], %{booking | status: :confirmed})
          {:ok, new_state}

        %{status: :confirmed} -> {:ok, state}
        nil -> {{:error, :booking_not_found}, state}
        %{status: :cancelled} -> {{:error, :booking_cancelled}, state}
      end
    end)
  end

  def cancel(booking_id, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state.bookings, booking_id) do
        %{status: :pending, flight_id: flight_id, seats: seats} = booking ->
          available = Map.get(state.flights, flight_id, 0)

          new_state =
            state
            |> put_in([:flights, flight_id], available + seats)
            |> put_in([:bookings, booking_id], %{booking | status: :cancelled})

          {:ok, new_state}

        %{status: :cancelled} -> {:ok, state}
        nil -> {:ok, state}
        %{status: :confirmed} -> {{:error, :already_confirmed}, state}
      end
    end)
  end

  def reset, do: Agent.update(__MODULE__, fn _ -> %{flights: %{}, bookings: %{}, counter: 0} end)

  defp simulate_latency(opts) do
    case Keyword.get(opts, :latency) do
      nil -> :ok
      latency -> :timer.sleep(latency)
    end
  end

  defp should_fail?(opts), do: :rand.uniform() < Keyword.get(opts, :failure_rate, 0)
end

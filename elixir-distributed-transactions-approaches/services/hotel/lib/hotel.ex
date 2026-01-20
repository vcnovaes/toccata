defmodule Hotel do
  @moduledoc """
  Hotel booking service for distributed transaction benchmarks.
  """

  use Agent

  def start_link(_opts \\ []) do
    Agent.start_link(fn -> %{rooms: %{}, bookings: %{}, counter: 0} end, name: __MODULE__)
  end

  def add_room(room_id, available_dates) do
    Agent.update(__MODULE__, fn state ->
      put_in(state, [:rooms, room_id], MapSet.new(available_dates))
    end)
  end

  def try_book(room_id, check_in, check_out, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      dates = Date.range(check_in, Date.add(check_out, -1)) |> Enum.to_list()
      available = Map.get(state.rooms, room_id, MapSet.new())

      if Enum.all?(dates, &MapSet.member?(available, &1)) do
        booking_id = "hotel_#{state.counter + 1}"
        new_available = Enum.reduce(dates, available, &MapSet.delete(&2, &1))

        new_state =
          state
          |> put_in([:rooms, room_id], new_available)
          |> put_in([:bookings, booking_id], %{room_id: room_id, dates: dates, status: :pending})
          |> update_in([:counter], &(&1 + 1))

        {{:ok, booking_id}, new_state}
      else
        {{:error, :room_not_available}, state}
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
        %{status: :pending, room_id: room_id, dates: dates} = booking ->
          available = Map.get(state.rooms, room_id, MapSet.new())
          new_available = Enum.reduce(dates, available, &MapSet.put(&2, &1))

          new_state =
            state
            |> put_in([:rooms, room_id], new_available)
            |> put_in([:bookings, booking_id], %{booking | status: :cancelled})

          {:ok, new_state}

        %{status: :cancelled} -> {:ok, state}
        nil -> {:ok, state}
        %{status: :confirmed} -> {{:error, :already_confirmed}, state}
      end
    end)
  end

  def reset, do: Agent.update(__MODULE__, fn _ -> %{rooms: %{}, bookings: %{}, counter: 0} end)

  defp simulate_latency(opts) do
    case Keyword.get(opts, :latency) do
      nil -> :ok
      latency -> :timer.sleep(latency)
    end
  end

  defp should_fail?(opts), do: :rand.uniform() < Keyword.get(opts, :failure_rate, 0)
end

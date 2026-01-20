defmodule Shipping do
  @moduledoc """
  Shipping service for distributed transaction benchmarks.
  """

  use Agent

  def start_link(_opts \\ []) do
    Agent.start_link(fn -> %{shipments: %{}, counter: 0} end, name: __MODULE__)
  end

  def try_arrange(address, items, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      shipment_id = "ship_#{state.counter + 1}"

      new_state =
        state
        |> put_in([:shipments, shipment_id], %{address: address, items: items, status: :pending})
        |> update_in([:counter], &(&1 + 1))

      {{:ok, shipment_id}, new_state}
    end)
  end

  def confirm(shipment_id, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state.shipments, shipment_id) do
        %{status: :pending} = shipment ->
          new_state = put_in(state, [:shipments, shipment_id], %{shipment | status: :confirmed})
          {:ok, new_state}

        %{status: :confirmed} -> {:ok, state}
        nil -> {{:error, :shipment_not_found}, state}
        %{status: :cancelled} -> {{:error, :shipment_cancelled}, state}
      end
    end)
  end

  def cancel(shipment_id, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state.shipments, shipment_id) do
        %{status: :pending} = shipment ->
          new_state = put_in(state, [:shipments, shipment_id], %{shipment | status: :cancelled})
          {:ok, new_state}

        %{status: :cancelled} -> {:ok, state}
        nil -> {:ok, state}
        %{status: :confirmed} -> {{:error, :already_confirmed}, state}
      end
    end)
  end

  def reset, do: Agent.update(__MODULE__, fn _ -> %{shipments: %{}, counter: 0} end)

  defp simulate_latency(opts) do
    case Keyword.get(opts, :latency) do
      nil -> :ok
      latency -> :timer.sleep(latency)
    end
  end

  defp should_fail?(opts), do: :rand.uniform() < Keyword.get(opts, :failure_rate, 0)
end

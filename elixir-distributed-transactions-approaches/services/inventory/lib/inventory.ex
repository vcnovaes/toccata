defmodule Inventory do
  @moduledoc """
  Inventory service for distributed transaction benchmarks.

  This service simulates an inventory management system with:
  - Try: Reserve items
  - Confirm: Deduct items from stock
  - Cancel: Release reserved items
  """

  use Agent

  def start_link(_opts \\ []) do
    Agent.start_link(
      fn ->
        %{
          products: %{},
          reservations: %{},
          reservation_counter: 0
        }
      end,
      name: __MODULE__
    )
  end

  def add_product(product_id, quantity) do
    Agent.update(__MODULE__, fn state ->
      put_in(state, [:products, product_id], quantity)
    end)
  end

  def get_stock(product_id) do
    Agent.get(__MODULE__, fn state ->
      Map.get(state.products, product_id, 0)
    end)
  end

  def try_reserve(product_id, quantity, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      stock = Map.get(state.products, product_id, 0)

      if stock >= quantity do
        reservation_id = "inv_res_#{state.reservation_counter + 1}"
        new_stock = stock - quantity

        new_state =
          state
          |> put_in([:products, product_id], new_stock)
          |> put_in([:reservations, reservation_id], %{
            product_id: product_id,
            quantity: quantity,
            status: :pending
          })
          |> update_in([:reservation_counter], &(&1 + 1))

        {{:ok, reservation_id}, new_state}
      else
        {{:error, :insufficient_stock}, state}
      end
    end)
  end

  def confirm(reservation_id, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state.reservations, reservation_id) do
        %{status: :pending} = reservation ->
          new_state =
            put_in(state, [:reservations, reservation_id], %{reservation | status: :confirmed})

          {:ok, new_state}

        %{status: :confirmed} ->
          {:ok, state}

        nil ->
          {{:error, :reservation_not_found}, state}

        %{status: :cancelled} ->
          {{:error, :reservation_cancelled}, state}
      end
    end)
  end

  def cancel(reservation_id, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state.reservations, reservation_id) do
        %{status: :pending, product_id: product_id, quantity: quantity} = reservation ->
          current_stock = Map.get(state.products, product_id, 0)

          new_state =
            state
            |> put_in([:products, product_id], current_stock + quantity)
            |> put_in([:reservations, reservation_id], %{reservation | status: :cancelled})

          {:ok, new_state}

        %{status: :cancelled} ->
          {:ok, state}

        nil ->
          {:ok, state}

        %{status: :confirmed} ->
          {{:error, :already_confirmed}, state}
      end
    end)
  end

  def reset do
    Agent.update(__MODULE__, fn _state ->
      %{products: %{}, reservations: %{}, reservation_counter: 0}
    end)
  end

  defp simulate_latency(opts) do
    case Keyword.get(opts, :latency) do
      nil -> :ok
      latency when is_integer(latency) -> :timer.sleep(latency)
      {min, max} -> :timer.sleep(min + :rand.uniform(max - min))
    end
  end

  defp should_fail?(opts) do
    case Keyword.get(opts, :failure_rate, 0) do
      0 -> false
      rate -> :rand.uniform() < rate
    end
  end
end

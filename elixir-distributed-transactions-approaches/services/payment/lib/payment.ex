defmodule Payment do
  @moduledoc """
  Payment service for distributed transaction benchmarks.

  This service simulates a payment processing system with:
  - Try: Reserve/authorize funds
  - Confirm: Capture/charge funds
  - Cancel: Release/void authorization
  """

  use Agent

  @doc """
  Starts the payment service state.
  """
  def start_link(_opts \\ []) do
    Agent.start_link(
      fn ->
        %{
          accounts: %{},
          reservations: %{},
          reservation_counter: 0
        }
      end,
      name: __MODULE__
    )
  end

  @doc """
  Creates an account with initial balance.
  """
  def create_account(account_id, balance) do
    Agent.update(__MODULE__, fn state ->
      put_in(state, [:accounts, account_id], balance)
    end)
  end

  @doc """
  Gets account balance.
  """
  def get_balance(account_id) do
    Agent.get(__MODULE__, fn state ->
      Map.get(state.accounts, account_id, 0)
    end)
  end

  @doc """
  Try phase: Reserve funds for a transaction.
  Returns {:ok, reservation_id} or {:error, reason}
  """
  def try_reserve(account_id, amount, opts \\ []) do
    # Simulate latency if configured
    simulate_latency(opts)
    # Simulate failure if configured
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      balance = Map.get(state.accounts, account_id, 0)

      if balance >= amount do
        reservation_id = "res_#{state.reservation_counter + 1}"
        new_balance = balance - amount

        new_state =
          state
          |> put_in([:accounts, account_id], new_balance)
          |> put_in([:reservations, reservation_id], %{
            account_id: account_id,
            amount: amount,
            status: :pending
          })
          |> update_in([:reservation_counter], &(&1 + 1))

        {{:ok, reservation_id}, new_state}
      else
        {{:error, :insufficient_funds}, state}
      end
    end)
  end

  @doc """
  Confirm phase: Capture the reserved funds.
  """
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
          # Idempotent - already confirmed
          {:ok, state}

        nil ->
          {{:error, :reservation_not_found}, state}

        %{status: :cancelled} ->
          {{:error, :reservation_cancelled}, state}
      end
    end)
  end

  @doc """
  Cancel phase: Release the reserved funds.
  """
  def cancel(reservation_id, opts \\ []) do
    simulate_latency(opts)
    if should_fail?(opts), do: {:error, :simulated_failure}

    Agent.get_and_update(__MODULE__, fn state ->
      case Map.get(state.reservations, reservation_id) do
        %{status: :pending, account_id: account_id, amount: amount} = reservation ->
          current_balance = Map.get(state.accounts, account_id, 0)

          new_state =
            state
            |> put_in([:accounts, account_id], current_balance + amount)
            |> put_in([:reservations, reservation_id], %{reservation | status: :cancelled})

          {:ok, new_state}

        %{status: :cancelled} ->
          # Idempotent - already cancelled
          {:ok, state}

        nil ->
          # Idempotent - nothing to cancel
          {:ok, state}

        %{status: :confirmed} ->
          {{:error, :already_confirmed}, state}
      end
    end)
  end

  @doc """
  Reset the service state (for testing).
  """
  def reset do
    Agent.update(__MODULE__, fn _state ->
      %{
        accounts: %{},
        reservations: %{},
        reservation_counter: 0
      }
    end)
  end

  @doc """
  Get all reservations (for debugging).
  """
  def get_reservations do
    Agent.get(__MODULE__, fn state -> state.reservations end)
  end

  # Private helpers

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

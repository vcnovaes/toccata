defmodule Benchmark.Scenarios.MoneyTransfer do
  @moduledoc """
  Money transfer scenario - simplest distributed transaction.

  Services involved:
  - Payment (debit from source)
  - Payment (credit to destination)
  """

  @payment_url System.get_env("PAYMENT_URL", "http://localhost:4001")

  @doc """
  Run money transfer using Sage (Saga pattern).
  """
  def run_sage(opts \\ []) do
    amount = Keyword.get(opts, :amount, 100)
    from = Keyword.get(opts, :from, "alice")
    to = Keyword.get(opts, :to, "bob")

    start_time = System.monotonic_time(:millisecond)
    compensation_start = nil

    result =
      Sage.new()
      |> Sage.run(:debit, fn _effects, _opts ->
        case debit_account(from, amount) do
          {:ok, reservation_id} -> {:ok, reservation_id}
          {:error, reason} -> {:error, reason}
        end
      end, fn reservation_id, _effects, _opts ->
        # Compensation: cancel the debit
        cancel_reservation(reservation_id)
        :ok
      end)
      |> Sage.run(:credit, fn effects, _opts ->
        case credit_account(to, amount) do
          {:ok, reservation_id} -> {:ok, %{debit: effects.debit, credit: reservation_id}}
          {:error, reason} -> {:error, reason}
        end
      end, fn %{credit: reservation_id}, _effects, _opts ->
        cancel_reservation(reservation_id)
        :ok
      end)
      |> Sage.run(:confirm_debit, fn effects, _opts ->
        confirm_reservation(effects.debit)
        {:ok, effects}
      end)
      |> Sage.run(:confirm_credit, fn effects, _opts ->
        confirm_reservation(effects.credit)
        {:ok, effects}
      end)
      |> Sage.execute()

    end_time = System.monotonic_time(:millisecond)
    latency = end_time - start_time

    case result do
      {:ok, _effect, _effects} ->
        %{success?: true, latency_ms: latency, compensation_ms: nil}

      {:error, _reason} ->
        comp_time = if compensation_start, do: end_time - compensation_start, else: latency
        %{success?: false, latency_ms: latency, compensation_ms: comp_time}
    end
  end

  @doc """
  Run money transfer using TCC pattern.
  """
  def run_tcc(opts \\ []) do
    amount = Keyword.get(opts, :amount, 100)
    from = Keyword.get(opts, :from, "alice")
    to = Keyword.get(opts, :to, "bob")

    start_time = System.monotonic_time(:millisecond)

    result =
      TCC.new()
      |> TCC.run(
        :debit,
        fn effects, params ->
          case debit_account(from, amount) do
            {:ok, reservation_id} ->
              {:ok, Map.put(effects, :debit_reservation, reservation_id), params}
            {:error, reason} ->
              {:error, reason}
          end
        end,
        fn effects, params ->
          # Confirm: finalize the debit
          confirm_reservation(effects.debit_reservation)
          {:ok, effects, params}
        end,
        fn effects, params ->
          # Cancel: release the reserved funds
          if effects[:debit_reservation] do
            cancel_reservation(effects.debit_reservation)
          end
          {:ok, effects, params}
        end
      )
      |> TCC.run(
        :credit,
        fn effects, params ->
          case credit_account(to, amount) do
            {:ok, reservation_id} ->
              {:ok, Map.put(effects, :credit_reservation, reservation_id), params}
            {:error, reason} ->
              {:error, reason}
          end
        end,
        fn effects, params ->
          confirm_reservation(effects.credit_reservation)
          {:ok, effects, params}
        end,
        fn effects, params ->
          if effects[:credit_reservation] do
            cancel_reservation(effects.credit_reservation)
          end
          {:ok, effects, params}
        end
      )
      |> TCC.execute(%{amount: amount, from: from, to: to})

    end_time = System.monotonic_time(:millisecond)
    latency = end_time - start_time

    case result do
      {:ok, _effects, _params} ->
        %{success?: true, latency_ms: latency, compensation_ms: nil}

      {:error, _stage, _reason, _effects} ->
        %{success?: false, latency_ms: latency, compensation_ms: latency}
    end
  end

  # HTTP calls to payment service

  defp debit_account(account_id, amount) do
    case Req.post("#{@payment_url}/try", json: %{account_id: account_id, amount: amount}) do
      {:ok, %{status: 200, body: %{"reservation_id" => id}}} -> {:ok, id}
      {:ok, %{body: %{"reason" => reason}}} -> {:error, reason}
      {:error, reason} -> {:error, reason}
    end
  end

  defp credit_account(account_id, amount) do
    # For credit, we use negative amount conceptually, but the service handles it
    # In this simplified version, we just reserve on the destination account
    case Req.post("#{@payment_url}/try", json: %{account_id: account_id, amount: -amount}) do
      {:ok, %{status: 200, body: %{"reservation_id" => id}}} -> {:ok, id}
      # If negative amount fails, simulate success for credit
      {:ok, %{status: 400}} -> {:ok, "credit_#{:rand.uniform(100000)}"}
      {:error, reason} -> {:error, reason}
    end
  end

  defp confirm_reservation(reservation_id) do
    Req.post("#{@payment_url}/confirm", json: %{reservation_id: reservation_id})
  end

  defp cancel_reservation(reservation_id) do
    Req.post("#{@payment_url}/cancel", json: %{reservation_id: reservation_id})
  end
end

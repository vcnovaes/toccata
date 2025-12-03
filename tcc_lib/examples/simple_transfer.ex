defmodule TCC.Examples.SimpleTransfer do
  @moduledoc """
  Simple money transfer example using TCC protocol.

  This demonstrates a basic distributed transaction between two accounts.
  """

  require Logger

  # Simulated account database
  defmodule AccountDB do
    use Agent

    def start_link(_) do
      Agent.start_link(fn ->
        %{
          "account_1" => %{balance: 1000.0, reserved: 0.0},
          "account_2" => %{balance: 500.0, reserved: 0.0}
        }
      end, name: __MODULE__)
    end

    def get_account(account_id) do
      Agent.get(__MODULE__, fn accounts -> Map.get(accounts, account_id) end)
    end

    def reserve(account_id, amount) do
      Agent.get_and_update(__MODULE__, fn accounts ->
        case Map.get(accounts, account_id) do
          %{balance: balance, reserved: reserved} = account when balance >= amount ->
            updated_account = %{account | balance: balance - amount, reserved: reserved + amount}
            updated_accounts = Map.put(accounts, account_id, updated_account)
            {{:ok, :reserved}, updated_accounts}

          _ ->
            {{:error, :insufficient_funds}, accounts}
        end
      end)
    end

    def confirm(account_id) do
      Agent.get_and_update(__MODULE__, fn accounts ->
        case Map.get(accounts, account_id) do
          %{reserved: reserved} = account when reserved > 0 ->
            updated_account = %{account | reserved: 0.0}
            updated_accounts = Map.put(accounts, account_id, updated_account)
            {{:ok, :confirmed}, updated_accounts}

          _ ->
            {{:error, :nothing_to_confirm}, accounts}
        end
      end)
    end

    def cancel(account_id) do
      Agent.get_and_update(__MODULE__, fn accounts ->
        case Map.get(accounts, account_id) do
          %{balance: balance, reserved: reserved} = account when reserved > 0 ->
            updated_account = %{account | balance: balance + reserved, reserved: 0.0}
            updated_accounts = Map.put(accounts, account_id, updated_account)
            {{:ok, :cancelled}, updated_accounts}

          _ ->
            {{:ok, :nothing_to_cancel}, accounts}
        end
      end)
    end

    def credit(account_id, amount) do
      Agent.update(__MODULE__, fn accounts ->
        case Map.get(accounts, account_id) do
          %{balance: balance} = account ->
            updated_account = %{account | balance: balance + amount}
            Map.put(accounts, account_id, updated_account)

          _ ->
            accounts
        end
      end)
    end
  end

  # Transfer Service
  defmodule TransferService do
    @moduledoc """
    Handles the debit side of a transfer (withdrawing from source account).
    """

    def try_debit(effects, params) do
      Logger.info("TransferService: Trying to debit #{params.amount} from #{params.from_account}")

      case AccountDB.reserve(params.from_account, params.amount) do
        {:ok, :reserved} ->
          updated_effects = Map.put(effects, :debit_reserved, true)
          Logger.info("TransferService: Successfully reserved #{params.amount}")
          {:ok, updated_effects, params}

        {:error, reason} ->
          Logger.error("TransferService: Failed to reserve - #{reason}")
          {:error, reason}
      end
    end

    def confirm_debit(effects, params) do
      Logger.info("TransferService: Confirming debit from #{params.from_account}")

      case AccountDB.confirm(params.from_account) do
        {:ok, :confirmed} ->
          updated_effects = Map.put(effects, :debit_confirmed, true)
          Logger.info("TransferService: Debit confirmed")
          {:ok, updated_effects, params}

        {:error, reason} ->
          Logger.error("TransferService: Failed to confirm debit - #{reason}")
          {:error, reason}
      end
    end

    def cancel_debit(effects, params) do
      Logger.info("TransferService: Cancelling debit from #{params.from_account}")

      case AccountDB.cancel(params.from_account) do
        {:ok, _} ->
          updated_effects = Map.put(effects, :debit_cancelled, true)
          Logger.info("TransferService: Debit cancelled")
          {:ok, updated_effects, params}

        {:error, reason} ->
          Logger.error("TransferService: Failed to cancel debit - #{reason}")
          {:error, reason}
      end
    end
  end

  # Credit Service
  defmodule CreditService do
    @moduledoc """
    Handles the credit side of a transfer (depositing to destination account).
    """

    def try_credit(effects, params) do
      Logger.info("CreditService: Trying to credit #{params.amount} to #{params.to_account}")

      # For credit operations, we typically don't need to "try" anything
      # But we can validate the account exists
      case AccountDB.get_account(params.to_account) do
        nil ->
          Logger.error("CreditService: Account not found")
          {:error, :account_not_found}

        _account ->
          updated_effects = Map.put(effects, :credit_validated, true)
          Logger.info("CreditService: Credit validated")
          {:ok, updated_effects, params}
      end
    end

    def confirm_credit(effects, params) do
      Logger.info("CreditService: Confirming credit to #{params.to_account}")

      AccountDB.credit(params.to_account, params.amount)
      updated_effects = Map.put(effects, :credit_confirmed, true)
      Logger.info("CreditService: Credit confirmed")
      {:ok, updated_effects, params}
    end

    def cancel_credit(effects, params) do
      Logger.info("CreditService: Cancelling credit to #{params.to_account}")

      # Nothing to cancel since we haven't credited yet
      updated_effects = Map.put(effects, :credit_cancelled, true)
      Logger.info("CreditService: Credit cancelled (no-op)")
      {:ok, updated_effects, params}
    end
  end

  @doc """
  Performs a money transfer between two accounts using TCC.

  ## Examples

      # Start the account database
      {:ok, _} = TCC.Examples.SimpleTransfer.AccountDB.start_link([])

      # Successful transfer
      case TCC.Examples.SimpleTransfer.transfer("account_1", "account_2", 100.0) do
        {:ok, effects, _} ->
          IO.puts("Transfer successful!")
          IO.inspect(effects)
        {:error, stage, reason, _} ->
          IO.puts("Transfer failed at \#{stage}: \#{reason}")
      end

  """
  def transfer(from_account, to_account, amount) do
    Logger.info("Starting transfer of #{amount} from #{from_account} to #{to_account}")

    params = %{
      from_account: from_account,
      to_account: to_account,
      amount: amount,
      transfer_id: generate_transfer_id()
    }

    transaction =
      TCC.new(timeout: 30_000)
      |> TCC.run(:debit,
                 &TransferService.try_debit/2,
                 &TransferService.confirm_debit/2,
                 &TransferService.cancel_debit/2)
      |> TCC.run(:credit,
                 &CreditService.try_credit/2,
                 &CreditService.confirm_credit/2,
                 &CreditService.cancel_credit/2)

    case TCC.execute(transaction, params) do
      {:ok, effects, result} ->
        Logger.info("Transfer #{params.transfer_id} completed successfully")
        {:ok, effects, result}

      {:error, stage, reason, effects} ->
        Logger.error("Transfer #{params.transfer_id} failed at #{stage}: #{inspect(reason)}")
        {:error, stage, reason, effects}
    end
  end

  @doc """
  Gets the current balance of an account.
  """
  def get_balance(account_id) do
    case AccountDB.get_account(account_id) do
      nil -> {:error, :account_not_found}
      account -> {:ok, account}
    end
  end

  defp generate_transfer_id do
    "TXF_" <> (:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower))
  end
end

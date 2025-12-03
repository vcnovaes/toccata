defmodule TCC do
  @moduledoc """
  TCC (Try-Confirm-Cancel) protocol implementation for distributed transactions.

  TCC is a two-phase commit protocol for distributed transactions that provides
  eventual consistency across multiple services. It consists of three phases:

  - **Try**: Reserve resources and validate business rules
  - **Confirm**: Commit the transaction and apply the changes
  - **Cancel**: Rollback the transaction and release reserved resources

  ## Example

      defmodule PaymentService do
        def try_payment(effects, params) do
          # Reserve funds
          case reserve_funds(params.account_id, params.amount) do
            {:ok, reservation_id} ->
              {:ok, effects, Map.put(params, :reservation_id, reservation_id)}
            {:error, reason} ->
              {:error, reason}
          end
        end

        def confirm_payment(effects, params) do
          # Deduct funds
          case deduct_funds(params.reservation_id) do
            {:ok, _} -> {:ok, effects, params}
            {:error, reason} -> {:error, reason}
          end
        end

        def cancel_payment(effects, params) do
          # Release reserved funds
          case release_funds(params.reservation_id) do
            {:ok, _} -> {:ok, effects, params}
            {:error, reason} -> {:error, reason}
          end
        end
      end

      TCC.new()
      |> TCC.run(:payment, &PaymentService.try_payment/2,
                           &PaymentService.confirm_payment/2,
                           &PaymentService.cancel_payment/2)
      |> TCC.run(:inventory, &InventoryService.try_reserve/2,
                             &InventoryService.confirm_reserve/2,
                             &InventoryService.cancel_reserve/2)
      |> TCC.execute(%{account_id: "123", amount: 100})

  """

  alias TCC.Transaction
  alias TCC.Action

  @type t :: Transaction.t()
  @type effects :: map()
  @type params :: any()
  @type stage_name :: atom()

  @doc """
  Creates a new TCC transaction.

  ## Options

    * `:timeout` - Maximum time in milliseconds for the entire transaction (default: 30_000)
    * `:retry_limit` - Number of retries for confirm/cancel phases (default: 3)
    * `:async` - Whether to execute confirm/cancel phases asynchronously (default: false)
    * `:telemetry_prefix` - Prefix for telemetry events (default: [:tcc])

  ## Examples

      iex> transaction = TCC.new()
      iex> is_struct(transaction, TCC.Transaction)
      true

      iex> transaction = TCC.new(timeout: 60_000, retry_limit: 5)
      iex> transaction.opts.timeout
      60_000

  """
  @spec new(keyword()) :: t()
  def new(opts \\ []) do
    Transaction.new(opts)
  end

  @doc """
  Adds a TCC action to the transaction pipeline.

  Each action must implement three functions:
  - Try function: `(effects, params) -> {:ok, effects, params} | {:error, reason}`
  - Confirm function: `(effects, params) -> {:ok, effects, params} | {:error, reason}`
  - Cancel function: `(effects, params) -> {:ok, effects, params} | {:error, reason}`

  ## Parameters

    * `transaction` - The TCC transaction struct
    * `name` - Unique name for this action
    * `try_fun` - Function to execute in the Try phase
    * `confirm_fun` - Function to execute in the Confirm phase
    * `cancel_fun` - Function to execute in the Cancel phase

  ## Examples

      TCC.new()
      |> TCC.run(:payment, &PaymentService.try_payment/2,
                           &PaymentService.confirm_payment/2,
                           &PaymentService.cancel_payment/2)

  """
  @spec run(t(), stage_name(), function(), function(), function()) :: t()
  def run(transaction, name, try_fun, confirm_fun, cancel_fun) do
    action = Action.new(name, try_fun, confirm_fun, cancel_fun)
    Transaction.add_action(transaction, action)
  end

  @doc """
  Adds a TCC action with custom options.

  ## Options

    * `:timeout` - Timeout for this specific action (overrides global timeout)
    * `:retry_limit` - Retry limit for this specific action
    * `:async` - Whether this action should be executed asynchronously

  ## Examples

      TCC.new()
      |> TCC.run_with_opts(:payment,
           &PaymentService.try_payment/2,
           &PaymentService.confirm_payment/2,
           &PaymentService.cancel_payment/2,
           timeout: 10_000, retry_limit: 5)

  """
  @spec run_with_opts(t(), stage_name(), function(), function(), function(), keyword()) :: t()
  def run_with_opts(transaction, name, try_fun, confirm_fun, cancel_fun, opts) do
    action = Action.new(name, try_fun, confirm_fun, cancel_fun, opts)
    Transaction.add_action(transaction, action)
  end

  @doc """
  Executes the TCC transaction with the given parameters.

  The execution follows these steps:
  1. Execute all Try functions in sequence
  2. If all Try functions succeed, execute all Confirm functions
  3. If any Try function fails, execute Cancel functions for completed Try operations
  4. Return the final result with effects and status

  ## Parameters

    * `transaction` - The TCC transaction struct
    * `params` - Initial parameters to pass to the first action

  ## Returns

    * `{:ok, effects, result}` - Transaction completed successfully
    * `{:error, failed_stage, reason, effects}` - Transaction failed with error details

  ## Examples

      case TCC.execute(transaction, %{amount: 100, account_id: "123"}) do
        {:ok, effects, result} ->
          IO.puts("Transaction succeeded")
        {:error, stage, reason, effects} ->
          IO.puts("Transaction failed at stage \#{stage}: \#{inspect(reason)}")
      end

  """
  @spec execute(t(), params()) ::
          {:ok, effects(), params()}
          | {:error, stage_name(), any(), effects()}
  def execute(transaction, params \\ %{}) do
    Transaction.execute(transaction, params)
  end

  @doc """
  Executes the TCC transaction asynchronously.

  Returns a task that can be awaited later.

  ## Examples

      task = TCC.async_execute(transaction, %{amount: 100})
      # Do other work...
      result = Task.await(task, :infinity)

  """
  @spec async_execute(t(), params()) :: Task.t()
  def async_execute(transaction, params \\ %{}) do
    Task.async(fn -> execute(transaction, params) end)
  end

  @doc """
  Returns the list of actions in the transaction.
  """
  @spec actions(t()) :: [Action.t()]
  def actions(%Transaction{actions: actions}), do: actions

  @doc """
  Returns the transaction options.
  """
  @spec options(t()) :: map()
  def options(%Transaction{opts: opts}), do: opts
end

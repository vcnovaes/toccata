defmodule TCC.Transaction do
  @moduledoc """
  Core transaction coordinator for TCC protocol.

  Manages the lifecycle of a distributed transaction, coordinating the
  Try, Confirm, and Cancel phases across multiple services.
  """

  alias TCC.Action

  @type t :: %__MODULE__{
          actions: [Action.t()],
          opts: map(),
          transaction_id: String.t()
        }

  defstruct actions: [], opts: %{}, transaction_id: nil

  @default_opts %{
    timeout: 30_000,
    retry_limit: 3,
    async: false,
    telemetry_prefix: [:tcc]
  }

  @doc """
  Creates a new transaction.
  """
  @spec new(keyword()) :: t()
  def new(opts \\ []) do
    %__MODULE__{
      actions: [],
      opts: Map.merge(@default_opts, Map.new(opts)),
      transaction_id: generate_transaction_id()
    }
  end

  @doc """
  Adds an action to the transaction.
  """
  @spec add_action(t(), Action.t()) :: t()
  def add_action(%__MODULE__{actions: actions} = transaction, action) do
    %{transaction | actions: actions ++ [action]}
  end

  @doc """
  Executes the transaction following the TCC protocol.
  """
  @spec execute(t(), any()) ::
          {:ok, map(), any()}
          | {:error, atom(), any(), map()}
  def execute(%__MODULE__{actions: actions, opts: _opts, transaction_id: tx_id}, params) do
    start_time = System.monotonic_time()
    effects = %{}

    metadata = %{
      transaction_id: tx_id,
      action_count: length(actions)
    }

    :telemetry.execute(
      [:tcc, :transaction, :start],
      %{system_time: System.system_time()},
      metadata
    )

    result =
      case execute_try_phase(actions, effects, params) do
        {:ok, final_effects, final_params, completed_actions} ->
          # All Try operations succeeded, execute Confirm phase
          case execute_confirm_phase(completed_actions, final_effects, final_params) do
            {:ok, confirmed_effects, confirmed_params} ->
              emit_transaction_stop(start_time, metadata, :success)
              {:ok, confirmed_effects, confirmed_params}

            {:error, reason} ->
              # Confirm phase failed, this is a critical error
              # In production, this should trigger alerts and manual intervention
              emit_transaction_stop(start_time, metadata, :confirm_failure)
              {:error, :confirm_phase, reason, final_effects}
          end

        {:error, failed_action, reason, partial_effects, completed_actions} ->
          # Try phase failed, execute Cancel phase for completed actions
          case execute_cancel_phase(completed_actions, partial_effects, params) do
            {:ok, cancelled_effects, _cancelled_params} ->
              emit_transaction_stop(start_time, metadata, :cancelled)
              {:error, failed_action, reason, cancelled_effects}

            {:error, cancel_reason} ->
              # Cancel phase also failed, this is a critical error
              emit_transaction_stop(start_time, metadata, :cancel_failure)
              {:error, :cancel_phase, cancel_reason, partial_effects}
          end
      end

    result
  end

  # Private functions

  defp execute_try_phase(actions, initial_effects, initial_params) do
    execute_try_phase(actions, initial_effects, initial_params, [])
  end

  defp execute_try_phase([], effects, params, completed) do
    {:ok, effects, params, Enum.reverse(completed)}
  end

  defp execute_try_phase([action | rest], effects, params, completed) do
    case Action.execute_try(action, effects, params) do
      {:ok, new_effects, new_params} ->
        # Store both the action and the result for potential cancel
        completed_action = {action, new_params}
        execute_try_phase(rest, new_effects, new_params, [completed_action | completed])

      {:error, reason} ->
        {:error, action.name, reason, effects, Enum.reverse(completed)}
    end
  end

  defp execute_confirm_phase(completed_actions, initial_effects, _initial_params) do
    Enum.reduce_while(completed_actions, {:ok, initial_effects, nil}, fn {action, params},
                                                                         {:ok, effects, _} ->
      case Action.execute_confirm(action, effects, params) do
        {:ok, new_effects, new_params} ->
          {:cont, {:ok, new_effects, new_params}}

        {:error, reason} ->
          {:halt, {:error, reason}}
      end
    end)
  end

  defp execute_cancel_phase(completed_actions, initial_effects, _initial_params) do
    # Execute cancels in reverse order (LIFO)
    completed_actions
    |> Enum.reverse()
    |> Enum.reduce_while({:ok, initial_effects, nil}, fn {action, params}, {:ok, effects, _} ->
      case Action.execute_cancel(action, effects, params) do
        {:ok, new_effects, new_params} ->
          {:cont, {:ok, new_effects, new_params}}

        {:error, reason} ->
          # Continue trying to cancel other actions even if one fails
          # Log the error but don't halt the cancel process
          require Logger
          Logger.error("Cancel failed for action #{action.name}: #{inspect(reason)}")
          {:cont, {:ok, effects, nil}}
      end
    end)
    |> case do
      {:ok, effects, params} -> {:ok, effects, params}
      error -> error
    end
  end

  defp emit_transaction_stop(start_time, metadata, status) do
    duration = System.monotonic_time() - start_time

    :telemetry.execute(
      [:tcc, :transaction, :stop],
      %{duration: duration},
      Map.put(metadata, :status, status)
    )
  end

  defp generate_transaction_id do
    "tcc_" <>
      (:crypto.strong_rand_bytes(16)
       |> Base.encode16(case: :lower))
  end
end

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
          transaction_id: String.t(),
          final_hooks: MapSet.t(),
          tracers: MapSet.t(),
          on_cancel_error: :raise | module()
        }

  defstruct actions: [],
            opts: %{},
            transaction_id: nil,
            final_hooks: MapSet.new(),
            tracers: MapSet.new(),
            on_cancel_error: :raise

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
  def execute(
        %__MODULE__{
          actions: actions,
          opts: _opts,
          transaction_id: tx_id,
          final_hooks: final_hooks,
          tracers: tracers,
          on_cancel_error: on_cancel_error
        },
        params
      ) do
    start_time = System.monotonic_time()
    effects = %{}
    tracer_state = {MapSet.to_list(tracers), params}

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
      case execute_try_phase(actions, effects, params, tracer_state) do
        {:ok, final_effects, final_params, completed_actions} ->
          # All Try operations succeeded, execute Confirm phase
          case execute_confirm_phase(completed_actions, final_effects, final_params, tracer_state) do
            {:ok, confirmed_effects, confirmed_params} ->
              emit_transaction_stop(start_time, metadata, :success)
              {:ok, confirmed_effects, confirmed_params}

            {:error, reason} ->
              # Confirm phase failed, this is a critical error
              # In production, this should trigger alerts and manual intervention
              emit_transaction_stop(start_time, metadata, :confirm_failure)
              {:error, :confirm_phase, reason, final_effects}
          end

        {:error, failed_action, reason, partial_effects, completed_actions, aborted?} ->
          # Try phase failed, execute Cancel phase for completed actions
          case execute_cancel_phase(
                 completed_actions,
                 partial_effects,
                 params,
                 tracer_state,
                 on_cancel_error
               ) do
            {:ok, cancelled_effects, _cancelled_params} ->
              status = if aborted?, do: :aborted, else: :cancelled
              emit_transaction_stop(start_time, metadata, status)
              {:error, failed_action, reason, cancelled_effects}

            {:error, cancel_reason} ->
              # Cancel phase also failed, this is a critical error
              emit_transaction_stop(start_time, metadata, :cancel_failure)
              {:error, :cancel_phase, cancel_reason, partial_effects}
          end
      end

    # Execute final hooks
    notify_final_hooks(result, MapSet.to_list(final_hooks), params)

    result
  end

  # Private functions

  defp execute_try_phase(actions, initial_effects, initial_params, tracer_state) do
    execute_try_phase(actions, initial_effects, initial_params, [], [], tracer_state)
  end

  # All actions processed, await any pending async tasks
  defp execute_try_phase([], effects, params, completed, pending_tasks, tracer_state) do
    case await_async_tasks(pending_tasks, effects, completed, tracer_state) do
      {:ok, new_effects, new_completed, _tracer_state} ->
        {:ok, new_effects, params, Enum.reverse(new_completed)}

      {:error, name, reason, new_effects, new_completed, aborted?} ->
        {:error, name, reason, new_effects, Enum.reverse(new_completed), aborted?}
    end
  end

  # Sync action - await pending async tasks first, then execute
  defp execute_try_phase([%Action{async: false} = action | rest], effects, params, completed, pending_tasks, tracer_state) do
    # Await any pending async tasks before executing sync action
    case await_async_tasks(pending_tasks, effects, completed, tracer_state) do
      {:ok, new_effects, new_completed, tracer_state} ->
        tracer_state = notify_tracers(tracer_state, action.name, :start_try)
        result = Action.execute_try(action, new_effects, params)
        tracer_state = notify_tracers(tracer_state, action.name, :finish_try)

        case result do
          {:ok, result_effects, new_params} ->
            completed_action = {action, new_params}
            execute_try_phase(rest, result_effects, new_params, [completed_action | new_completed], [], tracer_state)

          {:error, reason} ->
            {:error, action.name, reason, new_effects, Enum.reverse(new_completed), false}

          {:abort, reason} ->
            {:error, action.name, reason, new_effects, Enum.reverse(new_completed), true}
        end

      {:error, name, reason, new_effects, new_completed, aborted?} ->
        {:error, name, reason, new_effects, Enum.reverse(new_completed), aborted?}
    end
  end

  # Async action - start task and continue
  defp execute_try_phase([%Action{async: true} = action | rest], effects, params, completed, pending_tasks, tracer_state) do
    tracer_state = notify_tracers(tracer_state, action.name, :start_try)
    timeout = Map.get(action.opts, :timeout, 5_000)

    task =
      Task.async(fn ->
        Action.execute_try(action, effects, params)
      end)

    pending_task = {action, task, timeout, params}
    execute_try_phase(rest, effects, params, completed, [pending_task | pending_tasks], tracer_state)
  end

  # Await all pending async tasks
  defp await_async_tasks([], effects, completed, tracer_state) do
    {:ok, effects, completed, tracer_state}
  end

  defp await_async_tasks(pending_tasks, effects, completed, tracer_state) do
    # Await all tasks and collect results
    results =
      Enum.map(pending_tasks, fn {action, task, timeout, params} ->
        result =
          case Task.yield(task, timeout) || Task.shutdown(task) do
            {:ok, result} -> result
            nil -> {:error, :timeout}
            {:exit, reason} -> {:error, {:exit, reason}}
          end

        {action, result, params}
      end)

    # Notify tracers for all finished tasks
    tracer_state =
      Enum.reduce(results, tracer_state, fn {action, _result, _params}, ts ->
        notify_tracers(ts, action.name, :finish_try)
      end)

    # Process results - find first error or collect all successes
    process_async_results(results, effects, completed, tracer_state)
  end

  defp process_async_results(results, effects, completed, tracer_state) do
    Enum.reduce_while(results, {:ok, effects, completed, tracer_state}, fn
      {action, {:ok, new_effects, new_params}, _params}, {:ok, acc_effects, acc_completed, ts} ->
        merged_effects = Map.merge(acc_effects, new_effects)
        completed_action = {action, new_params}
        {:cont, {:ok, merged_effects, [completed_action | acc_completed], ts}}

      {action, {:error, reason}, _params}, {:ok, acc_effects, acc_completed, _ts} ->
        {:halt, {:error, action.name, reason, acc_effects, acc_completed, false}}

      {action, {:abort, reason}, _params}, {:ok, acc_effects, acc_completed, _ts} ->
        {:halt, {:error, action.name, reason, acc_effects, acc_completed, true}}
    end)
  end

  defp execute_confirm_phase(completed_actions, initial_effects, _initial_params, tracer_state) do
    Enum.reduce_while(completed_actions, {:ok, initial_effects, nil, tracer_state}, fn {action, params},
                                                                                        {:ok, effects, _, ts} ->
      ts = notify_tracers(ts, action.name, :start_confirm)
      result = Action.execute_confirm(action, effects, params)
      ts = notify_tracers(ts, action.name, :finish_confirm)

      case result do
        {:ok, new_effects, new_params} ->
          {:cont, {:ok, new_effects, new_params, ts}}

        {:error, reason} ->
          {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, effects, params, _ts} -> {:ok, effects, params}
      {:error, reason} -> {:error, reason}
    end
  end

  defp execute_cancel_phase(
         completed_actions,
         initial_effects,
         _initial_params,
         tracer_state,
         on_cancel_error
       ) do
    # Execute cancels in reverse order (LIFO)
    completed_actions
    |> Enum.reverse()
    |> Enum.reduce_while({:ok, initial_effects, nil, tracer_state}, fn {action, params},
                                                                       {:ok, effects, _, ts} ->
      ts = notify_tracers(ts, action.name, :start_cancel)

      result =
        try do
          Action.execute_cancel(action, effects, params)
        rescue
          exception ->
            {:critical_error, {:exception, exception, __STACKTRACE__}, action, effects}
        catch
          :exit, reason ->
            {:critical_error, {:exit, reason}, action, effects}

          :throw, error ->
            {:critical_error, {:throw, error}, action, effects}
        end

      ts = notify_tracers(ts, action.name, :finish_cancel)

      case result do
        {:ok, new_effects, new_params} ->
          {:cont, {:ok, new_effects, new_params, ts}}

        {:error, reason} ->
          # Continue trying to cancel other actions even if one fails
          # Log the error but don't halt the cancel process
          require Logger
          Logger.error("Cancel failed for action #{action.name}: #{inspect(reason)}")
          {:cont, {:ok, effects, nil, ts}}

        {:critical_error, error, _action, effects} ->
          if on_cancel_error == :raise do
            reraise_critical_error(error)
          else
            # Let the error handler deal with remaining cancellations
            {:halt, {:critical_error, error, effects, on_cancel_error}}
          end
      end
    end)
    |> case do
      {:ok, effects, params, _ts} -> {:ok, effects, params}
      {:critical_error, _error, effects, _handler} -> {:error, :cancel_critical_error, effects}
      error -> error
    end
  end

  defp reraise_critical_error({:exception, exception, stacktrace}) do
    reraise exception, stacktrace
  end

  defp reraise_critical_error({:exit, reason}) do
    exit(reason)
  end

  defp reraise_critical_error({:throw, error}) do
    throw(error)
  end

  defp notify_tracers({[], _state} = tracer_state, _name, _action), do: tracer_state

  defp notify_tracers({tracers, state}, name, action) do
    new_state =
      Enum.reduce(tracers, state, fn tracer, acc_state ->
        try do
          tracer.handle_event(name, action, acc_state)
        rescue
          _ -> acc_state
        catch
          _, _ -> acc_state
        end
      end)

    {tracers, new_state}
  end

  defp notify_final_hooks(_result, [], _opts), do: :ok

  defp notify_final_hooks(result, final_hooks, opts) do
    status = if elem(result, 0) == :ok, do: :ok, else: :error

    Enum.each(final_hooks, fn
      {module, function, args} ->
        try do
          apply(module, function, [status, opts | args])
        rescue
          e ->
            require Logger
            Logger.error("Final hook #{inspect(module)}.#{function} failed: #{inspect(e)}")
        catch
          _, _ -> :ok
        end

      hook when is_function(hook, 2) ->
        try do
          hook.(status, opts)
        rescue
          e ->
            require Logger
            Logger.error("Final hook failed: #{inspect(e)}")
        catch
          _, _ -> :ok
        end
    end)
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

defmodule TCC.Action do
  @moduledoc """
  Represents a single TCC action in a distributed transaction.

  Each action consists of three phases:
  - Try: Reserve resources and validate
  - Confirm: Commit the changes
  - Cancel: Rollback and release resources
  """

  @typedoc """
  Callback type - can be an anonymous function or MFA tuple.
  """
  @type callback :: function() | {module(), atom(), list()}

  @type t :: %__MODULE__{
          name: atom(),
          try_fun: callback(),
          confirm_fun: callback(),
          cancel_fun: callback(),
          opts: map(),
          async: boolean()
        }

  @enforce_keys [:name, :try_fun, :confirm_fun, :cancel_fun]
  defstruct [:name, :try_fun, :confirm_fun, :cancel_fun, opts: %{}, async: false]

  # Guards for MFA tuples
  defguardp is_mfa(mfa)
            when is_tuple(mfa) and tuple_size(mfa) == 3 and
                   is_atom(elem(mfa, 0)) and is_atom(elem(mfa, 1)) and is_list(elem(mfa, 2))

  defguardp is_callback(cb) when is_function(cb, 2) or is_mfa(cb)

  @doc """
  Creates a new TCC action.

  Callbacks can be either anonymous functions or MFA tuples `{Module, :function, args}`.
  When using MFA tuples, `effects` and `params` are prepended to the args list.

  ## Examples

      # Using anonymous functions
      iex> TCC.Action.new(:payment, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      %TCC.Action{name: :payment, ...}

      # Using MFA tuples
      iex> TCC.Action.new(:payment,
      ...>   {PaymentService, :try_payment, []},
      ...>   {PaymentService, :confirm_payment, []},
      ...>   {PaymentService, :cancel_payment, []})
      %TCC.Action{name: :payment, ...}

  """
  @spec new(atom(), callback(), callback(), callback(), keyword()) :: t()
  def new(name, try_fun, confirm_fun, cancel_fun, opts \\ [])
      when is_callback(try_fun) and is_callback(confirm_fun) and is_callback(cancel_fun) do
    {async, opts} = Keyword.pop(opts, :async, false)

    %__MODULE__{
      name: name,
      try_fun: try_fun,
      confirm_fun: confirm_fun,
      cancel_fun: cancel_fun,
      opts: Map.new(opts),
      async: async
    }
  end

  @doc """
  Creates a new async TCC action.

  Async actions are executed in parallel during the Try phase. They are awaited
  before the next synchronous action or at the end of the Try phase.

  ## Options

    * `:timeout` - Time in milliseconds to wait for the async action (default: 5000)

  ## Example

      TCC.new()
      |> TCC.run_async(:payment, &try_fn/2, &confirm_fn/2, &cancel_fn/2, timeout: 10_000)

  """
  @spec new_async(atom(), callback(), callback(), callback(), keyword()) :: t()
  def new_async(name, try_fun, confirm_fun, cancel_fun, opts \\ [])
      when is_callback(try_fun) and is_callback(confirm_fun) and is_callback(cancel_fun) do
    %__MODULE__{
      name: name,
      try_fun: try_fun,
      confirm_fun: confirm_fun,
      cancel_fun: cancel_fun,
      opts: Map.new(opts),
      async: true
    }
  end

  @doc """
  Returns true if the action is async.
  """
  @spec async?(t()) :: boolean()
  def async?(%__MODULE__{async: async}), do: async

  @doc """
  Executes the Try phase of the action.

  The try function can return:
  - `{:ok, effects, params}` - Success, continue to next action
  - `{:error, reason}` - Error, trigger cancel phase (allows retry)
  - `{:abort, reason}` - Unrecoverable error, trigger cancel phase (no retry)
  """
  @spec execute_try(t(), map(), any()) :: {:ok, map(), any()} | {:error, any()} | {:abort, any()}
  def execute_try(%__MODULE__{try_fun: try_fun, name: name, opts: opts}, effects, params) do
    timeout = Map.get(opts, :timeout, 5_000)

    execute_with_telemetry(name, :try, timeout, fn ->
      apply_callback(try_fun, effects, params)
    end)
  end

  @doc """
  Executes the Confirm phase of the action with retry logic.
  """
  @spec execute_confirm(t(), map(), any()) :: {:ok, map(), any()} | {:error, any()}
  def execute_confirm(
        %__MODULE__{confirm_fun: confirm_fun, name: name, opts: opts},
        effects,
        params
      ) do
    timeout = Map.get(opts, :timeout, 5_000)
    retry_limit = Map.get(opts, :retry_limit, 3)

    execute_with_retry(name, :confirm, timeout, retry_limit, fn ->
      apply_callback(confirm_fun, effects, params)
    end)
  end

  @doc """
  Executes the Cancel phase of the action with retry logic.
  """
  @spec execute_cancel(t(), map(), any()) :: {:ok, map(), any()} | {:error, any()}
  def execute_cancel(%__MODULE__{cancel_fun: cancel_fun, name: name, opts: opts}, effects, params) do
    timeout = Map.get(opts, :timeout, 5_000)
    retry_limit = Map.get(opts, :retry_limit, 3)

    execute_with_retry(name, :cancel, timeout, retry_limit, fn ->
      apply_callback(cancel_fun, effects, params)
    end)
  end

  # Apply callback - handles both anonymous functions and MFA tuples
  defp apply_callback({module, function, args}, effects, params)
       when is_mfa({module, function, args}) do
    apply(module, function, [effects, params | args])
  end

  defp apply_callback(fun, effects, params) when is_function(fun, 2) do
    fun.(effects, params)
  end

  # Private helpers

  defp execute_with_telemetry(name, phase, timeout, fun) do
    start_time = System.monotonic_time()
    metadata = %{action: name, phase: phase}

    :telemetry.execute(
      [:tcc, :action, :start],
      %{system_time: System.system_time()},
      metadata
    )

    try do
      result =
        case Task.await(Task.async(fun), timeout) do
          {:ok, _effects, _params} = success ->
            duration = System.monotonic_time() - start_time

            :telemetry.execute(
              [:tcc, :action, :stop],
              %{duration: duration},
              Map.put(metadata, :status, :success)
            )

            success

          {:error, reason} = error ->
            duration = System.monotonic_time() - start_time

            :telemetry.execute(
              [:tcc, :action, :stop],
              %{duration: duration},
              Map.merge(metadata, %{status: :error, reason: reason})
            )

            error

          {:abort, reason} = abort ->
            duration = System.monotonic_time() - start_time

            :telemetry.execute(
              [:tcc, :action, :stop],
              %{duration: duration},
              Map.merge(metadata, %{status: :abort, reason: reason})
            )

            abort
        end

      result
    rescue
      e ->
        duration = System.monotonic_time() - start_time

        :telemetry.execute(
          [:tcc, :action, :exception],
          %{duration: duration},
          Map.merge(metadata, %{kind: :error, reason: e})
        )

        {:error, e}
    end
  end

  defp execute_with_retry(name, phase, timeout, retry_opts, fun, attempt \\ 1)

  defp execute_with_retry(name, phase, timeout, retry_opts, fun, attempt)
       when is_map(retry_opts) do
    case execute_with_telemetry(name, phase, timeout, fun) do
      {:ok, _effects, _params} = success ->
        success

      {:error, _reason} = error ->
        if TCC.Retries.retry_with_backoff?(attempt, retry_opts) do
          execute_with_retry(name, phase, timeout, retry_opts, fun, attempt + 1)
        else
          error
        end
    end
  end

  defp execute_with_retry(name, phase, timeout, retry_limit, fun, attempt)
       when is_integer(retry_limit) do
    # Convert legacy retry_limit to retry_opts map
    retry_opts = %{
      retry_limit: retry_limit,
      base_backoff: 100,
      max_backoff: 5_000,
      enable_jitter: true
    }

    execute_with_retry(name, phase, timeout, retry_opts, fun, attempt)
  end
end

defmodule TCC.Action do
  @moduledoc """
  Represents a single TCC action in a distributed transaction.

  Each action consists of three phases:
  - Try: Reserve resources and validate
  - Confirm: Commit the changes
  - Cancel: Rollback and release resources
  """

  @type t :: %__MODULE__{
          name: atom(),
          try_fun: function(),
          confirm_fun: function(),
          cancel_fun: function(),
          opts: map()
        }

  @enforce_keys [:name, :try_fun, :confirm_fun, :cancel_fun]
  defstruct [:name, :try_fun, :confirm_fun, :cancel_fun, opts: %{}]

  @doc """
  Creates a new TCC action.

  ## Examples

      iex> TCC.Action.new(:payment, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      %TCC.Action{name: :payment, ...}

  """
  @spec new(atom(), function(), function(), function(), keyword()) :: t()
  def new(name, try_fun, confirm_fun, cancel_fun, opts \\ []) do
    %__MODULE__{
      name: name,
      try_fun: try_fun,
      confirm_fun: confirm_fun,
      cancel_fun: cancel_fun,
      opts: Map.new(opts)
    }
  end

  @doc """
  Executes the Try phase of the action.
  """
  @spec execute_try(t(), map(), any()) :: {:ok, map(), any()} | {:error, any()}
  def execute_try(%__MODULE__{try_fun: try_fun, name: name, opts: opts}, effects, params) do
    timeout = Map.get(opts, :timeout, 5_000)

    execute_with_telemetry(name, :try, timeout, fn ->
      try_fun.(effects, params)
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
      confirm_fun.(effects, params)
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
      cancel_fun.(effects, params)
    end)
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

  defp execute_with_retry(name, phase, timeout, retry_limit, fun, attempt \\ 1) do
    case execute_with_telemetry(name, phase, timeout, fun) do
      {:ok, _effects, _params} = success ->
        success

      {:error, _reason} = error ->
        if attempt < retry_limit do
          # Exponential backoff
          :timer.sleep(trunc(:math.pow(2, attempt) * 100))
          execute_with_retry(name, phase, timeout, retry_limit, fun, attempt + 1)
        else
          error
        end
    end
  end
end

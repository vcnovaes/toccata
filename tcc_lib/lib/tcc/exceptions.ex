defmodule TCC.EmptyError do
  @moduledoc """
  Raised at runtime when an empty TCC transaction is executed.
  """
  defexception [:message]

  @doc false
  def exception(_opts) do
    message = "trying to execute empty TCC transaction is not allowed"
    %__MODULE__{message: message}
  end
end

defmodule TCC.DuplicateStageError do
  @moduledoc """
  Raised at runtime when an action with a duplicated name is added to a TCC transaction.
  """
  defexception [:message]

  @impl true
  def exception(opts) do
    transaction = Keyword.fetch!(opts, :transaction)
    name = Keyword.fetch!(opts, :name)

    message = """
    #{inspect(name)} is already a member of the TCC transaction:

      #{inspect(transaction)}
    """

    %__MODULE__{message: message}
  end
end

defmodule TCC.DuplicateTracerError do
  @moduledoc """
  Raised at runtime when a duplicated tracer is added to a TCC transaction.
  """
  defexception [:message]

  @impl true
  def exception(opts) do
    transaction = Keyword.fetch!(opts, :transaction)
    module = Keyword.fetch!(opts, :module)

    message = """
    #{inspect(module)} is already defined as tracer for TCC transaction:

      #{inspect(transaction)}
    """

    %__MODULE__{message: message}
  end
end

defmodule TCC.DuplicateFinalHookError do
  @moduledoc """
  Raised at runtime when a duplicated final hook is added to a TCC transaction.
  """
  defexception [:message]

  @impl true
  def exception(opts) do
    transaction = Keyword.fetch!(opts, :transaction)
    hook = Keyword.fetch!(opts, :hook)

    message = """
    #{format_hook(hook)} is already defined as final hook for TCC transaction:

      #{inspect(transaction)}
    """

    %__MODULE__{message: message}
  end

  defp format_hook({m, f, a}), do: "#{inspect(m)}.#{to_string(f)}/#{to_string(length(a) + 2)}"
  defp format_hook(hook), do: inspect(hook)
end

defmodule TCC.AsyncTimeoutError do
  @moduledoc """
  Raised at runtime when an asynchronous action times out.
  """
  defexception [:name, :timeout]

  @impl true
  def message(%__MODULE__{name: name, timeout: timeout}) do
    """
    asynchronous action #{inspect(name)} has timed out,
    expected it to return within #{to_string(timeout)} milliseconds
    """
  end
end

defmodule TCC.MalformedTryReturnError do
  @moduledoc """
  Raised at runtime when a Try function returns a malformed result.
  """
  defexception [:action, :return]

  @impl true
  def message(%__MODULE__{action: action, return: return}) do
    """
    expected Try function for action #{inspect(action)} to return
    {:ok, effects, params}, {:error, reason} or {:abort, reason}, got:

      #{inspect(return)}
    """
  end
end

defmodule TCC.MalformedConfirmReturnError do
  @moduledoc """
  Raised at runtime when a Confirm function returns a malformed result.
  """
  defexception [:action, :return]

  @impl true
  def message(%__MODULE__{action: action, return: return}) do
    """
    expected Confirm function for action #{inspect(action)} to return
    {:ok, effects, params} or {:error, reason}, got:

      #{inspect(return)}
    """
  end
end

defmodule TCC.MalformedCancelReturnError do
  @moduledoc """
  Raised at runtime when a Cancel function returns a malformed result.
  """
  defexception [:action, :return]

  @impl true
  def message(%__MODULE__{action: action, return: return}) do
    """
    expected Cancel function for action #{inspect(action)} to return
    {:ok, effects, params} or {:error, reason}, got:

      #{inspect(return)}
    """
  end
end

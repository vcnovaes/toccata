defmodule TCC.CancelErrorHandler do
  @moduledoc """
  This module provides behaviour for cancel phase error handling.

  When a critical error occurs during the Cancel phase (exception raised, exit signal,
  or throw), the default behavior is to raise the error. However, you can register
  a custom error handler to implement alternative recovery strategies.

  ## Recovery Strategies

  Some solutions you might want to implement:

  - Send notification to a Slack channel about need of manual resolution
  - Retry the cancel operation with different parameters
  - Log the error and continue with other cancellations
  - Spawn a new supervised process that would retry cancellation later
    (useful when you have connection issues that would be resolved at some point)

  ## Example

      defmodule MyCancelErrorHandler do
        @behaviour TCC.CancelErrorHandler

        require Logger

        @impl true
        def handle_error({:exception, exception, stacktrace}, cancellations_to_run, opts) do
          Logger.error("Cancel failed: \#{Exception.message(exception)}")
          # Notify external system
          notify_slack(exception, cancellations_to_run)
          {:error, :cancel_failed}
        end

        def handle_error({:exit, reason}, cancellations_to_run, opts) do
          Logger.error("Cancel exited: \#{inspect(reason)}")
          {:error, :cancel_exit}
        end

        def handle_error({:throw, error}, cancellations_to_run, opts) do
          Logger.error("Cancel threw: \#{inspect(error)}")
          {:error, :cancel_throw}
        end
      end

      TCC.new()
      |> TCC.with_cancel_error_handler(MyCancelErrorHandler)
      |> TCC.run(:payment, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.execute(%{})

  ## Important Notes

  - The error handler is only called for critical errors (exceptions, exits, throws)
  - Regular `{:error, reason}` returns from cancel functions are handled normally
  - The handler receives information about remaining cancellations that need to run
  - You should always return `{:error, reason}` from the handler
  """

  @typedoc """
  The type of error that occurred during cancel execution.

  - `{:exception, exception, stacktrace}` - An exception was raised
  - `{:exit, reason}` - A process exit occurred
  - `{:throw, error}` - An error was thrown
  """
  @type error ::
          {:exception, exception :: Exception.t(), stacktrace :: Exception.stacktrace()}
          | {:exit, reason :: any()}
          | {:throw, error :: any()}

  @typedoc """
  List of cancellations that still need to be executed.

  Each tuple contains:
  - `name` - The name of the action
  - `cancel_fun` - The cancel function to execute
  - `effect_to_cancel` - The effect that needs to be cancelled
  """
  @type cancellations_to_run :: [
          {name :: TCC.stage_name(), cancel_fun :: function(), effect_to_cancel :: any()}
        ]

  @doc """
  Handler for critical errors during cancel phase execution.

  It receives:
  - `error` - The error that occurred (see `t:error/0`)
  - `cancellations_to_run` - List of remaining cancellations (see `t:cancellations_to_run/0`)
  - `opts` - Options passed to `TCC.execute/2`

  Must return `{:error, reason}` which will be returned by the TCC execution.
  """
  @callback handle_error(
              error :: error(),
              cancellations_to_run :: cancellations_to_run(),
              opts :: any()
            ) :: {:error, reason :: any()}
end

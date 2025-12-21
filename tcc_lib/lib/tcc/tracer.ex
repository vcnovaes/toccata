defmodule TCC.Tracer do
  @moduledoc """
  This module provides behaviour for TCC tracers.

  Tracing module is called before and after each phase of TCC execution:
  Try, Confirm, and Cancel phases. This is useful for monitoring, logging,
  and collecting metrics about transaction execution.

  ## Hooks State

  All hooks share their state, which by default contains options passed to `execute/2` function.
  This is useful if you want to persist timer of stage execution start and then persist it somewhere.

  Altering this state won't affect transactions or phases in any way,
  changes would be visible only to other tracing calls.

  ## Example

      defmodule MyTracer do
        @behaviour TCC.Tracer

        @impl true
        def handle_event(name, action, state) do
          IO.puts("Action \#{name}: \#{action}")
          state
        end
      end

      TCC.new()
      |> TCC.with_tracer(MyTracer)
      |> TCC.run(:payment, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.execute(%{})
  """

  @typedoc """
  The type of tracing action/event.

  - `:start_try` - Before executing Try phase for an action
  - `:finish_try` - After executing Try phase for an action
  - `:start_confirm` - Before executing Confirm phase for an action
  - `:finish_confirm` - After executing Confirm phase for an action
  - `:start_cancel` - Before executing Cancel phase for an action
  - `:finish_cancel` - After executing Cancel phase for an action
  """
  @type action ::
          :start_try
          | :finish_try
          | :start_confirm
          | :finish_confirm
          | :start_cancel
          | :finish_cancel

  @doc """
  Handler for TCC execution event.

  It receives the name of the TCC action stage, type of event (see `t:action/0`)
  and state shared for all tracing calls (see "Hooks State" in module doc).

  Returns updated state.
  """
  @callback handle_event(name :: TCC.stage_name(), action :: action(), state :: any()) :: any()
end

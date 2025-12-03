defmodule TCC.ActionTest do
  use ExUnit.Case
  alias TCC.Action

  describe "Action.new/5" do
    test "creates a new action with required fields" do
      try_fun = fn effects, params -> {:ok, effects, params} end
      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      action = Action.new(:test, try_fun, confirm_fun, cancel_fun)

      assert action.name == :test
      assert is_function(action.try_fun, 2)
      assert is_function(action.confirm_fun, 2)
      assert is_function(action.cancel_fun, 2)
      assert action.opts == %{}
    end

    test "creates an action with custom options" do
      try_fun = fn effects, params -> {:ok, effects, params} end
      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      action = Action.new(:test, try_fun, confirm_fun, cancel_fun, timeout: 5_000)

      assert action.opts == %{timeout: 5_000}
    end
  end

  describe "Action.execute_try/3" do
    test "executes try function successfully" do
      try_fun = fn effects, params ->
        {:ok, Map.put(effects, :executed, true), Map.put(params, :result, :success)}
      end

      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      action = Action.new(:test, try_fun, confirm_fun, cancel_fun)

      assert {:ok, effects, params} = Action.execute_try(action, %{}, %{})
      assert effects.executed == true
      assert params.result == :success
    end

    test "handles try function failure" do
      try_fun = fn _effects, _params -> {:error, :test_error} end
      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      action = Action.new(:test, try_fun, confirm_fun, cancel_fun)

      assert {:error, :test_error} = Action.execute_try(action, %{}, %{})
    end
  end

  describe "Action.execute_confirm/3" do
    test "executes confirm function successfully" do
      try_fun = fn effects, params -> {:ok, effects, params} end

      confirm_fun = fn effects, params ->
        {:ok, Map.put(effects, :confirmed, true), params}
      end

      cancel_fun = fn effects, params -> {:ok, effects, params} end

      action = Action.new(:test, try_fun, confirm_fun, cancel_fun)

      assert {:ok, effects, _params} = Action.execute_confirm(action, %{}, %{})
      assert effects.confirmed == true
    end

    test "retries on failure" do
      agent_name = :confirm_retry_agent
      {:ok, _} = Agent.start_link(fn -> 0 end, name: agent_name)

      try_fun = fn effects, params -> {:ok, effects, params} end

      confirm_fun = fn effects, params ->
        count = Agent.get_and_update(agent_name, fn c -> {c, c + 1} end)

        if count < 2 do
          {:error, :temporary_failure}
        else
          {:ok, Map.put(effects, :confirmed, true), params}
        end
      end

      cancel_fun = fn effects, params -> {:ok, effects, params} end

      action = Action.new(:test, try_fun, confirm_fun, cancel_fun, retry_limit: 3)

      assert {:ok, effects, _params} = Action.execute_confirm(action, %{}, %{})
      assert effects.confirmed == true

      # Verify it was called 3 times (initial + 2 retries)
      final_count = Agent.get(agent_name, & &1)
      assert final_count == 3

      Agent.stop(agent_name)
    end
  end

  describe "Action.execute_cancel/3" do
    test "executes cancel function successfully" do
      try_fun = fn effects, params -> {:ok, effects, params} end
      confirm_fun = fn effects, params -> {:ok, effects, params} end

      cancel_fun = fn effects, params ->
        {:ok, Map.put(effects, :cancelled, true), params}
      end

      action = Action.new(:test, try_fun, confirm_fun, cancel_fun)

      assert {:ok, effects, _params} = Action.execute_cancel(action, %{}, %{})
      assert effects.cancelled == true
    end
  end
end

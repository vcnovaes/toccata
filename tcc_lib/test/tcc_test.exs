defmodule TCCTest do
  use ExUnit.Case
  doctest TCC

  describe "TCC.new/1" do
    test "creates a new transaction with default options" do
      transaction = TCC.new()

      assert %TCC.Transaction{} = transaction
      assert transaction.actions == []
      assert transaction.transaction_id != nil
      assert transaction.opts.timeout == 30_000
      assert transaction.opts.retry_limit == 3
    end

    test "creates a new transaction with custom options" do
      transaction = TCC.new(timeout: 60_000, retry_limit: 5)

      assert transaction.opts.timeout == 60_000
      assert transaction.opts.retry_limit == 5
    end
  end

  describe "TCC.run/5" do
    test "adds an action to the transaction" do
      try_fun = fn effects, params -> {:ok, effects, params} end
      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      transaction =
        TCC.new()
        |> TCC.run(:test_action, try_fun, confirm_fun, cancel_fun)

      assert length(transaction.actions) == 1
      assert hd(transaction.actions).name == :test_action
    end

    test "adds multiple actions to the transaction" do
      try_fun = fn effects, params -> {:ok, effects, params} end
      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      transaction =
        TCC.new()
        |> TCC.run(:action1, try_fun, confirm_fun, cancel_fun)
        |> TCC.run(:action2, try_fun, confirm_fun, cancel_fun)

      assert length(transaction.actions) == 2
      assert Enum.map(transaction.actions, & &1.name) == [:action1, :action2]
    end
  end

  describe "TCC.execute/2" do
    test "successfully executes all phases when all operations succeed" do
      try_fun = fn effects, params ->
        {:ok, Map.put(effects, :try_executed, true), Map.put(params, :step, :tried)}
      end

      confirm_fun = fn effects, params ->
        {:ok, Map.put(effects, :confirm_executed, true), Map.put(params, :step, :confirmed)}
      end

      cancel_fun = fn effects, params ->
        {:ok, Map.put(effects, :cancel_executed, true), Map.put(params, :step, :cancelled)}
      end

      transaction =
        TCC.new()
        |> TCC.run(:test, try_fun, confirm_fun, cancel_fun)

      assert {:ok, effects, result} = TCC.execute(transaction, %{})
      assert effects.try_executed == true
      assert effects.confirm_executed == true
      refute Map.has_key?(effects, :cancel_executed)
      assert result.step == :confirmed
    end

    test "executes cancel phase when try phase fails" do
      successful_try = fn effects, params ->
        {:ok, Map.put(effects, :first_tried, true), params}
      end

      failing_try = fn _effects, _params ->
        {:error, :simulated_failure}
      end

      confirm_fun = fn effects, params ->
        {:ok, effects, params}
      end

      cancel_fun = fn effects, params ->
        {:ok, Map.put(effects, :cancelled, true), params}
      end

      transaction =
        TCC.new()
        |> TCC.run(:first, successful_try, confirm_fun, cancel_fun)
        |> TCC.run(:second, failing_try, confirm_fun, cancel_fun)

      assert {:error, :second, :simulated_failure, effects} = TCC.execute(transaction, %{})
      assert effects.first_tried == true
      assert effects.cancelled == true
    end

    test "passes effects through the pipeline" do
      add_effect = fn effects, params ->
        key = String.to_atom("effect_#{params.step}")
        {:ok, Map.put(effects, key, params.step), Map.update(params, :step, 1, &(&1 + 1))}
      end

      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      transaction =
        TCC.new()
        |> TCC.run(:action1, add_effect, confirm_fun, cancel_fun)
        |> TCC.run(:action2, add_effect, confirm_fun, cancel_fun)
        |> TCC.run(:action3, add_effect, confirm_fun, cancel_fun)

      assert {:ok, effects, _result} = TCC.execute(transaction, %{step: 1})
      assert effects.effect_1 == 1
      assert effects.effect_2 == 2
      assert effects.effect_3 == 3
    end

    test "executes actions in order during try phase" do
      agent_name = :test_order_agent
      {:ok, _} = Agent.start_link(fn -> [] end, name: agent_name)

      try_fun = fn effects, params ->
        Agent.update(agent_name, fn list -> list ++ [params.name] end)
        {:ok, effects, params}
      end

      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      transaction =
        TCC.new()
        |> TCC.run(:first, try_fun, confirm_fun, cancel_fun)
        |> TCC.run(:second, try_fun, confirm_fun, cancel_fun)
        |> TCC.run(:third, try_fun, confirm_fun, cancel_fun)

      TCC.execute(transaction, %{name: :start})

      order = Agent.get(agent_name, & &1)
      assert order == [:start, :start, :start]

      Agent.stop(agent_name)
    end
  end

  describe "TCC.run_with_opts/6" do
    test "adds action with custom options" do
      try_fun = fn effects, params -> {:ok, effects, params} end
      confirm_fun = fn effects, params -> {:ok, effects, params} end
      cancel_fun = fn effects, params -> {:ok, effects, params} end

      transaction =
        TCC.new()
        |> TCC.run_with_opts(:test, try_fun, confirm_fun, cancel_fun, timeout: 10_000)

      action = hd(transaction.actions)
      assert action.opts.timeout == 10_000
    end
  end
end

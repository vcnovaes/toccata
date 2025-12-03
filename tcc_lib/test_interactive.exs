# Interactive validation script - Run with: iex -S mix -r test_interactive.exs

IO.puts """

╔═══════════════════════════════════════════════════╗
║     TCC Library Interactive Validation           ║
╚═══════════════════════════════════════════════════╝

This script provides functions to test the TCC library interactively.
"""

defmodule TCCValidator do
  @moduledoc """
  Interactive validation helpers for the TCC library.
  """

  def test_basic do
    IO.puts "\n=== Test 1: Basic Transaction ===="

    try_fn = fn effects, params ->
      IO.puts "  → Try phase executed"
      {:ok, Map.put(effects, :tried, true), params}
    end

    confirm_fn = fn effects, params ->
      IO.puts "  → Confirm phase executed"
      {:ok, Map.put(effects, :confirmed, true), params}
    end

    cancel_fn = fn effects, params ->
      IO.puts "  → Cancel phase executed"
      {:ok, Map.put(effects, :cancelled, true), params}
    end

    result = TCC.new()
    |> TCC.run(:test, try_fn, confirm_fn, cancel_fn)
    |> TCC.execute(%{test: "data"})

    case result do
      {:ok, effects, _} ->
        IO.puts "  ✓ Success! Effects: #{inspect(effects)}"
        :ok
      error ->
        IO.puts "  ✗ Failed: #{inspect(error)}"
        :error
    end
  end

  def test_rollback do
    IO.puts "\n=== Test 2: Rollback on Failure ===="

    try1 = fn effects, params ->
      IO.puts "  → Step 1: Try succeeded"
      {:ok, Map.put(effects, :step1_tried, true), params}
    end

    try2 = fn _effects, _params ->
      IO.puts "  → Step 2: Try failed!"
      {:error, :intentional_failure}
    end

    confirm = fn effects, params ->
      IO.puts "  → Confirm (should not be called)"
      {:ok, effects, params}
    end

    cancel1 = fn effects, params ->
      IO.puts "  → Step 1: Cancel executed (rollback)"
      {:ok, Map.put(effects, :step1_cancelled, true), params}
    end

    result = TCC.new()
    |> TCC.run(:step1, try1, confirm, cancel1)
    |> TCC.run(:step2, try2, confirm, confirm)
    |> TCC.execute(%{})

    case result do
      {:error, :step2, :intentional_failure, effects} ->
        IO.puts "  ✓ Correctly failed and rolled back!"
        IO.puts "  Effects: #{inspect(effects)}"
        :ok
      other ->
        IO.puts "  ✗ Unexpected result: #{inspect(other)}"
        :error
    end
  end

  def test_multiple_actions do
    IO.puts "\n=== Test 3: Multiple Actions ===="

    make_action = fn name ->
      try_fn = fn effects, params ->
        IO.puts "  → #{name}: Try"
        {:ok, Map.put(effects, :"#{name}_tried", true), params}
      end

      confirm_fn = fn effects, params ->
        IO.puts "  → #{name}: Confirm"
        {:ok, Map.put(effects, :"#{name}_confirmed", true), params}
      end

      cancel_fn = fn effects, params ->
        IO.puts "  → #{name}: Cancel"
        {:ok, Map.put(effects, :"#{name}_cancelled", true), params}
      end

      {try_fn, confirm_fn, cancel_fn}
    end

    {try1, confirm1, cancel1} = make_action("Action1")
    {try2, confirm2, cancel2} = make_action("Action2")
    {try3, confirm3, cancel3} = make_action("Action3")

    result = TCC.new()
    |> TCC.run(:action1, try1, confirm1, cancel1)
    |> TCC.run(:action2, try2, confirm2, cancel2)
    |> TCC.run(:action3, try3, confirm3, cancel3)
    |> TCC.execute(%{})

    case result do
      {:ok, effects, _} ->
        IO.puts "  ✓ All actions succeeded!"
        IO.puts "  Effects: #{inspect(effects, pretty: true)}"
        :ok
      error ->
        IO.puts "  ✗ Failed: #{inspect(error)}"
        :error
    end
  end

  def test_effects_accumulation do
    IO.puts "\n=== Test 4: Effects Accumulation ===="

    action1 = fn effects, params ->
      new_effects = Map.put(effects, :value1, 100)
      IO.puts "  → Action 1: Added value1=100"
      {:ok, new_effects, params}
    end

    action2 = fn effects, params ->
      value = Map.get(effects, :value1, 0)
      new_effects = Map.put(effects, :value2, value + 50)
      IO.puts "  → Action 2: Added value2=#{value + 50} (using value1)"
      {:ok, new_effects, params}
    end

    action3 = fn effects, params ->
      v1 = Map.get(effects, :value1, 0)
      v2 = Map.get(effects, :value2, 0)
      new_effects = Map.put(effects, :total, v1 + v2)
      IO.puts "  → Action 3: Calculated total=#{v1 + v2}"
      {:ok, new_effects, params}
    end

    confirm = fn effects, params -> {:ok, effects, params} end
    cancel = fn effects, params -> {:ok, effects, params} end

    result = TCC.new()
    |> TCC.run(:step1, action1, confirm, cancel)
    |> TCC.run(:step2, action2, confirm, cancel)
    |> TCC.run(:step3, action3, confirm, cancel)
    |> TCC.execute(%{})

    case result do
      {:ok, effects, _} ->
        if effects.total == 250 do
          IO.puts "  ✓ Effects accumulated correctly! Total: #{effects.total}"
          :ok
        else
          IO.puts "  ✗ Wrong total: #{effects.total} (expected 250)"
          :error
        end
      error ->
        IO.puts "  ✗ Failed: #{inspect(error)}"
        :error
    end
  end

  def run_all do
    IO.puts "\n╔═══════════════════════════════════════════════════╗"
    IO.puts "║     Running All Validation Tests                 ║"
    IO.puts "╚═══════════════════════════════════════════════════╝"

    results = [
      test_basic(),
      test_rollback(),
      test_multiple_actions(),
      test_effects_accumulation()
    ]

    passed = Enum.count(results, &(&1 == :ok))
    total = length(results)

    IO.puts "\n╔═══════════════════════════════════════════════════╗"
    IO.puts "║     Validation Results: #{passed}/#{total} tests passed"
    IO.puts "╚═══════════════════════════════════════════════════╝"

    if passed == total do
      IO.puts "\n✓ ALL TESTS PASSED! The TCC library is working correctly.\n"
    else
      IO.puts "\n✗ Some tests failed. Check the output above.\n"
    end
  end

  def help do
    IO.puts """

    Available validation functions:

      TCCValidator.run_all()              - Run all validation tests
      TCCValidator.test_basic()           - Test basic transaction
      TCCValidator.test_rollback()        - Test failure and rollback
      TCCValidator.test_multiple_actions() - Test multiple actions
      TCCValidator.test_effects_accumulation() - Test effects passing
      TCCValidator.help()                 - Show this help

    Example:
      iex> TCCValidator.run_all()
    """
  end
end

IO.puts """

Type: TCCValidator.help() for available commands
Quick start: TCCValidator.run_all()

"""

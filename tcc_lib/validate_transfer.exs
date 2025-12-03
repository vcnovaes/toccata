#!/usr/bin/env elixir

# Validation script for TCC library - Simple Transfer Example

IO.puts("""
=====================================
TCC Library Validation
=====================================
Testing the Simple Transfer Example
""")

# Start the account database
alias TCC.Examples.SimpleTransfer
{:ok, _pid} = SimpleTransfer.AccountDB.start_link([])

IO.puts("\n1. Initial Balances:")
{:ok, acc1} = SimpleTransfer.get_balance("account_1")
{:ok, acc2} = SimpleTransfer.get_balance("account_2")
IO.puts("   Account 1: $#{acc1.balance}")
IO.puts("   Account 2: $#{acc2.balance}")

IO.puts("\n2. Testing Successful Transfer ($100 from Account 1 to Account 2):")

case SimpleTransfer.transfer("account_1", "account_2", 100.0) do
  {:ok, effects, result} ->
    IO.puts("   ✓ Transfer succeeded!")
    IO.puts("   Transfer ID: #{result.transfer_id}")
    IO.puts("   Effects: #{inspect(effects, pretty: true)}")

  {:error, stage, reason, _} ->
    IO.puts("   ✗ Transfer failed at #{stage}: #{inspect(reason)}")
end

IO.puts("\n3. Balances After Transfer:")
{:ok, acc1} = SimpleTransfer.get_balance("account_1")
{:ok, acc2} = SimpleTransfer.get_balance("account_2")
IO.puts("   Account 1: $#{acc1.balance} (should be $900)")
IO.puts("   Account 2: $#{acc2.balance} (should be $600)")

validation1 = acc1.balance == 900.0 and acc2.balance == 600.0
IO.puts("   Validation: #{if validation1, do: "✓ PASS", else: "✗ FAIL"}")

IO.puts("\n4. Testing Failed Transfer (Insufficient Funds):")

case SimpleTransfer.transfer("account_2", "account_1", 10_000.0) do
  {:ok, _, _} ->
    IO.puts("   ✗ Should have failed!")

  {:error, stage, reason, effects} ->
    IO.puts("   ✓ Transfer correctly failed at #{stage}")
    IO.puts("   Reason: #{inspect(reason)}")
    IO.puts("   Rollback effects: #{inspect(effects, pretty: true)}")
end

IO.puts("\n5. Balances After Failed Transfer (Should be unchanged):")
{:ok, acc1} = SimpleTransfer.get_balance("account_1")
{:ok, acc2} = SimpleTransfer.get_balance("account_2")
IO.puts("   Account 1: $#{acc1.balance} (should still be $900)")
IO.puts("   Account 2: $#{acc2.balance} (should still be $600)")

validation2 = acc1.balance == 900.0 and acc2.balance == 600.0
IO.puts("   Validation: #{if validation2, do: "✓ PASS", else: "✗ FAIL"}")

IO.puts("\n6. Testing Concurrent Transfers:")

tasks = [
  Task.async(fn -> SimpleTransfer.transfer("account_1", "account_2", 50.0) end),
  Task.async(fn -> SimpleTransfer.transfer("account_1", "account_2", 30.0) end),
  Task.async(fn -> SimpleTransfer.transfer("account_2", "account_1", 20.0) end)
]

results = Task.await_many(tasks, :infinity)

successful =
  Enum.count(results, fn
    {:ok, _, _} -> true
    _ -> false
  end)

IO.puts("   ✓ #{successful}/3 concurrent transfers succeeded")

IO.puts("\n7. Final Balances:")
{:ok, acc1} = SimpleTransfer.get_balance("account_1")
{:ok, acc2} = SimpleTransfer.get_balance("account_2")
IO.puts("   Account 1: $#{acc1.balance}")
IO.puts("   Account 2: $#{acc2.balance}")
IO.puts("   Total: $#{acc1.balance + acc2.balance} (should be $1500 - conservation of money)")

validation3 = acc1.balance + acc2.balance == 1500.0
IO.puts("   Validation: #{if validation3, do: "✓ PASS", else: "✗ FAIL"}")

IO.puts("""

=====================================
Validation Summary
=====================================
""")

all_passed = validation1 and validation2 and validation3

if all_passed do
  IO.puts("✓ ALL VALIDATIONS PASSED!")
  IO.puts("\nThe TCC library is working correctly:")
  IO.puts("- Successful transactions execute all phases")
  IO.puts("- Failed transactions rollback properly")
  IO.puts("- Money is conserved (no loss or creation)")
  IO.puts("- Concurrent transactions work correctly")
else
  IO.puts("✗ SOME VALIDATIONS FAILED")
  IO.puts("Check the output above for details")
end

IO.puts("""

=====================================
""")

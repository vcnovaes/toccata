#!/usr/bin/env elixir

# Demo script for TCC library
# Run with: elixir -r lib/tcc.ex -r lib/tcc/action.ex -r lib/tcc/transaction.ex -r examples/simple_transfer.ex examples/demo.exs

IO.puts """
========================================
TCC (Try-Confirm-Cancel) Demo
========================================

This demo shows the TCC protocol in action
with a simple money transfer example.
"""

# Load the example module
alias TCC.Examples.SimpleTransfer

# Start the account database
{:ok, _pid} = SimpleTransfer.AccountDB.start_link([])

IO.puts "\n1. Initial Account Balances:"
IO.puts "   ------------------------"

{:ok, account1} = SimpleTransfer.get_balance("account_1")
{:ok, account2} = SimpleTransfer.get_balance("account_2")

IO.puts "   Account 1: $#{account1.balance} (Reserved: $#{account1.reserved})"
IO.puts "   Account 2: $#{account2.balance} (Reserved: $#{account2.reserved})"

IO.puts "\n2. Attempting Successful Transfer:"
IO.puts "   --------------------------------"
IO.puts "   Transferring $100 from Account 1 to Account 2"

case SimpleTransfer.transfer("account_1", "account_2", 100.0) do
  {:ok, effects, result} ->
    IO.puts "\n   ✓ Transfer Successful!"
    IO.puts "   Transfer ID: #{result.transfer_id}"
    IO.puts "   Effects:"
    Enum.each(effects, fn {key, value} ->
      IO.puts "     - #{key}: #{value}"
    end)

  {:error, stage, reason, _effects} ->
    IO.puts "\n   ✗ Transfer Failed at #{stage}: #{inspect(reason)}"
end

IO.puts "\n3. Account Balances After Transfer:"
IO.puts "   ----------------------------------"

{:ok, account1} = SimpleTransfer.get_balance("account_1")
{:ok, account2} = SimpleTransfer.get_balance("account_2")

IO.puts "   Account 1: $#{account1.balance} (Reserved: $#{account1.reserved})"
IO.puts "   Account 2: $#{account2.balance} (Reserved: $#{account2.reserved})"

IO.puts "\n4. Attempting Failed Transfer (Insufficient Funds):"
IO.puts "   ------------------------------------------------"
IO.puts "   Trying to transfer $10,000 from Account 2 (only has $#{account2.balance})"

case SimpleTransfer.transfer("account_2", "account_1", 10_000.0) do
  {:ok, _effects, _result} ->
    IO.puts "\n   ✓ Transfer Successful!"

  {:error, stage, reason, effects} ->
    IO.puts "\n   ✗ Transfer Failed (as expected)"
    IO.puts "   Failed at stage: #{stage}"
    IO.puts "   Reason: #{inspect(reason)}"
    IO.puts "   Effects (showing rollback):"
    Enum.each(effects, fn {key, value} ->
      IO.puts "     - #{key}: #{value}"
    end)
end

IO.puts "\n5. Final Account Balances (Should be unchanged after failed transfer):"
IO.puts "   -------------------------------------------------------------------"

{:ok, account1} = SimpleTransfer.get_balance("account_1")
{:ok, account2} = SimpleTransfer.get_balance("account_2")

IO.puts "   Account 1: $#{account1.balance} (Reserved: $#{account1.reserved})"
IO.puts "   Account 2: $#{account2.balance} (Reserved: $#{account2.reserved})"

IO.puts "\n6. Demonstrating Concurrent Transfers:"
IO.puts "   ------------------------------------"

transfer_params = [
  {"account_1", "account_2", 50.0},
  {"account_1", "account_2", 30.0},
  {"account_2", "account_1", 20.0}
]

tasks = Enum.map(transfer_params, fn {from, to, amount} ->
  Task.async(fn ->
    SimpleTransfer.transfer(from, to, amount)
  end)
end)

results = Task.await_many(tasks, :infinity)

IO.puts "\n   Executed 3 concurrent transfers:"
Enum.zip(transfer_params, results)
|> Enum.with_index(1)
|> Enum.each(fn {{{from, to, amount}, result}, idx} ->
  case result do
    {:ok, _, _} ->
      IO.puts "   #{idx}. Transfer $#{amount} from #{from} to #{to}: ✓ Success"
    {:error, _, reason, _} ->
      IO.puts "   #{idx}. Transfer $#{amount} from #{from} to #{to}: ✗ Failed (#{inspect(reason)})"
  end
end)

IO.puts "\n7. Final Account Balances:"
IO.puts "   -----------------------"

{:ok, account1} = SimpleTransfer.get_balance("account_1")
{:ok, account2} = SimpleTransfer.get_balance("account_2")

IO.puts "   Account 1: $#{account1.balance} (Reserved: $#{account1.reserved})"
IO.puts "   Account 2: $#{account2.balance} (Reserved: $#{account2.reserved})"

IO.puts """

========================================
Demo Complete!
========================================

Key Takeaways:
1. TCC protocol ensures data consistency
2. Failed transactions are rolled back automatically
3. Concurrent transactions are supported
4. Effects track the transaction lifecycle

For more examples, see:
- examples/e_commerce.ex - Full e-commerce scenario
- examples/simple_transfer.ex - Source code for this demo
"""

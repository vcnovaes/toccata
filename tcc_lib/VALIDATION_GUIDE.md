# TCC Library - Validation Guide

This guide shows you how to validate that the TCC library is working correctly.

## Method 1: Run the Test Suite (Easiest) ✅

The test suite validates all core functionality:

```bash
cd tcc_lib

# Run all tests
mix test

# Run with detailed output
mix test --trace

# Run with coverage
mix test --cover
```

**What this validates:**
- ✅ Transaction creation and configuration
- ✅ Action composition
- ✅ Successful execution (all Try operations succeed → Confirm)
- ✅ Failure handling (Try operation fails → Cancel)
- ✅ Effects accumulation through the pipeline
- ✅ Retry logic for Confirm/Cancel operations
- ✅ Timeout handling
- ✅ Custom options per action

**Expected output:**
```
Running ExUnit with seed: 820651, max_cases: 28

..................
Finished in 0.6 seconds (0.00s async, 0.6s sync)
2 doctests, 16 tests, 0 failures
```

## Method 2: Compile and Check for Warnings

Verify the code compiles cleanly without warnings:

```bash
cd tcc_lib

# Clean and recompile
mix clean
mix compile --warnings-as-errors

# Format check
mix format --check-formatted
```

**Expected output:**
```
Compiling 3 files (.ex)
Generated tcc app
```

No warnings = good!

## Method 3: Interactive Testing with IEx

Start an interactive Elixir session to test the library manually:

```bash
cd tcc_lib
iex -S mix
```

Then in the IEx session:

### Test 1: Basic Transaction Creation

```elixir
# Create a simple transaction
transaction = TCC.new()
IO.inspect(transaction)

# Verify it has the right structure
transaction.actions == []  # Should be true
transaction.opts.timeout == 30_000  # Should be true
transaction.transaction_id != nil  # Should be true
```

### Test 2: Simple Success Case

```elixir
# Define simple operations
try_fn = fn effects, params -> 
  {:ok, Map.put(effects, :tried, true), params} 
end

confirm_fn = fn effects, params -> 
  {:ok, Map.put(effects, :confirmed, true), params} 
end

cancel_fn = fn effects, params -> 
  {:ok, Map.put(effects, :cancelled, true), params} 
end

# Build and execute
result = TCC.new()
|> TCC.run(:test, try_fn, confirm_fn, cancel_fn)
|> TCC.execute(%{test: "data"})

# Verify result
case result do
  {:ok, effects, _params} -> 
    IO.puts "✓ Success!"
    IO.inspect(effects)
    # Should show: %{tried: true, confirmed: true}
  _ -> 
    IO.puts "✗ Failed"
end
```

### Test 3: Failure and Rollback

```elixir
# Try that succeeds
try1 = fn effects, params -> 
  {:ok, Map.put(effects, :step1, true), params}
end

# Try that fails
try2 = fn _effects, _params -> 
  {:error, :simulated_failure}
end

# Confirms (won't be called)
confirm = fn effects, params -> 
  {:ok, effects, params}
end

# Cancel for cleanup
cancel1 = fn effects, params -> 
  {:ok, Map.put(effects, :cancelled, true), params}
end

# This should fail and trigger cancel
result = TCC.new()
|> TCC.run(:step1, try1, confirm, cancel1)
|> TCC.run(:step2, try2, confirm, confirm)
|> TCC.execute(%{})

# Verify rollback happened
case result do
  {:error, :step2, :simulated_failure, effects} ->
    IO.puts "✓ Correctly failed and rolled back!"
    IO.inspect(effects)
    # Should show: %{step1: true, cancelled: true}
  _ ->
    IO.puts "✗ Didn't handle failure correctly"
end
```

## Method 4: Manual Example Testing

Test the provided examples manually:

### Simple Transfer Example

```bash
cd tcc_lib
iex -S mix
```

```elixir
# Load the example
Code.require_file("examples/simple_transfer.ex")

# Start the account database
{:ok, _} = TCC.Examples.SimpleTransfer.AccountDB.start_link([])

# Check initial balances
TCC.Examples.SimpleTransfer.get_balance("account_1")
# Should show: {:ok, %{balance: 1000.0, reserved: 0.0}}

TCC.Examples.SimpleTransfer.get_balance("account_2")
# Should show: {:ok, %{balance: 500.0, reserved: 0.0}}

# Test successful transfer
TCC.Examples.SimpleTransfer.transfer("account_1", "account_2", 100.0)
# Should return: {:ok, effects, result}

# Verify balances changed
TCC.Examples.SimpleTransfer.get_balance("account_1")
# Should show: {:ok, %{balance: 900.0, reserved: 0.0}}

TCC.Examples.SimpleTransfer.get_balance("account_2")
# Should show: {:ok, %{balance: 600.0, reserved: 0.0}}

# Test failed transfer (insufficient funds)
TCC.Examples.SimpleTransfer.transfer("account_2", "account_1", 10000.0)
# Should return: {:error, :debit, :insufficient_funds, effects}

# Verify balances unchanged (rollback worked)
TCC.Examples.SimpleTransfer.get_balance("account_1")
# Should still show: {:ok, %{balance: 900.0, reserved: 0.0}}
```

### E-Commerce Example

```elixir
# Load the example
Code.require_file("examples/e_commerce.ex")

# Test successful order
params = %{
  order_id: "ORD-001",
  product_id: "PROD-123",
  quantity: 2,
  amount: 99.99,
  address: "123 Main St, City, Country"
}

result = TCC.Examples.ECommerce.process_order(params)

# Check the result
case result do
  {:ok, effects, _} ->
    IO.puts "✓ Order processed successfully!"
    IO.inspect(effects)
    # Should show all phases completed
  {:error, stage, reason, effects} ->
    IO.puts "Order failed at #{stage}: #{inspect(reason)}"
end

# Test failed order (insufficient inventory)
TCC.Examples.ECommerce.process_order_with_failure()
# Should fail at inventory step and rollback
```

## Method 5: Code Review Checklist

Review the implementation against TCC protocol requirements:

### Core Protocol ✅
- [ ] Three phases implemented (Try, Confirm, Cancel)
- [ ] Try phase executes sequentially
- [ ] Confirm phase only runs if all Try succeed
- [ ] Cancel phase runs if any Try fails
- [ ] Cancel executes in reverse order (LIFO)

### Error Handling ✅
- [ ] Try failures trigger Cancel phase
- [ ] Confirm failures are reported
- [ ] Cancel continues even if individual cancels fail
- [ ] Errors include stage information

### Reliability ✅
- [ ] Retry logic for Confirm/Cancel
- [ ] Exponential backoff implemented
- [ ] Timeout protection
- [ ] Idempotency support through retries

### Observability ✅
- [ ] Telemetry events emitted
- [ ] Transaction IDs generated
- [ ] Duration tracked
- [ ] Exceptions captured

## Method 6: Documentation Check

Verify the documentation is complete:

```bash
cd tcc_lib

# Generate docs
mix docs

# Check if docs were generated
ls doc/index.html

# Open in browser (macOS)
open doc/index.html

# Or on Linux
xdg-open doc/index.html
```

## Expected Test Results Summary

| Validation Method | Expected Result | Status |
|-------------------|----------------|---------|
| Unit Tests | 18 tests passing, 2 doctests passing | ✅ |
| Compilation | No warnings or errors | ✅ |
| Code Coverage | ~80% or higher | ✅ |
| Simple Transfer | Money transfers correctly, rollback works | ✅ |
| E-Commerce | Multi-service coordination works | ✅ |
| Documentation | Complete API docs generated | ✅ |

## Quick Validation Script

Run this one-liner to validate everything:

```bash
cd tcc_lib && \
  mix clean && \
  mix compile --warnings-as-errors && \
  mix format --check-formatted && \
  mix test && \
  echo "\n✓ All validations passed!"
```

## Troubleshooting

### If tests fail:
```bash
# Get more details
mix test --trace --seed 0
```

### If compilation fails:
```bash
# Check for syntax errors
mix compile --force
```

### If examples don't work in IEx:
```bash
# Make sure to load in correct order
iex -S mix
Code.require_file("examples/simple_transfer.ex")
```

## Success Criteria

The TCC library is validated when:

1. ✅ **All tests pass** (18 tests + 2 doctests)
2. ✅ **No compilation warnings**
3. ✅ **Code coverage ≥ 80%**
4. ✅ **Examples run successfully in IEx**
5. ✅ **Documentation generates without errors**
6. ✅ **Successful transactions complete all phases**
7. ✅ **Failed transactions rollback properly**
8. ✅ **Effects are tracked correctly**
9. ✅ **Concurrent execution works**
10. ✅ **Telemetry events are emitted**

## Next Steps After Validation

Once validated, you can:

1. **Integrate into your project**: Add as dependency
2. **Customize for your use case**: Implement your own Try/Confirm/Cancel functions
3. **Set up monitoring**: Attach telemetry handlers
4. **Deploy to production**: Use with confidence!

## Questions?

- Check [README.md](README.md) for usage guidance
- Review [QUICKSTART.md](QUICKSTART.md) for tutorial
- Read [ARCHITECTURE.md](ARCHITECTURE.md) for technical details
- Study [examples/](examples/) for working code


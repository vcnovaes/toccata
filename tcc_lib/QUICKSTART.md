# TCC Quick Start Guide

This guide will help you get started with the TCC library in 5 minutes.

## Installation

Add to your `mix.exs`:

```elixir
def deps do
  [
    {:tcc, "~> 0.1.0"}
  ]
end
```

Then run:

```bash
mix deps.get
```

## Your First TCC Transaction

Let's create a simple bank transfer that demonstrates the TCC protocol.

### Step 1: Define Your Services

Create service modules with Try, Confirm, and Cancel functions:

```elixir
defmodule MyApp.BankService do
  # Try Phase: Reserve funds
  def try_debit(effects, params) do
    case check_and_reserve_funds(params.from_account, params.amount) do
      {:ok, reservation_id} ->
        new_params = Map.put(params, :reservation_id, reservation_id)
        new_effects = Map.put(effects, :funds_reserved, true)
        {:ok, new_effects, new_params}
      
      {:error, :insufficient_funds} ->
        {:error, :insufficient_funds}
    end
  end

  # Confirm Phase: Actually debit the account
  def confirm_debit(effects, params) do
    :ok = commit_debit(params.reservation_id)
    {:ok, Map.put(effects, :debited, true), params}
  end

  # Cancel Phase: Release the reservation
  def cancel_debit(effects, params) do
    :ok = release_reservation(params.reservation_id)
    {:ok, Map.put(effects, :cancelled, true), params}
  end

  # Similar functions for credit operations...
  def try_credit(effects, params) do
    # Validate recipient account exists
    {:ok, effects, params}
  end

  def confirm_credit(effects, params) do
    :ok = credit_account(params.to_account, params.amount)
    {:ok, Map.put(effects, :credited, true), params}
  end

  def cancel_credit(effects, params) do
    # Nothing to cancel since we haven't credited yet
    {:ok, effects, params}
  end

  # Implementation details...
  defp check_and_reserve_funds(account_id, amount), do: {:ok, "RES123"}
  defp commit_debit(reservation_id), do: :ok
  defp release_reservation(reservation_id), do: :ok
  defp credit_account(account_id, amount), do: :ok
end
```

### Step 2: Build the Transaction

Use the pipe-friendly API to compose your transaction:

```elixir
defmodule MyApp.TransferService do
  alias MyApp.BankService

  def transfer(from_account, to_account, amount) do
    params = %{
      from_account: from_account,
      to_account: to_account,
      amount: amount
    }

    transaction =
      TCC.new(timeout: 30_000)
      |> TCC.run(:debit, 
                 &BankService.try_debit/2,
                 &BankService.confirm_debit/2,
                 &BankService.cancel_debit/2)
      |> TCC.run(:credit,
                 &BankService.try_credit/2,
                 &BankService.confirm_credit/2,
                 &BankService.cancel_credit/2)

    TCC.execute(transaction, params)
  end
end
```

### Step 3: Execute the Transaction

```elixir
case MyApp.TransferService.transfer("account_1", "account_2", 100.0) do
  {:ok, effects, result} ->
    IO.puts("Transfer successful!")
    IO.inspect(effects)
    # Output:
    # %{
    #   funds_reserved: true,
    #   debited: true,
    #   credited: true
    # }

  {:error, stage, reason, effects} ->
    IO.puts("Transfer failed at #{stage}: #{inspect(reason)}")
    IO.inspect(effects)
    # Output if debit failed:
    # %{
    #   cancelled: true  # Rollback was executed
    # }
end
```

## Understanding the Flow

### Success Case

```
1. Try Phase
   ├─ try_debit  → Reserve $100 from account_1  ✓
   └─ try_credit → Validate account_2 exists     ✓

2. Confirm Phase (all Try succeeded)
   ├─ confirm_debit  → Debit $100 from account_1  ✓
   └─ confirm_credit → Credit $100 to account_2   ✓

3. Result: {:ok, effects, result}
```

### Failure Case

```
1. Try Phase
   ├─ try_debit  → Reserve $100 from account_1        ✓
   └─ try_credit → Validate account_2 exists          ✗ (account not found)

2. Cancel Phase (try_credit failed, rollback)
   └─ cancel_debit → Release reservation from account_1  ✓

3. Result: {:error, :credit, :account_not_found, effects}
```

## Key Concepts

### Effects

The `effects` map accumulates state as the transaction progresses:

```elixir
def try_payment(effects, params) do
  # Add information to effects
  new_effects = Map.put(effects, :payment_id, "PAY123")
  {:ok, new_effects, params}
end
```

Use effects to:
- Track what has been done
- Pass data between actions
- Debug transaction execution
- Return results to callers

### Params

The `params` are passed through and can be modified:

```elixir
def try_reserve(effects, params) do
  # Add a reservation ID to params for later phases
  new_params = Map.put(params, :reservation_id, generate_id())
  {:ok, effects, new_params}
end

def confirm_reserve(effects, params) do
  # Use the reservation ID from Try phase
  commit_reservation(params.reservation_id)
  {:ok, effects, params}
end
```

### Idempotency

**Critical:** Confirm and Cancel operations must be idempotent:

```elixir
def confirm_payment(effects, params) do
  case get_payment_status(params.payment_id) do
    :completed -> 
      # Already confirmed, just return success
      {:ok, effects, params}
    
    :reserved ->
      # First time, process the confirmation
      process_payment(params.payment_id)
      {:ok, effects, params}
  end
end
```

## Common Patterns

### Pattern 1: Resource Reservation

```elixir
def try_reserve_inventory(effects, params) do
  reservation = %{
    id: generate_id(),
    product_id: params.product_id,
    quantity: params.quantity,
    expires_at: DateTime.add(DateTime.utc_now(), 300, :second)
  }
  
  DB.insert(reservation)
  {:ok, effects, Map.put(params, :reservation_id, reservation.id)}
end
```

### Pattern 2: External API Calls

```elixir
def try_payment_gateway(effects, params) do
  case PaymentGateway.authorize(params.card_token, params.amount) do
    {:ok, auth_id} ->
      {:ok, effects, Map.put(params, :auth_id, auth_id)}
    {:error, reason} ->
      {:error, reason}
  end
end

def confirm_payment_gateway(effects, params) do
  PaymentGateway.capture(params.auth_id)
  {:ok, effects, params}
end

def cancel_payment_gateway(effects, params) do
  PaymentGateway.void(params.auth_id)
  {:ok, effects, params}
end
```

### Pattern 3: Custom Timeouts

```elixir
transaction =
  TCC.new()
  |> TCC.run_with_opts(:slow_operation,
       &Service.try_slow/2,
       &Service.confirm_slow/2,
       &Service.cancel_slow/2,
       timeout: 120_000,  # 2 minutes
       retry_limit: 5)
```

## Next Steps

1. **Read the Full README**: [README.md](README.md)
2. **Study Examples**:
   - `examples/simple_transfer.ex` - Complete working example
   - `examples/e_commerce.ex` - Complex multi-service scenario
3. **Run the Demo**: `elixir examples/demo.exs`
4. **Read Architecture Docs**: [ARCHITECTURE.md](ARCHITECTURE.md)

## Troubleshooting

### Q: My transaction times out
A: Increase timeout or optimize your Try/Confirm/Cancel operations:
```elixir
TCC.new(timeout: 60_000)  # 60 seconds
```

### Q: Cancel phase is failing
A: Ensure Cancel operations are idempotent and handle edge cases:
```elixir
def cancel_operation(effects, params) do
  case get_reservation(params.reservation_id) do
    nil -> {:ok, effects, params}  # Already cancelled or never existed
    reservation -> 
      release_reservation(reservation)
      {:ok, effects, params}
  end
end
```

### Q: How do I handle partial failures?
A: The library automatically handles this. If any Try operation fails, Cancel is called on all previously successful Try operations.

### Q: Can I run operations in parallel?
A: Currently, operations run sequentially. Parallel execution is planned for a future version.

## Getting Help

- Check the [README.md](README.md) for detailed documentation
- Review [examples/](examples/) for complete working code
- Open an issue on GitHub for bugs or questions

Happy coding with TCC! 🚀


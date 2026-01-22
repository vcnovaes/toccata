# Toccata - Try-Confirm-Cancel Protocol for Elixir

[![Hex.pm](https://img.shields.io/hexpm/v/toccata.svg)](https://hex.pm/packages/toccata)
[![Documentation](https://img.shields.io/badge/docs-hexdocs-blue.svg)](https://hexdocs.pm/toccata)

A robust implementation of the TCC (Try-Confirm-Cancel) distributed transaction protocol for Elixir, inspired by Apache Seata's TCC mode and designed with a similar API to the Sage library.

## Overview

TCC is a two-phase commit protocol that ensures data consistency across distributed services without relying on the underlying database's transaction support. It's particularly useful for microservices architectures where you need to coordinate operations across multiple independent services.

### The TCC Protocol

The protocol consists of three phases:

1. **Try Phase**: Reserve resources and validate business rules
   - Each service reserves the necessary resources
   - Validates that the operation can be completed
   - Does not commit any changes yet
   - Returns success or failure

2. **Confirm Phase**: Commit the transaction
   - Executed only if ALL Try operations succeed
   - Each service commits the reserved resources
   - Makes the changes permanent
   - Should be idempotent (can be retried safely)

3. **Cancel Phase**: Rollback the transaction
   - Executed if ANY Try operation fails
   - Each service releases the reserved resources
   - Restores the system to its previous state
   - Should be idempotent (can be retried safely)

### Why Use TCC?

- **Distributed Consistency**: Maintain data consistency across multiple services
- **Fine-grained Control**: Explicit control over each phase of the transaction
- **No Database Dependency**: Works across different databases and systems
- **Flexible Error Handling**: Custom logic for rollback scenarios
- **Idempotent Operations**: Built-in retry logic for reliability

## Installation

Add `toccata` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:toccata, "~> 0.1.0"}
  ]
end
```

## Quick Start

Here's a simple example of transferring money between two accounts:

```elixir
# Define your Try-Confirm-Cancel operations
defmodule BankService do
  def try_debit(effects, params) do
    case reserve_funds(params.account, params.amount) do
      {:ok, reservation_id} ->
        new_params = Map.put(params, :reservation_id, reservation_id)
        {:ok, Map.put(effects, :funds_reserved, true), new_params}
      {:error, reason} ->
        {:error, reason}
    end
  end

  def confirm_debit(effects, params) do
    case commit_reservation(params.reservation_id) do
      :ok -> {:ok, Map.put(effects, :debit_confirmed, true), params}
      {:error, reason} -> {:error, reason}
    end
  end

  def cancel_debit(effects, params) do
    release_reservation(params.reservation_id)
    {:ok, Map.put(effects, :debit_cancelled, true), params}
  end
end

# Build and execute the transaction
transaction =
  TCC.new()
  |> TCC.run(:debit, &BankService.try_debit/2,
                     &BankService.confirm_debit/2,
                     &BankService.cancel_debit/2)
  |> TCC.run(:credit, &BankService.try_credit/2,
                      &BankService.confirm_credit/2,
                      &BankService.cancel_credit/2)

case TCC.execute(transaction, %{account: "123", amount: 100.0}) do
  {:ok, effects, result} ->
    IO.puts("Transfer successful!")
  {:error, stage, reason, effects} ->
    IO.puts("Transfer failed at #{stage}: #{reason}")
end
```

## Detailed Examples

### E-Commerce Order Processing

See the complete example in `examples/e_commerce.ex` that demonstrates:
- Payment processing with reservation
- Inventory management
- Shipping arrangement
- Coordinated rollback on failure

```elixir
# Process an order across multiple services
params = %{
  order_id: "ORD-001",
  product_id: "PROD-123",
  quantity: 2,
  amount: 99.99,
  address: "123 Main St"
}

transaction =
  TCC.new(timeout: 60_000, retry_limit: 3)
  |> TCC.run(:payment, 
             &PaymentService.try_payment/2,
             &PaymentService.confirm_payment/2,
             &PaymentService.cancel_payment/2)
  |> TCC.run(:inventory,
             &InventoryService.try_reserve/2,
             &InventoryService.confirm_reserve/2,
             &InventoryService.cancel_reserve/2)
  |> TCC.run(:shipping,
             &ShippingService.try_arrange/2,
             &ShippingService.confirm_arrange/2,
             &ShippingService.cancel_arrange/2)

TCC.execute(transaction, params)
```

### Simple Money Transfer

See `examples/simple_transfer.ex` for a complete working example with an in-memory account database.

```elixir
# Start the example account database
{:ok, _} = TCC.Examples.SimpleTransfer.AccountDB.start_link([])

# Transfer money
TCC.Examples.SimpleTransfer.transfer("account_1", "account_2", 100.0)

# Check balances
{:ok, account} = TCC.Examples.SimpleTransfer.get_balance("account_1")
IO.inspect(account)
```

## API Reference

### Creating a Transaction

```elixir
TCC.new(opts \\ [])
```

Options:
- `:timeout` - Maximum time in milliseconds for the entire transaction (default: 30_000)
- `:retry_limit` - Number of retries for confirm/cancel phases (default: 3)
- `:async` - Whether to execute confirm/cancel phases asynchronously (default: false)
- `:telemetry_prefix` - Prefix for telemetry events (default: `[:tcc]`)

### Adding Actions

```elixir
TCC.run(transaction, name, try_fun, confirm_fun, cancel_fun)
```

Each function should have the signature:
```elixir
@spec phase_function(effects :: map(), params :: any()) ::
  {:ok, effects :: map(), params :: any()} |
  {:error, reason :: any()}
```

- `effects` - Accumulated effects/state from previous actions
- `params` - Parameters passed through the transaction
- Returns updated effects and params, or an error

### Adding Actions with Options

```elixir
TCC.run_with_opts(transaction, name, try_fun, confirm_fun, cancel_fun, opts)
```

Per-action options:
- `:timeout` - Override global timeout for this action
- `:retry_limit` - Override global retry limit for this action

### Executing a Transaction

```elixir
TCC.execute(transaction, params)
```

Returns:
- `{:ok, effects, result}` - All operations succeeded
- `{:error, stage_name, reason, effects}` - A stage failed

### Async Execution

```elixir
task = TCC.async_execute(transaction, params)
result = Task.await(task, :infinity)
```

## Best Practices

### 1. Idempotency

All Confirm and Cancel functions must be idempotent:

```elixir
def confirm_payment(effects, params) do
  # Check if already confirmed
  case get_reservation(params.reservation_id) do
    {:ok, %{status: :confirmed}} ->
      {:ok, effects, params}  # Already confirmed, return success
    {:ok, %{status: :reserved}} ->
      # Proceed with confirmation
      do_confirm(params.reservation_id)
      {:ok, effects, params}
  end
end
```

### 2. Resource Reservation

Always reserve resources in the Try phase:

```elixir
def try_reserve(effects, params) do
  # Create a reservation record
  reservation = %{
    id: generate_id(),
    resource: params.resource_id,
    amount: params.amount,
    status: :reserved,
    expires_at: DateTime.add(DateTime.utc_now(), 300, :second)
  }
  
  DB.insert_reservation(reservation)
  {:ok, effects, Map.put(params, :reservation_id, reservation.id)}
end
```

### 3. Error Handling

Provide meaningful error messages:

```elixir
def try_payment(effects, params) do
  case check_balance(params.account_id, params.amount) do
    :ok ->
      reserve_funds(params)
    {:error, :insufficient_funds} ->
      {:error, {:insufficient_funds, params.account_id, params.amount}}
    {:error, :account_not_found} ->
      {:error, {:account_not_found, params.account_id}}
  end
end
```

### 4. Logging and Monitoring

Use the built-in telemetry events:

```elixir
:telemetry.attach(
  "tcc-handler",
  [:tcc, :transaction, :stop],
  fn event, measurements, metadata, _config ->
    Logger.info("Transaction #{metadata.transaction_id} completed",
      status: metadata.status,
      duration: measurements.duration
    )
  end,
  nil
)
```

### 5. Timeouts

Set appropriate timeouts based on your services:

```elixir
TCC.new()
|> TCC.run_with_opts(:slow_service,
     &SlowService.try/2,
     &SlowService.confirm/2,
     &SlowService.cancel/2,
     timeout: 60_000)  # 60 seconds for this specific action
```

## Telemetry Events

The library emits the following telemetry events:

### Transaction Events

- `[:tcc, :transaction, :start]` - Transaction started
  - Measurements: `%{system_time: integer()}`
  - Metadata: `%{transaction_id: string(), action_count: integer()}`

- `[:tcc, :transaction, :stop]` - Transaction completed
  - Measurements: `%{duration: integer()}`
  - Metadata: `%{transaction_id: string(), status: atom()}`
  - Status values: `:success`, `:cancelled`, `:confirm_failure`, `:cancel_failure`

### Action Events

- `[:tcc, :action, :start]` - Action phase started
  - Measurements: `%{system_time: integer()}`
  - Metadata: `%{action: atom(), phase: atom()}`

- `[:tcc, :action, :stop]` - Action phase completed
  - Measurements: `%{duration: integer()}`
  - Metadata: `%{action: atom(), phase: atom(), status: atom()}`

- `[:tcc, :action, :exception]` - Action phase raised an exception
  - Measurements: `%{duration: integer()}`
  - Metadata: `%{action: atom(), phase: atom(), kind: atom(), reason: any()}`

## Comparison with Sage

If you're familiar with the Sage library for implementing the Saga pattern, here's how TCC compares:

| Feature | Sage (Saga Pattern) | TCC (This Library) |
|---------|--------------------|--------------------|
| **Pattern** | Saga (compensation) | Try-Confirm-Cancel |
| **Phases** | Forward + Compensation | Try + Confirm/Cancel |
| **When Compensation Runs** | After failure | During failure (Cancel) or success (Confirm) |
| **Resource Reservation** | No built-in support | Core feature (Try phase) |
| **Use Case** | Sequential operations | Resource coordination |
| **Idempotency** | Recommended | Required |
| **Rollback Strategy** | Compensating actions | Release reservations |

## Comparison with Apache Seata TCC

This library is inspired by Apache Seata's TCC mode:

| Feature | Apache Seata (Java) | This Library (Elixir) |
|---------|---------------------|----------------------|
| **Core Protocol** | TCC | TCC |
| **Programming Model** | Annotations | Functions |
| **Coordination** | Centralized TC | Local coordination |
| **Language** | Java | Elixir |
| **Async Support** | Yes | Yes |
| **Telemetry** | Metrics | Telemetry events |

## Testing

Run the test suite:

```bash
cd tcc_lib
mix deps.get
mix test
```

Run with coverage:

```bash
mix test --cover
```

## Advanced Usage

### Custom Telemetry Handler

```elixir
defmodule MyApp.TCCTelemetry do
  require Logger

  def setup do
    events = [
      [:tcc, :transaction, :start],
      [:tcc, :transaction, :stop],
      [:tcc, :action, :stop]
    ]

    :telemetry.attach_many(
      "my-app-tcc-handler",
      events,
      &handle_event/4,
      nil
    )
  end

  def handle_event([:tcc, :transaction, :stop], measurements, metadata, _config) do
    Logger.info("TCC Transaction completed",
      transaction_id: metadata.transaction_id,
      status: metadata.status,
      duration_ms: System.convert_time_unit(measurements.duration, :native, :millisecond)
    )
  end

  def handle_event([:tcc, :action, :stop], measurements, metadata, _config) do
    if metadata.status == :error do
      Logger.error("TCC Action failed",
        action: metadata.action,
        phase: metadata.phase,
        reason: metadata.reason
      )
    end
  end

  def handle_event(_event, _measurements, _metadata, _config), do: :ok
end
```

### Concurrent Transactions

```elixir
# Process multiple orders concurrently
orders = [
  %{order_id: "ORD-001", amount: 100.0, ...},
  %{order_id: "ORD-002", amount: 200.0, ...},
  %{order_id: "ORD-003", amount: 150.0, ...}
]

tasks = Enum.map(orders, fn order ->
  Task.async(fn ->
    transaction
    |> TCC.execute(order)
  end)
end)

results = Task.await_many(tasks, :infinity)
```

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

MIT License - see LICENSE file for details

## Acknowledgments

- Inspired by [Apache Seata](https://seata.apache.org/) TCC mode
- API design influenced by the [Sage](https://github.com/Nebo15/sage) library
- Based on the TCC protocol pattern for distributed transactions

## Further Reading

- [TCC Pattern Overview](https://seata.apache.org/docs/user/mode/tcc)
- [Distributed Transaction Patterns](https://microservices.io/patterns/data/saga.html)
- [Apache Seata Documentation](https://seata.apache.org/docs/overview/what-is-seata/)

## Support

For questions, issues, or feature requests, please open an issue on GitHub.


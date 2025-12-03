# TCC Library Architecture

## Overview

This document describes the architecture and design decisions of the TCC (Try-Confirm-Cancel) library for Elixir.

## Core Components

### 1. TCC Module (`lib/tcc.ex`)

The main entry point for the library. Provides a high-level, user-friendly API for:
- Creating transactions
- Adding actions to transactions
- Executing transactions synchronously or asynchronously

**Key Functions:**
- `new/1` - Creates a new transaction with options
- `run/5` - Adds a TCC action to the transaction
- `execute/2` - Executes the transaction

### 2. TCC.Transaction Module (`lib/tcc/transaction.ex`)

The transaction coordinator that implements the core TCC protocol logic.

**Responsibilities:**
- Managing the transaction lifecycle
- Coordinating the Try, Confirm, and Cancel phases
- Handling failures and triggering appropriate rollbacks
- Emitting telemetry events

**Key Functions:**
- `new/1` - Initializes a transaction struct
- `add_action/2` - Adds an action to the transaction
- `execute/2` - Executes all phases of the transaction

**Execution Flow:**

```
Start Transaction
      |
      v
Try Phase (Sequential)
      |
      +-- Try Action 1 --> Success
      |         |
      |         +-- Try Action 2 --> Success
      |         |         |
      |         |         +-- Try Action 3 --> Failure
      |         |                   |
      |         |                   v
      |         |           Cancel Phase (Reverse Order)
      |         |                   |
      |         |                   +-- Cancel Action 2
      |         |                   |
      |         |                   +-- Cancel Action 1
      |         |                   |
      |         |                   v
      |         |           Return Error
      |         |
      |         v
      +-- All Try Succeeded
            |
            v
      Confirm Phase (Sequential)
            |
            +-- Confirm Action 1
            |
            +-- Confirm Action 2
            |
            +-- Confirm Action 3
            |
            v
      Return Success
```

### 3. TCC.Action Module (`lib/tcc/action.ex`)

Represents a single action in the transaction with its three phases.

**Responsibilities:**
- Storing action functions (try, confirm, cancel)
- Executing action phases with timeout and retry logic
- Emitting action-level telemetry events

**Key Features:**
- Timeout management per action
- Retry logic with exponential backoff for confirm/cancel
- Telemetry integration for monitoring

## Design Decisions

### 1. Sequential Execution

**Decision:** Execute Try and Confirm phases sequentially, not in parallel.

**Rationale:**
- Simplifies error handling and state management
- Maintains predictable order of operations
- Easier to debug and reason about
- Matches Apache Seata's default behavior

**Trade-off:** Lower performance compared to parallel execution, but better reliability and simpler code.

### 2. Reverse-Order Cancellation

**Decision:** Execute Cancel operations in reverse order (LIFO).

**Rationale:**
- Mirrors the undo stack pattern
- Dependencies are naturally handled (last reserved, first released)
- Reduces risk of deadlocks

**Example:**
```
Try:    Payment -> Inventory -> Shipping
Cancel: Shipping -> Inventory -> Payment
```

### 3. Effects Accumulator

**Decision:** Thread an `effects` map through all operations.

**Rationale:**
- Allows actions to share state and results
- Provides visibility into what happened during the transaction
- Useful for logging, debugging, and returning results to callers

**Example:**
```elixir
effects = %{
  payment_reserved: true,
  payment_reservation_id: "PAY-123",
  inventory_reserved: true,
  inventory_reservation_id: "INV-456"
}
```

### 4. Idempotency Requirements

**Decision:** Require Confirm and Cancel operations to be idempotent.

**Rationale:**
- Network failures can cause retries
- Ensures reliability in distributed environments
- Matches industry best practices (Apache Seata, Saga pattern)

**Implementation:** Built-in retry logic with exponential backoff.

### 5. Telemetry Integration

**Decision:** Use Telemetry instead of built-in metrics.

**Rationale:**
- Telemetry is the Elixir ecosystem standard
- Flexible - users can attach their own handlers
- No dependency on specific monitoring tools
- Lightweight and performant

**Events Emitted:**
- `[:tcc, :transaction, :start]`
- `[:tcc, :transaction, :stop]`
- `[:tcc, :action, :start]`
- `[:tcc, :action, :stop]`
- `[:tcc, :action, :exception]`

### 6. No Global State

**Decision:** No GenServer or global state management.

**Rationale:**
- Simpler architecture
- Better testability
- No single point of failure
- Easier to reason about
- Follows functional programming principles

**Trade-off:** No centralized transaction coordinator like Apache Seata's TC. Each transaction is independent.

### 7. Functional API (Pipe-Friendly)

**Decision:** Design API to work well with Elixir's pipe operator.

**Rationale:**
- Idiomatic Elixir style
- Inspired by Sage library's successful API
- Easy to read and understand transaction flow

**Example:**
```elixir
TCC.new()
|> TCC.run(:action1, &try1/2, &confirm1/2, &cancel1/2)
|> TCC.run(:action2, &try2/2, &confirm2/2, &cancel2/2)
|> TCC.execute(params)
```

## Comparison with Apache Seata

### Similarities

1. **Protocol**: Both implement the TCC protocol
2. **Phases**: Try, Confirm, Cancel with same semantics
3. **Idempotency**: Required for Confirm/Cancel operations
4. **Retry Logic**: Built-in retry for reliability

### Differences

| Aspect | Apache Seata | This Library |
|--------|--------------|--------------|
| Architecture | Client-Server (TC, TM, RM) | Embedded library |
| State Management | Centralized TC | Local to transaction |
| Language | Java | Elixir |
| API Style | Annotations | Functions |
| Distributed Coordination | Yes (across services) | No (within single service) |
| Global Transaction ID | Managed by TC | Generated locally |

### When to Use Which

**Use Apache Seata when:**
- You need cross-service coordination
- You're in a Java ecosystem
- You want centralized transaction management
- You need transaction logs for audit

**Use This Library when:**
- You're in an Elixir ecosystem
- You want a lightweight solution
- You're coordinating operations within a single service
- You prefer local state over distributed coordination

## Testing Strategy

### Unit Tests

- Test each module in isolation
- Mock external dependencies
- Test success and failure paths
- Verify effects accumulation

### Integration Tests

- Test complete transaction flows
- Test rollback scenarios
- Test concurrent transactions
- Test timeout and retry behavior

### Example Tests

Located in:
- `test/tcc_test.exs` - Core TCC functionality
- `test/tcc/action_test.exs` - Action execution and retry logic

## Extension Points

### 1. Custom Telemetry Handlers

Users can attach handlers to monitor transactions:

```elixir
:telemetry.attach_many(
  "my-handler",
  [[:tcc, :transaction, :stop], [:tcc, :action, :stop]],
  &MyApp.TelemetryHandler.handle_event/4,
  nil
)
```

### 2. Custom Timeout Strategies

Per-action or per-transaction timeout configuration:

```elixir
TCC.new(timeout: 60_000)
|> TCC.run_with_opts(:slow_action, try_fn, confirm_fn, cancel_fn,
     timeout: 120_000)
```

### 3. Custom Retry Logic

Configure retry limits and implement custom backoff strategies:

```elixir
TCC.new(retry_limit: 5)
|> TCC.run_with_opts(:critical_action, try_fn, confirm_fn, cancel_fn,
     retry_limit: 10)
```

## Future Enhancements

### Potential Improvements

1. **Persistent Transaction Log**
   - Store transaction history for audit and recovery
   - Implement crash recovery mechanisms

2. **Parallel Execution**
   - Option to execute independent Try operations in parallel
   - Dependency graph analysis

3. **Saga Pattern Integration**
   - Unified API supporting both TCC and Saga patterns
   - Allow mixing patterns in same transaction

4. **Distributed Coordination**
   - Optional centralized coordinator (like Seata's TC)
   - Cross-service transaction support

5. **Timeouts and Deadlines**
   - Global transaction deadline enforcement
   - Automatic timeout calculation based on action count

6. **Circuit Breaker Integration**
   - Integrate with circuit breaker libraries
   - Automatic failure detection and prevention

7. **Visualization Tools**
   - Transaction flow visualization
   - Real-time monitoring dashboard

## Performance Considerations

### Current Performance Characteristics

- **Sequential Execution**: Linear time complexity O(n) where n = number of actions
- **Memory Usage**: Minimal, stores only action definitions and accumulated effects
- **Overhead**: Telemetry events have negligible overhead (<1μs per event)

### Optimization Opportunities

1. **Parallel Try Phase**: Could reduce latency for independent operations
2. **Lazy Effect Evaluation**: Store effects as thunks instead of values
3. **Effect Streaming**: Stream effects instead of accumulating in memory

### Benchmarking

Run benchmarks with:

```bash
mix run benchmarks/simple_transaction.exs
```

## Security Considerations

1. **Input Validation**: Users responsible for validating transaction parameters
2. **Timeout Limits**: Prevents resource exhaustion from long-running operations
3. **Error Information**: Avoid leaking sensitive data in error messages
4. **Telemetry Data**: Be cautious about PII in telemetry metadata

## Conclusion

This architecture provides a solid foundation for implementing distributed transactions using the TCC protocol in Elixir. The design prioritizes:

- **Simplicity**: Easy to understand and use
- **Reliability**: Built-in retry and error handling
- **Observability**: Comprehensive telemetry integration
- **Flexibility**: Extensible through options and handlers
- **Elixir Idioms**: Functional, pipe-friendly API


# TCC vs Other Distributed Transaction Patterns

This document compares the TCC (Try-Confirm-Cancel) pattern with other distributed transaction approaches.

## TCC vs Saga Pattern

### Saga Pattern

The Saga pattern coordinates distributed transactions using a sequence of local transactions, each with a compensating transaction for rollback.

**Flow:**
```
Forward:     T1 → T2 → T3 → T4
Compensate:  C4 ← C3 ← C2 ← C1
```

**Example (using Sage library):**
```elixir
Sage.new()
|> Sage.run(:payment, &process_payment/2, &refund_payment/3)
|> Sage.run(:inventory, &deduct_inventory/2, &restore_inventory/3)
|> Sage.execute(params)
```

### TCC Pattern

The TCC pattern first reserves resources (Try), then either commits (Confirm) or releases (Cancel) them.

**Flow:**
```
Try:      T1 → T2 → T3 → T4
Confirm:  C1 → C2 → C3 → C4  (if all Try succeed)
Cancel:   X4 ← X3 ← X2 ← X1  (if any Try fails)
```

**Example (this library):**
```elixir
TCC.new()
|> TCC.run(:payment, &try_payment/2, &confirm_payment/2, &cancel_payment/2)
|> TCC.run(:inventory, &try_inventory/2, &confirm_inventory/2, &cancel_inventory/2)
|> TCC.execute(params)
```

### Key Differences

| Aspect | Saga | TCC |
|--------|------|-----|
| **Phases** | 2 (Forward + Compensation) | 3 (Try + Confirm/Cancel) |
| **Resource Locking** | No (eventual consistency) | Yes (Try phase reserves) |
| **Commit Point** | Each step commits immediately | All commit together (Confirm) |
| **Isolation** | Low (dirty reads possible) | High (resources reserved) |
| **Complexity** | Lower | Higher |
| **Use Case** | Long-running workflows | Short-lived transactions |
| **Consistency** | Eventual | Strong (within reservation window) |
| **Rollback** | Compensating actions | Release reservations |

### When to Use Each

**Use Saga When:**
- Operations are long-running (hours/days)
- Eventual consistency is acceptable
- Operations can't be easily "reserved"
- Simpler implementation is preferred
- Cross-organization boundaries

**Example Saga Scenarios:**
- Order fulfillment workflow
- User onboarding process
- Document approval workflow
- Email campaign management

**Use TCC When:**
- Need strong consistency guarantees
- Operations are short-lived (seconds/minutes)
- Resources can be reserved
- Preventing dirty reads is critical
- Financial transactions

**Example TCC Scenarios:**
- Bank transfers
- E-commerce order processing
- Inventory reservation systems
- Payment processing
- Booking systems (hotels, flights)

## TCC vs Two-Phase Commit (2PC)

### Two-Phase Commit (2PC)

Traditional distributed transaction protocol with a coordinator and participants.

**Phases:**
1. **Prepare Phase**: All participants vote to commit or abort
2. **Commit Phase**: Coordinator decides based on votes

**Pros:**
- Strong consistency (ACID properties)
- Atomic commitment across all resources
- Well-understood protocol

**Cons:**
- Blocking protocol (participants wait for coordinator)
- Single point of failure (coordinator)
- Poor performance over unreliable networks
- Holding locks during coordination
- Not suitable for microservices

### TCC Pattern

**Phases:**
1. **Try**: Reserve resources
2. **Confirm/Cancel**: Commit or rollback

**Pros:**
- Non-blocking (Try phase can fail immediately)
- Better performance in distributed systems
- Application-level control
- Works across different databases/systems
- Suitable for microservices

**Cons:**
- Application must implement reservation logic
- More complex to implement
- Requires idempotent operations

### Comparison Table

| Aspect | 2PC | TCC |
|--------|-----|-----|
| **Blocking** | Yes | No |
| **Database Support** | Required | Not required |
| **Performance** | Poor in distributed systems | Better |
| **Flexibility** | Limited | High |
| **Complexity** | Protocol-level | Application-level |
| **Failure Recovery** | Coordinator-dependent | Self-contained |
| **Network Sensitivity** | High | Lower |
| **Use in Microservices** | Not recommended | Good fit |

## TCC vs Event Sourcing

### Event Sourcing

Store state changes as a sequence of events; derive current state by replaying events.

**Characteristics:**
- Events are immutable
- Full audit trail
- Can rebuild state at any point in time
- Often combined with CQRS

**Example:**
```elixir
# Events
[
  %PaymentReserved{amount: 100},
  %InventoryReserved{quantity: 1},
  %PaymentConfirmed{},
  %InventoryDeducted{}
]

# Current state derived from events
```

### TCC Pattern

**Characteristics:**
- Explicit state transitions (Try → Confirm/Cancel)
- Focus on resource coordination
- Transactional consistency
- Less about audit trail, more about coordination

### Comparison

| Aspect | Event Sourcing | TCC |
|--------|----------------|-----|
| **Primary Goal** | Audit trail & state reconstruction | Transaction coordination |
| **State Management** | Event stream | Current state |
| **Complexity** | High (event store, projections) | Medium (reservation logic) |
| **Audit Trail** | Complete, immutable | Optional |
| **Time Travel** | Yes (replay events) | No |
| **Consistency** | Eventual | Strong (during transaction) |
| **Use Case** | Systems needing full history | Systems needing coordination |

**Note:** Event Sourcing and TCC can be combined! Use TCC for transaction coordination and emit events for audit trail.

## Implementation Comparison: Apache Seata vs This Library

### Apache Seata (Java)

```java
@TwoPhaseBusinessAction(
    name = "payment",
    commitMethod = "confirm",
    rollbackMethod = "cancel"
)
public boolean tryPayment(BusinessActionContext ctx, 
                         @BusinessActionContextParameter(paramName = "amount") int amount) {
    // Reserve funds
    return paymentService.reserve(amount);
}

public boolean confirm(BusinessActionContext ctx) {
    // Commit payment
    return paymentService.commit(ctx.getActionContext("reservationId"));
}

public boolean cancel(BusinessActionContext ctx) {
    // Cancel payment
    return paymentService.cancel(ctx.getActionContext("reservationId"));
}
```

**Architecture:**
- Centralized Transaction Coordinator (TC)
- Distributed across multiple services
- Requires Seata server deployment
- Automatic global transaction management

### This Library (Elixir)

```elixir
defmodule PaymentService do
  def try_payment(effects, params) do
    case reserve_funds(params.amount) do
      {:ok, reservation_id} ->
        {:ok, effects, Map.put(params, :reservation_id, reservation_id)}
      {:error, reason} ->
        {:error, reason}
    end
  end

  def confirm_payment(effects, params) do
    commit_funds(params.reservation_id)
    {:ok, effects, params}
  end

  def cancel_payment(effects, params) do
    release_funds(params.reservation_id)
    {:ok, effects, params}
  end
end

TCC.new()
|> TCC.run(:payment, &PaymentService.try_payment/2,
                     &PaymentService.confirm_payment/2,
                     &PaymentService.cancel_payment/2)
|> TCC.execute(params)
```

**Architecture:**
- Embedded library (no separate server)
- Local transaction coordination
- Explicit function calls
- Manual transaction composition

### Feature Comparison

| Feature | Apache Seata | This Library |
|---------|--------------|--------------|
| **Language** | Java | Elixir |
| **Deployment** | Requires Seata server | Library only |
| **API Style** | Annotations | Functions |
| **Global TX Management** | Automatic | Manual |
| **Cross-Service** | Yes | Within service |
| **Learning Curve** | Steep | Moderate |
| **Operational Complexity** | High | Low |
| **Flexibility** | Less | More |
| **Best For** | Large microservices | Single service coordination |

## Choosing the Right Pattern

### Decision Tree

```
Do you need distributed transaction across services?
├─ Yes
│  ├─ Can resources be reserved?
│  │  ├─ Yes → TCC with distributed coordinator (Apache Seata)
│  │  └─ No → Saga pattern
│  └─ Is it long-running (hours/days)?
│     └─ Yes → Saga pattern
└─ No (within single service)
   ├─ Need strong consistency?
   │  └─ Yes → TCC (this library) or 2PC
   └─ Need audit trail?
      └─ Yes → Event Sourcing + TCC

Strong consistency needed?
├─ Yes → TCC or 2PC
└─ No → Saga or Event Sourcing

Database supports 2PC?
├─ Yes → Consider 2PC (if simple case)
└─ No → TCC or Saga

Resources can be reserved?
├─ Yes → TCC
└─ No → Saga
```

### Pattern Selection Guide

| Scenario | Recommended Pattern | Why |
|----------|-------------------|-----|
| Bank transfer | **TCC** | Strong consistency, short-lived |
| E-commerce order | **TCC** or **Saga** | TCC if immediate, Saga if workflow |
| User registration | **Saga** | Long-running, eventual consistency OK |
| Hotel booking | **TCC** | Need to hold reservation |
| Document approval | **Saga** | Long-running workflow |
| Payment processing | **TCC** | Strong consistency required |
| Inventory management | **TCC** | Resource reservation critical |
| Email campaign | **Saga** | Long-running, asynchronous |

## Performance Comparison

### Latency

**TCC (this library):**
- Try phase: n × operation_time
- Confirm phase: n × operation_time
- Total: ~2n × operation_time (if all succeed)

**Saga:**
- Forward phase: n × operation_time
- Total: ~n × operation_time (if all succeed)
- Compensation if failure: +m × operation_time

**2PC:**
- Prepare phase: n × (operation_time + network_latency)
- Commit phase: n × network_latency
- Total: Slower due to blocking and coordination

### Throughput

**TCC**: Higher than 2PC, similar to Saga
**Saga**: High (no resource locking)
**2PC**: Lower (resource locking during coordination)

### Resource Utilization

**TCC**: Medium (resources reserved during Try+Confirm)
**Saga**: Low (no reservation)
**2PC**: High (locks held during coordination)

## Conclusion

Each pattern has its place:

- **TCC**: Best for short-lived transactions needing strong consistency with resource coordination
- **Saga**: Best for long-running workflows with eventual consistency
- **2PC**: Best for simple cases with database support (avoid in microservices)
- **Event Sourcing**: Best when full audit trail is required

This TCC library is ideal for:
- Elixir applications needing distributed transaction coordination
- Financial and booking systems requiring strong consistency
- Scenarios where resources can be reserved
- Applications that benefit from explicit transaction control

For cross-service TCC in Java ecosystems, consider Apache Seata.
For long-running workflows in Elixir, consider the Sage library.


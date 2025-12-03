# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-12-02

### Added

#### Core Features
- Initial implementation of TCC (Try-Confirm-Cancel) protocol
- `TCC` module with pipe-friendly API
- `TCC.Transaction` module for transaction coordination
- `TCC.Action` module for individual action execution
- Support for three-phase transactions: Try, Confirm, Cancel
- Automatic rollback on Try phase failures
- Sequential execution of Try and Confirm phases
- Reverse-order execution of Cancel phase (LIFO)

#### Reliability Features
- Automatic retry logic for Confirm and Cancel phases with exponential backoff
- Configurable timeout per transaction and per action
- Configurable retry limits (default: 3 retries)
- Idempotency support through retry mechanism
- Effects accumulator for tracking transaction state

#### Observability
- Telemetry integration with events:
  - `[:tcc, :transaction, :start]` - Transaction started
  - `[:tcc, :transaction, :stop]` - Transaction completed
  - `[:tcc, :action, :start]` - Action phase started
  - `[:tcc, :action, :stop]` - Action phase completed
  - `[:tcc, :action, :exception]` - Action phase exception
- Comprehensive metadata for all telemetry events
- Transaction ID generation for tracking

#### API
- `TCC.new/1` - Create new transaction with options
- `TCC.run/5` - Add action to transaction
- `TCC.run_with_opts/6` - Add action with custom options
- `TCC.execute/2` - Execute transaction synchronously
- `TCC.async_execute/2` - Execute transaction asynchronously
- `TCC.actions/1` - Get list of actions in transaction
- `TCC.options/1` - Get transaction options

#### Configuration Options
- `:timeout` - Maximum time for entire transaction (default: 30_000ms)
- `:retry_limit` - Number of retries for confirm/cancel (default: 3)
- `:async` - Async execution mode (default: false)
- `:telemetry_prefix` - Custom telemetry event prefix (default: `[:tcc]`)

#### Documentation
- Comprehensive README with usage examples
- Quick Start Guide (QUICKSTART.md)
- Architecture documentation (ARCHITECTURE.md)
- API documentation with @moduledoc and @doc
- Doctests for core functionality

#### Examples
- `examples/simple_transfer.ex` - Money transfer between accounts with in-memory database
- `examples/e_commerce.ex` - Complete e-commerce order processing:
  - Payment service with reservation and confirmation
  - Inventory management
  - Shipping arrangement
- `examples/demo.exs` - Interactive demo script

#### Testing
- Comprehensive test suite with ExUnit
- Unit tests for TCC, Transaction, and Action modules
- Integration tests for complete transaction flows
- Test coverage for:
  - Successful transactions
  - Failed transactions with rollback
  - Effects accumulation
  - Retry logic
  - Concurrent execution
  - Custom options

### Technical Details

#### Dependencies
- `telemetry ~> 1.0` - For observability
- `ex_doc ~> 0.29` - For documentation generation (dev only)

#### Inspired By
- Apache Seata's TCC mode for protocol design
- Sage library for API design patterns
- Industry best practices for distributed transactions

### Known Limitations
- Sequential execution only (no parallel Try operations)
- No persistent transaction log (in-memory only)
- No distributed coordinator (local transactions only)
- No automatic timeout deadline enforcement across all actions

### Breaking Changes
- N/A (initial release)

### Deprecated
- N/A (initial release)

### Security
- No known security issues
- Timeout limits prevent resource exhaustion
- No sensitive data logged in telemetry by default

---

## [Unreleased]

### Planned Features
- Parallel execution of independent Try operations
- Persistent transaction log for audit and recovery
- Distributed transaction coordinator support
- Saga pattern integration
- Circuit breaker integration
- Transaction visualization tools
- Enhanced error recovery mechanisms
- Automatic deadline management
- Performance benchmarks

---

[0.1.0]: https://github.com/yourusername/tcc/releases/tag/v0.1.0


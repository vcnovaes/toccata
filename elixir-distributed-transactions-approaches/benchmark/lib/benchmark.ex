defmodule Benchmark do
  @moduledoc """
  Benchmark orchestrator for comparing Sage (Saga) vs TCC patterns.

  This module coordinates benchmark runs across containerized microservices
  and collects metrics for comparison.
  """

  alias Benchmark.{Scenarios, Metrics, Chaos}

  @doc """
  Run all benchmark scenarios.
  """
  def run_all(opts \\ []) do
    scenarios = Keyword.get(opts, :scenarios, [:money_transfer, :order, :booking])
    patterns = Keyword.get(opts, :patterns, [:sage, :tcc])
    iterations = Keyword.get(opts, :iterations, 100)
    warmup = Keyword.get(opts, :warmup, 10)

    results =
      for scenario <- scenarios, pattern <- patterns do
        run_scenario(scenario, pattern, iterations: iterations, warmup: warmup)
      end

    generate_report(results, opts)
  end

  @doc """
  Run a specific benchmark scenario.
  """
  def run_scenario(scenario, pattern, opts \\ []) do
    iterations = Keyword.get(opts, :iterations, 100)
    warmup = Keyword.get(opts, :warmup, 10)

    IO.puts("\n=== Running #{scenario} with #{pattern} (#{iterations} iterations) ===\n")

    # Reset services
    reset_services()

    # Setup scenario data
    setup_scenario(scenario)

    # Warmup
    IO.puts("Warming up...")
    for _ <- 1..warmup do
      execute_scenario(scenario, pattern)
    end

    # Run benchmark
    IO.puts("Running benchmark...")
    metrics = Metrics.new()

    metrics =
      Enum.reduce(1..iterations, metrics, fn i, acc ->
        if rem(i, 10) == 0, do: IO.write(".")
        result = execute_scenario(scenario, pattern)
        Metrics.record(acc, result)
      end)

    IO.puts("\nDone!")

    %{
      scenario: scenario,
      pattern: pattern,
      iterations: iterations,
      metrics: Metrics.summarize(metrics)
    }
  end

  @doc """
  Run chaos engineering tests.
  """
  def run_chaos(scenario, pattern, chaos_type, opts \\ []) do
    iterations = Keyword.get(opts, :iterations, 50)

    IO.puts("\n=== Chaos Test: #{scenario} with #{pattern}, chaos: #{chaos_type} ===\n")

    # Apply chaos
    Chaos.apply(chaos_type)

    # Run scenario
    result = run_scenario(scenario, pattern, iterations: iterations)

    # Remove chaos
    Chaos.remove_all()

    Map.put(result, :chaos_type, chaos_type)
  end

  # Private functions

  defp reset_services do
    services = [:payment, :inventory, :shipping, :hotel, :flight]

    Enum.each(services, fn service ->
      url = service_url(service) <> "/reset"
      Req.post!(url)
    end)
  end

  defp setup_scenario(:money_transfer) do
    # Create accounts
    Req.post!(service_url(:payment) <> "/accounts", json: %{account_id: "alice", balance: 10000})
    Req.post!(service_url(:payment) <> "/accounts", json: %{account_id: "bob", balance: 10000})
  end

  defp setup_scenario(:order) do
    # Create account and inventory
    Req.post!(service_url(:payment) <> "/accounts", json: %{account_id: "customer", balance: 100_000})
    Req.post!(service_url(:inventory) <> "/products", json: %{product_id: "laptop", quantity: 1000})
  end

  defp setup_scenario(:booking) do
    # Create account, flight, and hotel rooms
    Req.post!(service_url(:payment) <> "/accounts", json: %{account_id: "traveler", balance: 500_000})
    Req.post!(service_url(:flight) <> "/flights", json: %{flight_id: "FL123", seats: 1000})

    dates =
      Date.range(Date.utc_today(), Date.add(Date.utc_today(), 30))
      |> Enum.map(&Date.to_iso8601/1)

    Req.post!(service_url(:hotel) <> "/rooms", json: %{room_id: "room101", available_dates: dates})
  end

  defp execute_scenario(:money_transfer, :sage) do
    Scenarios.MoneyTransfer.run_sage()
  end

  defp execute_scenario(:money_transfer, :tcc) do
    Scenarios.MoneyTransfer.run_tcc()
  end

  defp execute_scenario(:order, :sage) do
    Scenarios.Order.run_sage()
  end

  defp execute_scenario(:order, :tcc) do
    Scenarios.Order.run_tcc()
  end

  defp execute_scenario(:booking, :sage) do
    Scenarios.Booking.run_sage()
  end

  defp execute_scenario(:booking, :tcc) do
    Scenarios.Booking.run_tcc()
  end

  defp service_url(:payment), do: System.get_env("PAYMENT_URL", "http://localhost:4001")
  defp service_url(:inventory), do: System.get_env("INVENTORY_URL", "http://localhost:4002")
  defp service_url(:shipping), do: System.get_env("SHIPPING_URL", "http://localhost:4003")
  defp service_url(:hotel), do: System.get_env("HOTEL_URL", "http://localhost:4004")
  defp service_url(:flight), do: System.get_env("FLIGHT_URL", "http://localhost:4005")

  defp generate_report(results, _opts) do
    IO.puts("\n\n========== BENCHMARK RESULTS ==========\n")

    for result <- results do
      IO.puts("#{result.scenario} - #{result.pattern}:")
      IO.puts("  Iterations: #{result.iterations}")
      IO.puts("  Success Rate: #{result.metrics.success_rate}%")
      IO.puts("  Latency (p50): #{result.metrics.p50_ms}ms")
      IO.puts("  Latency (p95): #{result.metrics.p95_ms}ms")
      IO.puts("  Latency (p99): #{result.metrics.p99_ms}ms")
      IO.puts("  Compensation Time (avg): #{result.metrics.avg_compensation_ms}ms")
      IO.puts("")
    end

    results
  end
end

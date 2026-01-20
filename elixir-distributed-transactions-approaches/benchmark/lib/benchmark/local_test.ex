defmodule Benchmark.LocalTest do
  @moduledoc """
  Local benchmark test that doesn't require external services.
  Uses in-memory simulated services for quick testing.
  """

  @doc """
  Run local benchmarks without external services.
  """
  def run_all(opts \\ []) do
    iterations = Keyword.get(opts, :iterations, 100)
    output_file = Keyword.get(opts, :output_file, "benchmark_results.json")

    IO.puts("\n========================================")
    IO.puts("  TCC vs Saga Local Benchmark Suite")
    IO.puts("========================================\n")

    results = %{
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      iterations: iterations,
      scenarios: []
    }

    # Run scenarios
    scenarios = [
      {:simple_transaction, "Simple 2-step transaction"},
      {:medium_transaction, "Medium 4-step transaction"},
      {:complex_transaction, "Complex 5-step transaction"}
    ]

    scenario_results =
      Enum.map(scenarios, fn {scenario, description} ->
        IO.puts("\n--- #{description} ---")

        sage_result = run_local_sage(scenario, iterations)
        tcc_result = run_local_tcc(scenario, iterations)

        %{
          name: Atom.to_string(scenario),
          description: description,
          sage: sage_result,
          tcc: tcc_result
        }
      end)

    results = %{results | scenarios: scenario_results}

    # Print summary
    print_summary(results)

    # Save to file
    save_results(results, output_file)

    results
  end

  defp run_local_sage(scenario, iterations) do
    IO.write("  Sage: ")
    start_time = System.monotonic_time(:millisecond)

    latencies =
      Enum.map(1..iterations, fn i ->
        if rem(i, 10) == 0, do: IO.write(".")
        run_sage_transaction(scenario)
      end)

    total_time = System.monotonic_time(:millisecond) - start_time
    IO.puts(" Done!")

    calculate_stats(latencies, total_time, iterations)
  end

  defp run_local_tcc(scenario, iterations) do
    IO.write("  TCC:  ")
    start_time = System.monotonic_time(:millisecond)

    latencies =
      Enum.map(1..iterations, fn i ->
        if rem(i, 10) == 0, do: IO.write(".")
        run_tcc_transaction(scenario)
      end)

    total_time = System.monotonic_time(:millisecond) - start_time
    IO.puts(" Done!")

    calculate_stats(latencies, total_time, iterations)
  end

  defp run_sage_transaction(:simple_transaction) do
    start = System.monotonic_time(:microsecond)

    _result =
      Sage.new()
      |> Sage.run(:step1, fn _effects, _opts ->
        simulate_work(1)
        {:ok, :step1_done}
      end, fn _effect, _effects, _opts -> :ok end)
      |> Sage.run(:step2, fn _effects, _opts ->
        simulate_work(1)
        {:ok, :step2_done}
      end, fn _effect, _effects, _opts -> :ok end)
      |> Sage.execute()

    (System.monotonic_time(:microsecond) - start) / 1000
  end

  defp run_sage_transaction(:medium_transaction) do
    start = System.monotonic_time(:microsecond)

    _result =
      Sage.new()
      |> Sage.run(:step1, fn _effects, _opts -> simulate_work(1); {:ok, :s1} end, fn _, _, _ -> :ok end)
      |> Sage.run(:step2, fn _effects, _opts -> simulate_work(1); {:ok, :s2} end, fn _, _, _ -> :ok end)
      |> Sage.run(:step3, fn _effects, _opts -> simulate_work(1); {:ok, :s3} end, fn _, _, _ -> :ok end)
      |> Sage.run(:step4, fn _effects, _opts -> simulate_work(1); {:ok, :s4} end, fn _, _, _ -> :ok end)
      |> Sage.execute()

    (System.monotonic_time(:microsecond) - start) / 1000
  end

  defp run_sage_transaction(:complex_transaction) do
    start = System.monotonic_time(:microsecond)

    _result =
      Sage.new()
      |> Sage.run(:step1, fn _effects, _opts -> simulate_work(1); {:ok, :s1} end, fn _, _, _ -> :ok end)
      |> Sage.run(:step2, fn _effects, _opts -> simulate_work(1); {:ok, :s2} end, fn _, _, _ -> :ok end)
      |> Sage.run(:step3, fn _effects, _opts -> simulate_work(1); {:ok, :s3} end, fn _, _, _ -> :ok end)
      |> Sage.run(:step4, fn _effects, _opts -> simulate_work(1); {:ok, :s4} end, fn _, _, _ -> :ok end)
      |> Sage.run(:step5, fn _effects, _opts -> simulate_work(1); {:ok, :s5} end, fn _, _, _ -> :ok end)
      |> Sage.execute()

    (System.monotonic_time(:microsecond) - start) / 1000
  end

  defp run_tcc_transaction(:simple_transaction) do
    start = System.monotonic_time(:microsecond)

    _result =
      TCC.new()
      |> TCC.run(:step1,
        fn effects, params -> simulate_work(1); {:ok, effects, params} end,
        fn effects, params -> {:ok, effects, params} end,
        fn effects, params -> {:ok, effects, params} end)
      |> TCC.run(:step2,
        fn effects, params -> simulate_work(1); {:ok, effects, params} end,
        fn effects, params -> {:ok, effects, params} end,
        fn effects, params -> {:ok, effects, params} end)
      |> TCC.execute(%{})

    (System.monotonic_time(:microsecond) - start) / 1000
  end

  defp run_tcc_transaction(:medium_transaction) do
    start = System.monotonic_time(:microsecond)

    _result =
      TCC.new()
      |> TCC.run(:step1, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.run(:step2, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.run(:step3, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.run(:step4, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.execute(%{})

    (System.monotonic_time(:microsecond) - start) / 1000
  end

  defp run_tcc_transaction(:complex_transaction) do
    start = System.monotonic_time(:microsecond)

    _result =
      TCC.new()
      |> TCC.run(:step1, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.run(:step2, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.run(:step3, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.run(:step4, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.run(:step5, &try_fn/2, &confirm_fn/2, &cancel_fn/2)
      |> TCC.execute(%{})

    (System.monotonic_time(:microsecond) - start) / 1000
  end

  defp try_fn(effects, params), do: (simulate_work(1); {:ok, effects, params})
  defp confirm_fn(effects, params), do: {:ok, effects, params}
  defp cancel_fn(effects, params), do: {:ok, effects, params}

  defp simulate_work(ms) do
    # Simulate some CPU work
    Enum.reduce(1..100, 0, fn i, acc -> acc + i end)
    if ms > 0, do: :timer.sleep(ms)
  end

  defp calculate_stats(latencies, total_time, iterations) do
    sorted = Enum.sort(latencies)
    count = length(sorted)

    %{
      total_time_ms: total_time,
      throughput: Float.round(iterations / (total_time / 1000), 2),
      min_ms: Float.round(Enum.min(sorted), 3),
      max_ms: Float.round(Enum.max(sorted), 3),
      avg_ms: Float.round(Enum.sum(sorted) / count, 3),
      p50_ms: Float.round(Enum.at(sorted, div(count, 2)), 3),
      p95_ms: Float.round(Enum.at(sorted, trunc(count * 0.95)), 3),
      p99_ms: Float.round(Enum.at(sorted, trunc(count * 0.99)), 3)
    }
  end

  defp print_summary(results) do
    IO.puts("\n\n========================================")
    IO.puts("           BENCHMARK RESULTS")
    IO.puts("========================================\n")

    for scenario <- results.scenarios do
      IO.puts("#{scenario.description}")
      IO.puts(String.duplicate("-", 50))
      IO.puts("")
      IO.puts("                    Sage          TCC")
      IO.puts("  Throughput:    #{pad(scenario.sage.throughput)}/s    #{pad(scenario.tcc.throughput)}/s")
      IO.puts("  Avg Latency:   #{pad(scenario.sage.avg_ms)}ms    #{pad(scenario.tcc.avg_ms)}ms")
      IO.puts("  P50 Latency:   #{pad(scenario.sage.p50_ms)}ms    #{pad(scenario.tcc.p50_ms)}ms")
      IO.puts("  P95 Latency:   #{pad(scenario.sage.p95_ms)}ms    #{pad(scenario.tcc.p95_ms)}ms")
      IO.puts("  P99 Latency:   #{pad(scenario.sage.p99_ms)}ms    #{pad(scenario.tcc.p99_ms)}ms")
      IO.puts("")
    end

    IO.puts("========================================\n")
  end

  defp pad(num) when is_float(num), do: :io_lib.format("~8.3f", [num]) |> to_string()
  defp pad(num) when is_integer(num), do: :io_lib.format("~8d", [num]) |> to_string()

  defp save_results(results, filename) do
    json = Jason.encode!(results, pretty: true)
    File.write!(filename, json)
    IO.puts("Results saved to: #{filename}")
  end
end

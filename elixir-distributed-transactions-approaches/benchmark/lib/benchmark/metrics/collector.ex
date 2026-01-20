defmodule Benchmark.Metrics do
  @moduledoc """
  Metrics collection and aggregation for benchmark results.
  """

  defstruct [
    :total,
    :successes,
    :failures,
    :latencies,
    :compensation_times
  ]

  @doc """
  Create a new metrics collector.
  """
  def new do
    %__MODULE__{
      total: 0,
      successes: 0,
      failures: 0,
      latencies: [],
      compensation_times: []
    }
  end

  @doc """
  Record a benchmark result.
  """
  def record(%__MODULE__{} = metrics, result) do
    %{
      metrics
      | total: metrics.total + 1,
        successes: metrics.successes + if(result.success?, do: 1, else: 0),
        failures: metrics.failures + if(result.success?, do: 0, else: 1),
        latencies: [result.latency_ms | metrics.latencies],
        compensation_times:
          if(result.compensation_ms,
            do: [result.compensation_ms | metrics.compensation_times],
            else: metrics.compensation_times
          )
    }
  end

  @doc """
  Summarize collected metrics.
  """
  def summarize(%__MODULE__{} = metrics) do
    sorted_latencies = Enum.sort(metrics.latencies)
    count = length(sorted_latencies)

    %{
      total: metrics.total,
      successes: metrics.successes,
      failures: metrics.failures,
      success_rate: Float.round(metrics.successes / max(metrics.total, 1) * 100, 2),
      min_ms: Enum.min(sorted_latencies, fn -> 0 end),
      max_ms: Enum.max(sorted_latencies, fn -> 0 end),
      avg_ms: Float.round(Enum.sum(sorted_latencies) / max(count, 1), 2),
      p50_ms: percentile(sorted_latencies, 50),
      p95_ms: percentile(sorted_latencies, 95),
      p99_ms: percentile(sorted_latencies, 99),
      avg_compensation_ms:
        if(metrics.compensation_times != [],
          do:
            Float.round(
              Enum.sum(metrics.compensation_times) / length(metrics.compensation_times),
              2
            ),
          else: 0
        )
    }
  end

  defp percentile([], _p), do: 0

  defp percentile(sorted_list, p) do
    count = length(sorted_list)
    index = trunc(count * p / 100)
    Enum.at(sorted_list, min(index, count - 1))
  end
end

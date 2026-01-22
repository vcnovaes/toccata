defmodule Benchmark.MixProject do
  use Mix.Project

  def project do
    [
      app: :benchmark,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Benchmark.Application, []}
    ]
  end

  defp deps do
    [
      # HTTP client
      {:req, "~> 0.4"},
      # Benchmarking
      {:benchee, "~> 1.1"},
      {:benchee_html, "~> 1.0"},
      {:benchee_json, "~> 1.0"},
      # JSON
      {:jason, "~> 1.4"},
      # Statistics
      {:statistex, "~> 1.0"},
      # Saga library
      {:sage, "~> 0.6"},
      # Toccata TCC library (local path)
      {:toccata, path: "../../tcc_lib"}
    ]
  end

  defp aliases do
    [
      "benchmark.run": ["run lib/benchmark/cli.exs"],
      "benchmark.chaos": ["run lib/benchmark/chaos_cli.exs"]
    ]
  end
end

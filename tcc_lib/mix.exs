defmodule TCC.MixProject do
  use Mix.Project

  def project do
    [
      app: :tcc,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:telemetry, "~> 1.0"},
      {:ex_doc, "~> 0.29", only: :dev, runtime: false}
    ]
  end

  defp description do
    """
    TCC (Try-Confirm-Cancel) protocol implementation for distributed transactions in Elixir.
    Inspired by Apache Seata's TCC mode and designed with a similar API to the Sage library.
    """
  end

  defp package do
    [
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/yourusername/tcc"}
    ]
  end

  defp docs do
    [
      main: "TCC",
      extras: ["README.md"]
    ]
  end
end

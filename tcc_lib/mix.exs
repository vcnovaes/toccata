defmodule Toccata.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/viniciusmuller/toccata"

  def project do
    [
      app: :toccata,
      version: @version,
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      docs: docs(),
      source_url: @source_url,
      homepage_url: @source_url
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
    Toccata: A TCC (Try-Confirm-Cancel) protocol implementation for distributed
    transactions in Elixir. Inspired by Apache Seata's TCC mode and designed
    with a similar API to the Sage library.
    """
  end

  defp package do
    [
      name: "toccata",
      licenses: ["MIT"],
      links: %{
        "GitHub" => @source_url,
        "Changelog" => "#{@source_url}/blob/main/CHANGELOG.md"
      },
      files: ~w(lib .formatter.exs mix.exs README.md LICENSE CHANGELOG.md)
    ]
  end

  defp docs do
    [
      main: "readme",
      name: "Toccata",
      source_ref: "v#{@version}",
      source_url: @source_url,
      extras: ["README.md", "CHANGELOG.md"]
    ]
  end
end

defmodule Benchmark.Chaos do
  @moduledoc """
  Chaos engineering scenarios for testing distributed transaction resilience.
  """

  alias Benchmark.Chaos.ToxiproxyClient

  @proxies ["payment", "inventory", "shipping", "hotel", "flight"]

  @doc """
  Apply a chaos scenario.
  """
  def apply(:latency_100ms) do
    Enum.each(@proxies, fn proxy ->
      ToxiproxyClient.add_latency(proxy, 100)
    end)
  end

  def apply(:latency_500ms) do
    Enum.each(@proxies, fn proxy ->
      ToxiproxyClient.add_latency(proxy, 500)
    end)
  end

  def apply(:latency_jitter) do
    Enum.each(@proxies, fn proxy ->
      ToxiproxyClient.add_latency(proxy, 100, jitter: 50)
    end)
  end

  def apply(:packet_loss_10) do
    Enum.each(@proxies, fn proxy ->
      ToxiproxyClient.add_timeout(proxy, 1000, toxicity: 0.1)
    end)
  end

  def apply(:packet_loss_30) do
    Enum.each(@proxies, fn proxy ->
      ToxiproxyClient.add_timeout(proxy, 1000, toxicity: 0.3)
    end)
  end

  def apply(:slow_network) do
    Enum.each(@proxies, fn proxy ->
      ToxiproxyClient.add_bandwidth_limit(proxy, 100)
    end)
  end

  def apply(:payment_down) do
    ToxiproxyClient.disable_proxy("payment")
  end

  def apply(:inventory_down) do
    ToxiproxyClient.disable_proxy("inventory")
  end

  def apply(:random_service_down) do
    proxy = Enum.random(@proxies)
    ToxiproxyClient.disable_proxy(proxy)
  end

  def apply(:cascade_failure) do
    # Simulate cascading failures with increasing latency
    @proxies
    |> Enum.with_index()
    |> Enum.each(fn {proxy, idx} ->
      ToxiproxyClient.add_latency(proxy, 100 * (idx + 1))
    end)
  end

  def apply(:chaos_mix) do
    # Mix of different chaos conditions
    ToxiproxyClient.add_latency("payment", 200, jitter: 100)
    ToxiproxyClient.add_latency("inventory", 150, jitter: 50)
    ToxiproxyClient.add_timeout("shipping", 500, toxicity: 0.2)
    ToxiproxyClient.add_latency("hotel", 300)
    ToxiproxyClient.add_bandwidth_limit("flight", 50)
  end

  def apply(:none), do: :ok

  @doc """
  Remove all chaos conditions.
  """
  def remove_all do
    ToxiproxyClient.reset_all()
  end

  @doc """
  List available chaos scenarios.
  """
  def list_scenarios do
    [
      :none,
      :latency_100ms,
      :latency_500ms,
      :latency_jitter,
      :packet_loss_10,
      :packet_loss_30,
      :slow_network,
      :payment_down,
      :inventory_down,
      :random_service_down,
      :cascade_failure,
      :chaos_mix
    ]
  end
end

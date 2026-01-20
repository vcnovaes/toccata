defmodule Benchmark.Chaos.ToxiproxyClient do
  @moduledoc """
  Client for interacting with Toxiproxy API to inject network faults.
  """

  @toxiproxy_url System.get_env("TOXIPROXY_URL", "http://localhost:8474")

  @doc """
  Add latency to a proxy.
  """
  def add_latency(proxy_name, latency_ms, opts \\ []) do
    jitter = Keyword.get(opts, :jitter, 0)
    toxicity = Keyword.get(opts, :toxicity, 1.0)

    toxic = %{
      name: "latency_#{proxy_name}",
      type: "latency",
      stream: "downstream",
      toxicity: toxicity,
      attributes: %{
        latency: latency_ms,
        jitter: jitter
      }
    }

    Req.post("#{@toxiproxy_url}/proxies/#{proxy_name}/toxics", json: toxic)
  end

  @doc """
  Add timeout (simulates packet loss/connection timeout).
  """
  def add_timeout(proxy_name, timeout_ms, opts \\ []) do
    toxicity = Keyword.get(opts, :toxicity, 1.0)

    toxic = %{
      name: "timeout_#{proxy_name}",
      type: "timeout",
      stream: "downstream",
      toxicity: toxicity,
      attributes: %{
        timeout: timeout_ms
      }
    }

    Req.post("#{@toxiproxy_url}/proxies/#{proxy_name}/toxics", json: toxic)
  end

  @doc """
  Add bandwidth limit.
  """
  def add_bandwidth_limit(proxy_name, rate_kb, opts \\ []) do
    toxicity = Keyword.get(opts, :toxicity, 1.0)

    toxic = %{
      name: "bandwidth_#{proxy_name}",
      type: "bandwidth",
      stream: "downstream",
      toxicity: toxicity,
      attributes: %{
        rate: rate_kb
      }
    }

    Req.post("#{@toxiproxy_url}/proxies/#{proxy_name}/toxics", json: toxic)
  end

  @doc """
  Add connection reset (simulates TCP RST).
  """
  def add_reset_peer(proxy_name, opts \\ []) do
    toxicity = Keyword.get(opts, :toxicity, 1.0)
    timeout = Keyword.get(opts, :timeout, 0)

    toxic = %{
      name: "reset_#{proxy_name}",
      type: "reset_peer",
      stream: "downstream",
      toxicity: toxicity,
      attributes: %{
        timeout: timeout
      }
    }

    Req.post("#{@toxiproxy_url}/proxies/#{proxy_name}/toxics", json: toxic)
  end

  @doc """
  Disable a proxy (simulates network partition).
  """
  def disable_proxy(proxy_name) do
    Req.post("#{@toxiproxy_url}/proxies/#{proxy_name}", json: %{enabled: false})
  end

  @doc """
  Enable a proxy.
  """
  def enable_proxy(proxy_name) do
    Req.post("#{@toxiproxy_url}/proxies/#{proxy_name}", json: %{enabled: true})
  end

  @doc """
  Remove a specific toxic.
  """
  def remove_toxic(proxy_name, toxic_name) do
    Req.delete("#{@toxiproxy_url}/proxies/#{proxy_name}/toxics/#{toxic_name}")
  end

  @doc """
  Remove all toxics from a proxy.
  """
  def remove_all_toxics(proxy_name) do
    case Req.get("#{@toxiproxy_url}/proxies/#{proxy_name}/toxics") do
      {:ok, %{status: 200, body: toxics}} when is_list(toxics) ->
        Enum.each(toxics, fn toxic ->
          remove_toxic(proxy_name, toxic["name"])
        end)

      _ ->
        :ok
    end
  end

  @doc """
  Remove all toxics from all proxies.
  """
  def reset_all do
    proxies = ["payment", "inventory", "shipping", "hotel", "flight"]

    Enum.each(proxies, fn proxy ->
      remove_all_toxics(proxy)
      enable_proxy(proxy)
    end)
  end

  @doc """
  Get proxy status.
  """
  def get_proxy(proxy_name) do
    Req.get("#{@toxiproxy_url}/proxies/#{proxy_name}")
  end

  @doc """
  List all proxies.
  """
  def list_proxies do
    Req.get("#{@toxiproxy_url}/proxies")
  end
end

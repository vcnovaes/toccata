defmodule Benchmark.Scenarios.Order do
  @moduledoc """
  E-commerce order scenario - medium complexity distributed transaction.

  Services involved:
  - Payment (authorize payment)
  - Inventory (reserve items)
  - Shipping (arrange delivery)
  """

  @payment_url System.get_env("PAYMENT_URL", "http://localhost:4001")
  @inventory_url System.get_env("INVENTORY_URL", "http://localhost:4002")
  @shipping_url System.get_env("SHIPPING_URL", "http://localhost:4003")

  @doc """
  Run order processing using Sage (Saga pattern).
  """
  def run_sage(opts \\ []) do
    amount = Keyword.get(opts, :amount, 999)
    product_id = Keyword.get(opts, :product_id, "laptop")
    quantity = Keyword.get(opts, :quantity, 1)
    address = Keyword.get(opts, :address, "123 Main St")

    start_time = System.monotonic_time(:millisecond)

    result =
      Sage.new()
      |> Sage.run(:payment, fn _effects, _opts ->
        case authorize_payment("customer", amount) do
          {:ok, id} -> {:ok, id}
          {:error, reason} -> {:error, reason}
        end
      end, fn payment_id, _effects, _opts ->
        cancel_payment(payment_id)
        :ok
      end)
      |> Sage.run(:inventory, fn _effects, _opts ->
        case reserve_inventory(product_id, quantity) do
          {:ok, id} -> {:ok, id}
          {:error, reason} -> {:error, reason}
        end
      end, fn inventory_id, _effects, _opts ->
        cancel_inventory(inventory_id)
        :ok
      end)
      |> Sage.run(:shipping, fn _effects, _opts ->
        case arrange_shipping(address, [%{product: product_id, qty: quantity}]) do
          {:ok, id} -> {:ok, id}
          {:error, reason} -> {:error, reason}
        end
      end, fn shipping_id, _effects, _opts ->
        cancel_shipping(shipping_id)
        :ok
      end)
      |> Sage.run(:confirm_all, fn effects, _opts ->
        confirm_payment(effects.payment)
        confirm_inventory(effects.inventory)
        confirm_shipping(effects.shipping)
        {:ok, effects}
      end)
      |> Sage.execute()

    end_time = System.monotonic_time(:millisecond)
    latency = end_time - start_time

    case result do
      {:ok, _effect, _effects} ->
        %{success?: true, latency_ms: latency, compensation_ms: nil}

      {:error, _reason} ->
        %{success?: false, latency_ms: latency, compensation_ms: latency}
    end
  end

  @doc """
  Run order processing using TCC pattern.
  """
  def run_tcc(opts \\ []) do
    amount = Keyword.get(opts, :amount, 999)
    product_id = Keyword.get(opts, :product_id, "laptop")
    quantity = Keyword.get(opts, :quantity, 1)
    address = Keyword.get(opts, :address, "123 Main St")

    start_time = System.monotonic_time(:millisecond)

    result =
      TCC.new()
      |> TCC.run(
        :payment,
        fn effects, params ->
          case authorize_payment("customer", amount) do
            {:ok, id} -> {:ok, Map.put(effects, :payment_id, id), params}
            {:error, reason} -> {:error, reason}
          end
        end,
        fn effects, params ->
          confirm_payment(effects.payment_id)
          {:ok, effects, params}
        end,
        fn effects, params ->
          if effects[:payment_id], do: cancel_payment(effects.payment_id)
          {:ok, effects, params}
        end
      )
      |> TCC.run(
        :inventory,
        fn effects, params ->
          case reserve_inventory(product_id, quantity) do
            {:ok, id} -> {:ok, Map.put(effects, :inventory_id, id), params}
            {:error, reason} -> {:error, reason}
          end
        end,
        fn effects, params ->
          confirm_inventory(effects.inventory_id)
          {:ok, effects, params}
        end,
        fn effects, params ->
          if effects[:inventory_id], do: cancel_inventory(effects.inventory_id)
          {:ok, effects, params}
        end
      )
      |> TCC.run(
        :shipping,
        fn effects, params ->
          case arrange_shipping(address, [%{product: product_id, qty: quantity}]) do
            {:ok, id} -> {:ok, Map.put(effects, :shipping_id, id), params}
            {:error, reason} -> {:error, reason}
          end
        end,
        fn effects, params ->
          confirm_shipping(effects.shipping_id)
          {:ok, effects, params}
        end,
        fn effects, params ->
          if effects[:shipping_id], do: cancel_shipping(effects.shipping_id)
          {:ok, effects, params}
        end
      )
      |> TCC.execute(%{amount: amount, product_id: product_id, quantity: quantity})

    end_time = System.monotonic_time(:millisecond)
    latency = end_time - start_time

    case result do
      {:ok, _effects, _params} ->
        %{success?: true, latency_ms: latency, compensation_ms: nil}

      {:error, _stage, _reason, _effects} ->
        %{success?: false, latency_ms: latency, compensation_ms: latency}
    end
  end

  # Payment service calls
  defp authorize_payment(account_id, amount) do
    case Req.post("#{@payment_url}/try", json: %{account_id: account_id, amount: amount}) do
      {:ok, %{status: 200, body: %{"reservation_id" => id}}} -> {:ok, id}
      {:ok, %{body: %{"reason" => reason}}} -> {:error, reason}
      {:error, reason} -> {:error, reason}
    end
  end

  defp confirm_payment(id), do: Req.post("#{@payment_url}/confirm", json: %{reservation_id: id})
  defp cancel_payment(id), do: Req.post("#{@payment_url}/cancel", json: %{reservation_id: id})

  # Inventory service calls
  defp reserve_inventory(product_id, quantity) do
    case Req.post("#{@inventory_url}/try", json: %{product_id: product_id, quantity: quantity}) do
      {:ok, %{status: 200, body: %{"reservation_id" => id}}} -> {:ok, id}
      {:ok, %{body: %{"reason" => reason}}} -> {:error, reason}
      {:error, reason} -> {:error, reason}
    end
  end

  defp confirm_inventory(id), do: Req.post("#{@inventory_url}/confirm", json: %{reservation_id: id})
  defp cancel_inventory(id), do: Req.post("#{@inventory_url}/cancel", json: %{reservation_id: id})

  # Shipping service calls
  defp arrange_shipping(address, items) do
    case Req.post("#{@shipping_url}/try", json: %{address: address, items: items}) do
      {:ok, %{status: 200, body: %{"shipment_id" => id}}} -> {:ok, id}
      {:ok, %{body: %{"reason" => reason}}} -> {:error, reason}
      {:error, reason} -> {:error, reason}
    end
  end

  defp confirm_shipping(id), do: Req.post("#{@shipping_url}/confirm", json: %{shipment_id: id})
  defp cancel_shipping(id), do: Req.post("#{@shipping_url}/cancel", json: %{shipment_id: id})
end

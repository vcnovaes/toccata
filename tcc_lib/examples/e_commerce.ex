defmodule TCC.Examples.ECommerce do
  @moduledoc """
  Example of using TCC for an e-commerce order processing system.

  This example demonstrates a distributed transaction across multiple services:
  - Payment Service: Handles payment processing
  - Inventory Service: Manages product inventory
  - Shipping Service: Arranges shipping
  """

  require Logger

  # ============================================================================
  # Payment Service
  # ============================================================================

  defmodule PaymentService do
    @moduledoc """
    Handles payment operations with reservation and confirmation.
    """

    def try_payment(effects, params) do
      Logger.info("PaymentService: Trying to reserve payment for order #{params.order_id}")

      # Simulate payment reservation
      case reserve_payment(params) do
        {:ok, reservation_id} ->
          updated_params = Map.put(params, :payment_reservation_id, reservation_id)
          updated_effects = Map.put(effects, :payment_reserved, true)

          Logger.info("PaymentService: Successfully reserved payment #{reservation_id}")
          {:ok, updated_effects, updated_params}

        {:error, reason} ->
          Logger.error("PaymentService: Failed to reserve payment - #{reason}")
          {:error, reason}
      end
    end

    def confirm_payment(effects, params) do
      Logger.info("PaymentService: Confirming payment #{params.payment_reservation_id}")

      # Simulate payment confirmation (actual charge)
      case confirm_payment_reservation(params.payment_reservation_id) do
        {:ok, transaction_id} ->
          updated_params = Map.put(params, :payment_transaction_id, transaction_id)
          updated_effects = Map.put(effects, :payment_confirmed, true)

          Logger.info("PaymentService: Payment confirmed with transaction #{transaction_id}")
          {:ok, updated_effects, updated_params}

        {:error, reason} ->
          Logger.error("PaymentService: Failed to confirm payment - #{reason}")
          {:error, reason}
      end
    end

    def cancel_payment(effects, params) do
      Logger.info("PaymentService: Cancelling payment reservation #{params.payment_reservation_id}")

      # Release the payment reservation
      case release_payment_reservation(params.payment_reservation_id) do
        {:ok, _} ->
          updated_effects = Map.put(effects, :payment_cancelled, true)

          Logger.info("PaymentService: Payment reservation cancelled")
          {:ok, updated_effects, params}

        {:error, reason} ->
          Logger.error("PaymentService: Failed to cancel payment - #{reason}")
          {:error, reason}
      end
    end

    # Simulated backend operations
    defp reserve_payment(params) do
      # Simulate checking account balance and creating reservation
      if params.amount > 0 and params.amount < 10_000 do
        reservation_id = "PAY_RES_" <> generate_id()
        {:ok, reservation_id}
      else
        {:error, :insufficient_funds}
      end
    end

    defp confirm_payment_reservation(reservation_id) do
      # Simulate actual payment processing
      transaction_id = "PAY_TXN_" <> generate_id()
      {:ok, transaction_id}
    end

    defp release_payment_reservation(_reservation_id) do
      # Simulate releasing the reservation
      {:ok, :released}
    end

    defp generate_id do
      :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
    end
  end

  # ============================================================================
  # Inventory Service
  # ============================================================================

  defmodule InventoryService do
    @moduledoc """
    Manages inventory reservations and commitments.
    """

    def try_reserve_inventory(effects, params) do
      Logger.info("InventoryService: Trying to reserve #{params.quantity} units of product #{params.product_id}")

      case reserve_inventory(params) do
        {:ok, reservation_id} ->
          updated_params = Map.put(params, :inventory_reservation_id, reservation_id)
          updated_effects = Map.put(effects, :inventory_reserved, true)

          Logger.info("InventoryService: Successfully reserved inventory #{reservation_id}")
          {:ok, updated_effects, updated_params}

        {:error, reason} ->
          Logger.error("InventoryService: Failed to reserve inventory - #{reason}")
          {:error, reason}
      end
    end

    def confirm_reserve_inventory(effects, params) do
      Logger.info("InventoryService: Confirming inventory reservation #{params.inventory_reservation_id}")

      case commit_inventory_reservation(params.inventory_reservation_id) do
        {:ok, _} ->
          updated_effects = Map.put(effects, :inventory_confirmed, true)

          Logger.info("InventoryService: Inventory reservation confirmed")
          {:ok, updated_effects, params}

        {:error, reason} ->
          Logger.error("InventoryService: Failed to confirm inventory - #{reason}")
          {:error, reason}
      end
    end

    def cancel_reserve_inventory(effects, params) do
      Logger.info("InventoryService: Cancelling inventory reservation #{params.inventory_reservation_id}")

      case release_inventory_reservation(params.inventory_reservation_id) do
        {:ok, _} ->
          updated_effects = Map.put(effects, :inventory_cancelled, true)

          Logger.info("InventoryService: Inventory reservation cancelled")
          {:ok, updated_effects, params}

        {:error, reason} ->
          Logger.error("InventoryService: Failed to cancel inventory - #{reason}")
          {:error, reason}
      end
    end

    # Simulated backend operations
    defp reserve_inventory(params) do
      # Simulate checking inventory and creating reservation
      if params.quantity > 0 and params.quantity <= 100 do
        reservation_id = "INV_RES_" <> generate_id()
        {:ok, reservation_id}
      else
        {:error, :insufficient_inventory}
      end
    end

    defp commit_inventory_reservation(_reservation_id) do
      # Simulate deducting from inventory
      {:ok, :committed}
    end

    defp release_inventory_reservation(_reservation_id) do
      # Simulate releasing the reservation
      {:ok, :released}
    end

    defp generate_id do
      :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
    end
  end

  # ============================================================================
  # Shipping Service
  # ============================================================================

  defmodule ShippingService do
    @moduledoc """
    Handles shipping arrangements and cancellations.
    """

    def try_arrange_shipping(effects, params) do
      Logger.info("ShippingService: Trying to arrange shipping for order #{params.order_id}")

      case reserve_shipping(params) do
        {:ok, shipping_id} ->
          updated_params = Map.put(params, :shipping_id, shipping_id)
          updated_effects = Map.put(effects, :shipping_reserved, true)

          Logger.info("ShippingService: Successfully arranged shipping #{shipping_id}")
          {:ok, updated_effects, updated_params}

        {:error, reason} ->
          Logger.error("ShippingService: Failed to arrange shipping - #{reason}")
          {:error, reason}
      end
    end

    def confirm_arrange_shipping(effects, params) do
      Logger.info("ShippingService: Confirming shipping arrangement #{params.shipping_id}")

      case confirm_shipping(params.shipping_id) do
        {:ok, tracking_number} ->
          updated_params = Map.put(params, :tracking_number, tracking_number)
          updated_effects = Map.put(effects, :shipping_confirmed, true)

          Logger.info("ShippingService: Shipping confirmed with tracking #{tracking_number}")
          {:ok, updated_effects, updated_params}

        {:error, reason} ->
          Logger.error("ShippingService: Failed to confirm shipping - #{reason}")
          {:error, reason}
      end
    end

    def cancel_arrange_shipping(effects, params) do
      Logger.info("ShippingService: Cancelling shipping arrangement #{params.shipping_id}")

      case cancel_shipping(params.shipping_id) do
        {:ok, _} ->
          updated_effects = Map.put(effects, :shipping_cancelled, true)

          Logger.info("ShippingService: Shipping arrangement cancelled")
          {:ok, updated_effects, params}

        {:error, reason} ->
          Logger.error("ShippingService: Failed to cancel shipping - #{reason}")
          {:error, reason}
      end
    end

    # Simulated backend operations
    defp reserve_shipping(params) do
      # Simulate checking shipping availability
      if params.address != nil do
        shipping_id = "SHIP_" <> generate_id()
        {:ok, shipping_id}
      else
        {:error, :invalid_address}
      end
    end

    defp confirm_shipping(_shipping_id) do
      # Simulate confirming with shipping carrier
      tracking_number = "TRACK_" <> generate_id()
      {:ok, tracking_number}
    end

    defp cancel_shipping(_shipping_id) do
      # Simulate cancelling shipping arrangement
      {:ok, :cancelled}
    end

    defp generate_id do
      :crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)
    end
  end

  # ============================================================================
  # Order Processing
  # ============================================================================

  @doc """
  Processes an order using TCC distributed transaction.

  ## Examples

      # Successful order
      params = %{
        order_id: "ORD-001",
        product_id: "PROD-123",
        quantity: 2,
        amount: 99.99,
        address: "123 Main St, City, Country"
      }

      case TCC.Examples.ECommerce.process_order(params) do
        {:ok, effects, result} ->
          IO.puts("Order processed successfully!")
          IO.inspect(effects)
        {:error, stage, reason, effects} ->
          IO.puts("Order failed at \#{stage}: \#{reason}")
      end

  """
  def process_order(params) do
    Logger.info("Starting order processing for order #{params.order_id}")

    transaction =
      TCC.new(timeout: 60_000, retry_limit: 3)
      |> TCC.run(:payment,
                 &PaymentService.try_payment/2,
                 &PaymentService.confirm_payment/2,
                 &PaymentService.cancel_payment/2)
      |> TCC.run(:inventory,
                 &InventoryService.try_reserve_inventory/2,
                 &InventoryService.confirm_reserve_inventory/2,
                 &InventoryService.cancel_reserve_inventory/2)
      |> TCC.run(:shipping,
                 &ShippingService.try_arrange_shipping/2,
                 &ShippingService.confirm_arrange_shipping/2,
                 &ShippingService.cancel_arrange_shipping/2)

    case TCC.execute(transaction, params) do
      {:ok, effects, result} ->
        Logger.info("Order #{params.order_id} completed successfully")
        {:ok, effects, result}

      {:error, stage, reason, effects} ->
        Logger.error("Order #{params.order_id} failed at #{stage}: #{inspect(reason)}")
        {:error, stage, reason, effects}
    end
  end

  @doc """
  Example of a failing order (insufficient inventory).
  """
  def process_order_with_failure do
    params = %{
      order_id: "ORD-002",
      product_id: "PROD-456",
      quantity: 1000,  # Too much inventory requested
      amount: 99.99,
      address: "456 Elm St, City, Country"
    }

    process_order(params)
  end

  @doc """
  Example of running multiple orders concurrently.
  """
  def process_orders_concurrently(order_params_list) do
    tasks = Enum.map(order_params_list, fn params ->
      Task.async(fn -> process_order(params) end)
    end)

    Task.await_many(tasks, :infinity)
  end
end

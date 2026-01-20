defmodule Inventory.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Inventory, []},
      {Plug.Cowboy, scheme: :http, plug: Inventory.Router, options: [port: port()]}
    ]

    opts = [strategy: :one_for_one, name: Inventory.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp port, do: System.get_env("PORT", "4002") |> String.to_integer()
end

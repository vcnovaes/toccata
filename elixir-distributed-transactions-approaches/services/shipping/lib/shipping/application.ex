defmodule Shipping.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Shipping, []},
      {Plug.Cowboy, scheme: :http, plug: Shipping.Router, options: [port: port()]}
    ]
    opts = [strategy: :one_for_one, name: Shipping.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp port, do: System.get_env("PORT", "4003") |> String.to_integer()
end

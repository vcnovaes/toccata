defmodule Payment.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Payment, []},
      {Plug.Cowboy, scheme: :http, plug: Payment.Router, options: [port: port()]}
    ]

    opts = [strategy: :one_for_one, name: Payment.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp port do
    System.get_env("PORT", "4001") |> String.to_integer()
  end
end

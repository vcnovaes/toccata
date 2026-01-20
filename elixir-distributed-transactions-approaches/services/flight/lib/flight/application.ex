defmodule Flight.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Flight, []},
      {Plug.Cowboy, scheme: :http, plug: Flight.Router, options: [port: port()]}
    ]
    opts = [strategy: :one_for_one, name: Flight.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp port, do: System.get_env("PORT", "4005") |> String.to_integer()
end

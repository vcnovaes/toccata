defmodule Hotel.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Hotel, []},
      {Plug.Cowboy, scheme: :http, plug: Hotel.Router, options: [port: port()]}
    ]
    opts = [strategy: :one_for_one, name: Hotel.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp port, do: System.get_env("PORT", "4004") |> String.to_integer()
end

defimpl Inspect, for: TCC.Transaction do
  import Inspect.Algebra

  @try_symbol "try→"
  @confirm_symbol "confirm→"
  @cancel_symbol "cancel←"
  @async_symbol "(async)"

  def inspect(transaction, opts) do
    list = to_list(transaction)

    left =
      concat(["#TCC.Transaction", format_cancel_error_handler(transaction.on_cancel_error), "<"])

    container_doc(left, list, ">", opts, fn str, _ -> str end)
  end

  defp to_list(transaction) do
    actions = Enum.map(transaction.actions, &format_action/1)
    final_hooks = Enum.map(transaction.final_hooks, &concat("finally: ", format_final_hook(&1)))
    tracers = Enum.map(transaction.tracers, &concat("tracer: ", inspect_module(&1)))
    Enum.concat([actions, final_hooks, tracers])
  end

  defp format_action(action) do
    name = "#{Atom.to_string(action.name)}: "
    group(concat([name, nest(build_action(action), String.length(name))]))
  end

  defp build_action(action) do
    try_part = concat([@try_symbol, " ", format_callback(action.try_fun)])
    confirm_part = concat([@confirm_symbol, " ", format_callback(action.confirm_fun)])
    cancel_part = concat([@cancel_symbol, " ", format_callback(action.cancel_fun)])
    async_part = if action.async, do: concat([" ", @async_symbol]), else: ""
    opts_part = format_action_opts(action.opts)

    glue(
      glue(
        glue(concat([try_part, async_part, opts_part]), confirm_part),
        cancel_part
      ),
      ""
    )
  end

  defp format_cancel_error_handler(:raise), do: ""
  defp format_cancel_error_handler(handler), do: concat(["(with ", Kernel.inspect(handler), ")"])

  defp format_action_opts(opts) when opts == %{}, do: ""
  defp format_action_opts(opts), do: concat([" ", Kernel.inspect(Map.to_list(opts))])

  defp format_callback({module, function, args}) do
    concat([inspect_module(module), ".", Kernel.to_string(function), format_args(args)])
  end

  defp format_callback(function) when is_function(function) do
    Kernel.inspect(function)
  end

  defp format_final_hook({module, function, args}) do
    concat([inspect_module(module), ".", Kernel.to_string(function), format_args(args)])
  end

  defp format_final_hook(function) when is_function(function) do
    Kernel.inspect(function)
  end

  defp format_args([]), do: "/2"

  defp format_args(args),
    do: concat(["(effects, params, ", Enum.map_join(args, ", ", &Kernel.inspect/1), ")"])

  defp inspect_module(module), do: Kernel.inspect(module)
end

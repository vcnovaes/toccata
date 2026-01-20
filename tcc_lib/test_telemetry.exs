# test_telemetry.exs
# Script para testar e visualizar os eventos de telemetria da biblioteca TCC
#
# Execute com: mix run test_telemetry.exs

IO.puts("""
╔═══════════════════════════════════════════════════════════════╗
║           TESTE DE TELEMETRIA - BIBLIOTECA TOCCATA            ║
╚═══════════════════════════════════════════════════════════════╝
""")

# Attach telemetry handler ANTES de executar a transação
:telemetry.attach_many(
  "test-handler",
  [
    [:tcc, :transaction, :start],
    [:tcc, :transaction, :stop],
    [:tcc, :action, :start],
    [:tcc, :action, :stop],
    [:tcc, :action, :exception]
  ],
  fn event, measurements, metadata, _config ->
    event_name = event |> Enum.join(".") |> String.upcase()

    duration_str =
      case Map.get(measurements, :duration) do
        nil -> ""
        d -> " | duração: #{System.convert_time_unit(d, :native, :millisecond)}ms"
      end

    status_str =
      case Map.get(metadata, :status) do
        nil -> ""
        s -> " | status: #{s}"
      end

    action_str =
      case Map.get(metadata, :action) do
        nil -> ""
        a -> " | action: #{a}"
      end

    phase_str =
      case Map.get(metadata, :phase) do
        nil -> ""
        p -> " | phase: #{p}"
      end

    tx_id_str =
      case Map.get(metadata, :transaction_id) do
        nil -> ""
        id -> " | tx_id: #{String.slice(id, 0, 12)}..."
      end

    IO.puts("📊 #{event_name}#{tx_id_str}#{action_str}#{phase_str}#{status_str}#{duration_str}")
  end,
  nil
)

# Definir funções de exemplo
try_fn = fn effects, params ->
  IO.puts("   → Executando TRY...")
  # Simula latência
  Process.sleep(100)
  {:ok, Map.put(effects, :tried, true), params}
end

confirm_fn = fn effects, params ->
  IO.puts("   → Executando CONFIRM...")
  Process.sleep(50)
  {:ok, Map.put(effects, :confirmed, true), params}
end

cancel_fn = fn effects, params ->
  IO.puts("   → Executando CANCEL...")
  {:ok, Map.put(effects, :cancelled, true), params}
end

# ============================================
# TESTE 1: Transação com sucesso
# ============================================
IO.puts("""

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🧪 TESTE 1: Transação com SUCESSO (2 etapas)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
""")

result =
  TCC.new()
  |> TCC.run(:payment, try_fn, confirm_fn, cancel_fn)
  |> TCC.run(:inventory, try_fn, confirm_fn, cancel_fn)
  |> TCC.execute(%{amount: 100})

case result do
  {:ok, effects, _params} ->
    IO.puts("\n✅ Transação concluída com SUCESSO!")
    IO.puts("   Effects: #{inspect(effects)}")

  {:error, stage, reason, _effects} ->
    IO.puts("\n❌ Transação FALHOU no estágio #{stage}: #{inspect(reason)}")
end

# ============================================
# TESTE 2: Transação com falha (rollback)
# ============================================
IO.puts("""

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🧪 TESTE 2: Transação com FALHA (rollback automático)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
""")

# Try que falha
try_failing = fn _effects, _params ->
  IO.puts("   💥 Simulando FALHA no TRY...")
  {:error, :insufficient_funds}
end

result2 =
  TCC.new()
  |> TCC.run(:payment, try_fn, confirm_fn, cancel_fn)
  |> TCC.run(:inventory, try_failing, confirm_fn, cancel_fn)
  |> TCC.execute(%{amount: 100})

case result2 do
  {:ok, effects, _params} ->
    IO.puts("\n✅ Transação concluída com SUCESSO!")
    IO.puts("   Effects: #{inspect(effects)}")

  {:error, stage, reason, effects} ->
    IO.puts("\n❌ Transação FALHOU no estágio :#{stage}")
    IO.puts("   Razão: #{inspect(reason)}")
    IO.puts("   Effects após rollback: #{inspect(effects)}")
end

IO.puts("""

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✨ Testes de telemetria concluídos!
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
""")

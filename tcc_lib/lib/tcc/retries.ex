defmodule TCC.Retries do
  @moduledoc """
  This module implements retry logic with exponential back-off for confirm and cancel phases.

  ## Retry Options

  * `:retry_limit` - Maximum number of retry attempts (required)
  * `:base_backoff` - Base backoff time in milliseconds (default: nil, no backoff)
  * `:max_backoff` - Maximum backoff time in milliseconds (default: 5000)
  * `:enable_jitter` - Whether to add randomization to backoff (default: true)

  ## Backoff Calculation

  For exponential backoff this formula is used:

      min(max_backoff, (base_backoff * 2) ^ retry_count)

  Example without jitter:

  | Attempt | Base Backoff | Max Backoff | Sleep time |
  |---------|--------------|-------------|------------|
  | 1       | 10           | 30000       | 20         |
  | 2       | 10           | 30000       | 400        |
  | 3       | 10           | 30000       | 8000       |
  | 4       | 10           | 30000       | 30000      |

  When jitter is enabled, backoff value is randomized:

      random(0, min(max_backoff, (base_backoff * 2) ^ retry_count))

  Example with jitter:

  | Attempt | Base Backoff | Max Backoff | Sleep interval |
  |---------|--------------|-------------|----------------|
  | 1       | 10           | 30000       | 0..20          |
  | 2       | 10           | 30000       | 0..400         |
  | 3       | 10           | 30000       | 0..8000        |
  | 4       | 10           | 30000       | 0..30000       |

  For more reasoning behind using jitter, check out
  [this blog post](https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/).
  """

  require Logger

  @type retry_opts :: [
          {:retry_limit, pos_integer()},
          {:base_backoff, pos_integer() | nil},
          {:max_backoff, pos_integer()},
          {:enable_jitter, boolean()}
        ]

  @doc """
  Returns `true` if the operation should be retried, `false` otherwise.

  Optionally delays with exponential backoff based on retry options.
  Malformed retry options are logged and ignored.
  """
  @spec retry_with_backoff?(count :: pos_integer(), opts :: retry_opts() | map()) :: boolean()
  def retry_with_backoff?(count, opts) when is_list(opts) do
    retry_with_backoff?(count, Map.new(opts))
  end

  def retry_with_backoff?(count, opts) when is_map(opts) do
    limit = Map.get(opts, :retry_limit)

    if is_integer(limit) and limit > count do
      base_backoff = Map.get(opts, :base_backoff)
      max_backoff = Map.get(opts, :max_backoff, 5_000)
      jitter_enabled? = Map.get(opts, :enable_jitter, true)

      backoff = get_backoff(count, base_backoff, max_backoff, jitter_enabled?)
      :ok = maybe_sleep(backoff)
      true
    else
      false
    end
  end

  @doc """
  Calculates the backoff time for a given retry attempt.

  This function is public for testing purposes.
  """
  @spec get_backoff(
          count :: pos_integer(),
          base_backoff :: pos_integer() | nil,
          max_backoff :: pos_integer() | nil,
          jitter_enabled :: boolean()
        ) :: non_neg_integer()
  def get_backoff(_count, nil, _max_backoff, _jitter_enabled?) do
    0
  end

  def get_backoff(count, base_backoff, max_backoff, true)
      when is_integer(base_backoff) and base_backoff >= 1 and
             is_integer(max_backoff) and max_backoff >= 1 do
    random(calculate_backoff(count, base_backoff, max_backoff))
  end

  def get_backoff(count, base_backoff, max_backoff, _jitter_enabled?)
      when is_integer(base_backoff) and base_backoff >= 1 and
             is_integer(max_backoff) and max_backoff >= 1 do
    calculate_backoff(count, base_backoff, max_backoff)
  end

  def get_backoff(_count, base_backoff, max_backoff, _jitter_enabled?) do
    Logger.warning(
      "[TCC] Ignoring retry backoff options, expected base_backoff and max_backoff to be integer and >= 1, got: " <>
        "base_backoff: #{inspect(base_backoff)}, max_backoff: #{inspect(max_backoff)}"
    )

    0
  end

  defp calculate_backoff(count, base_backoff, max_backoff) do
    min(max_backoff, trunc(:math.pow(base_backoff * 2, count)))
  end

  defp random(n) when is_integer(n) and n > 0, do: :rand.uniform(n) - 1
  defp random(n) when is_integer(n), do: 0

  defp maybe_sleep(0), do: :ok
  defp maybe_sleep(backoff), do: :timer.sleep(backoff)
end

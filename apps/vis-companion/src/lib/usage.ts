import type { TranscriptTurn } from './types';

/**
 * Turn usage math and the canonical `meta-tokens` / `meta-cost` renderers shared
 * with the TUI: tokens read `11.5k→35 ↺ 4.1k` and cost reads `~$0.0070`.
 */

export interface Usage {
  input: number;
  output: number;
  cached: number;
  cost: number;
}

function finiteNumber(...values: unknown[]): number | undefined {
  return values.find((value): value is number => typeof value === 'number' && Number.isFinite(value));
}

/** 35 → `35`, 11461 → `11.5k`, 2000000 → `2M`. One decimal, trailing `.0` dropped. */
export function humanizeCount(value: number): string {
  const count = Math.trunc(value);
  if (count < 1_000) return String(count);
  const scale = count < 1_000_000 ? 1_000 : 1_000_000;
  const unit = count < 1_000_000 ? 'k' : 'M';
  const tenths = Math.round(count / (scale / 10));
  const whole = Math.floor(tenths / 10);
  const fraction = tenths % 10;
  return `${whole}${fraction ? `.${fraction}` : ''}${unit}`;
}

/**
 * `11.5k→35`, with ` ↺ 4.1k` only when cached input is positive. Null for
 * a zero-usage turn so a failed provider call never renders a bare `0→0`.
 */
export function formatTokens({ input, output, cached }: { input?: number; output?: number; cached?: number }): string | null {
  const inTok = input ?? 0;
  const outTok = output ?? 0;
  if (inTok <= 0 && outTok <= 0) return null;
  const cache = cached && cached > 0 ? ` ↺ ${humanizeCount(cached)}` : '';
  return `${humanizeCount(inTok)}→${humanizeCount(outTok)}${cache}`;
}

/** `~$1.23` / `~$0.0070` / `~$0.000042`. Null for zero or missing — never `$0`. */
export function formatCost(value?: number): string | null {
  if (typeof value !== 'number' || !Number.isFinite(value) || value <= 0) return null;
  return `~$${value.toFixed(value >= 1 ? 2 : value >= 0.0001 ? 4 : 6)}`;
}


/**
 * One turn's totals, memoized on the turn OBJECT. A decoded turn never mutates,
 * and `GatewayClient.transcript` hands back the SAME object for a row the
 * gateway repeated, so re-rendering a long session re-reads these numbers
 * instead of re-deriving them per bubble, per frame.
 */
const turnTotals = new WeakMap<TranscriptTurn, Usage>();

/** One turn's token + cost slots, tolerating every shape the wire has carried. */
export function turnUsage(turn: TranscriptTurn): Usage {
  const memo = turnTotals.get(turn);
  if (memo) return memo;
  const costMap = typeof turn.cost === 'object' && turn.cost ? turn.cost : undefined;
  const usage = {
    input: finiteNumber(turn.tokens?.input, turn.input_tokens) ?? 0,
    output: finiteNumber(turn.tokens?.output, turn.output_tokens) ?? 0,
    cached: finiteNumber(turn.tokens?.cached, turn.input_cache_read_tokens) ?? 0,
    cost:
      finiteNumber(turn.total_cost, typeof turn.cost === 'number' ? turn.cost : undefined, costMap?.total_cost) ?? 0,
  };
  turnTotals.set(turn, usage);
  return usage;
}


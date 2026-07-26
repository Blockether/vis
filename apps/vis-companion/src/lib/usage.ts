import type { TranscriptTurn } from './types';

/**
 * Session usage math, mirroring the TUI footer's `session-usage`
 * (`channel_tui/footer.clj`) and the canonical `meta-tokens` / `meta-cost`
 * renderers in `internal/format.clj`, so the companion's composer strip and the
 * TUI's footer can never drift in shape: tokens read `11.5k→35 (cached 4.1k)`
 * and cost reads `~$0.0070`.
 */

export interface Usage {
  input: number;
  output: number;
  cached: number;
  cost: number;
  /** Turns that actually reported usage — the cumulative denominator. */
  turns: number;
}

export const EMPTY_USAGE: Usage = { input: 0, output: 0, cached: 0, cost: 0, turns: 0 };

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
 * `11.5k→35`, with ` (cached 4.1k)` only when cached input is positive. Null for
 * a zero-usage turn so a failed provider call never renders a bare `0→0`.
 */
export function formatTokens({ input, output, cached }: { input?: number; output?: number; cached?: number }): string | null {
  const inTok = input ?? 0;
  const outTok = output ?? 0;
  if (inTok <= 0 && outTok <= 0) return null;
  const cache = cached && cached > 0 ? ` (cached ${humanizeCount(cached)})` : '';
  return `${humanizeCount(inTok)}→${humanizeCount(outTok)}${cache}`;
}

/** `~$1.23` / `~$0.0070` / `~$0.000042`. Null for zero or missing — never `$0`. */
export function formatCost(value?: number): string | null {
  if (typeof value !== 'number' || !Number.isFinite(value) || value <= 0) return null;
  return `~$${value.toFixed(value >= 1 ? 2 : value >= 0.0001 ? 4 : 6)}`;
}

/** Exact figure for the hover/long-press detail, where humanizing would lie. */
export function exactCost(value: number): string {
  return `$${value.toFixed(value >= 1 ? 4 : 6)}`;
}

/**
 * One turn's totals, memoized on the turn OBJECT. A decoded turn never mutates,
 * and `GatewayClient.transcript` hands back the SAME object for a row the
 * gateway repeated, so re-rendering a long session re-reads these numbers
 * instead of re-deriving them per bubble, per frame.
 */
const turnTotals = new WeakMap<TranscriptTurn, Omit<Usage, 'turns'>>();

/** One turn's token + cost slots, tolerating every shape the wire has carried. */
export function turnUsage(turn: TranscriptTurn): Omit<Usage, 'turns'> {
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

/**
 * Running totals of the LAST transcript folded: `prefix[i]` is the cumulative
 * usage through turn `i`. Turn objects stay identity-stable across a refetch, so
 * an update that only appends to (or rewrites the tail of) a long session reuses
 * the shared prefix instead of re-folding the whole history.
 */
let prefixCache: { turns: readonly TranscriptTurn[]; prefix: Usage[] } | null = null;

/**
 * Cumulative usage across a session's transcript — the SAME fold the TUI footer
 * runs over its message vector (`footer/session-usage`), so both surfaces read
 * one number. Costs O(turns that actually changed), not O(session), for every
 * update after the first.
 */
export function sessionUsage(turns: TranscriptTurn[]): Usage {
  const cached = prefixCache;
  let shared = 0;
  if (cached) {
    const limit = Math.min(cached.turns.length, turns.length);
    while (shared < limit && cached.turns[shared] === turns[shared]) shared += 1;
  }
  if (cached && shared === turns.length && shared === cached.turns.length) {
    return prefixTotal(cached.prefix, shared);
  }
  const prefix = cached ? cached.prefix.slice(0, shared) : [];
  let total = prefixTotal(prefix, shared);
  for (let index = shared; index < turns.length; index += 1) {
    const usage = turnUsage(turns[index]);
    const reported = usage.input > 0 || usage.output > 0 || usage.cost > 0;
    total = {
      input: total.input + usage.input,
      output: total.output + usage.output,
      cached: total.cached + usage.cached,
      cost: total.cost + usage.cost,
      turns: total.turns + (reported ? 1 : 0),
    };
    prefix.push(total);
  }
  prefixCache = { turns, prefix };
  return total;
}

/** Cumulative total through `count` turns — empty usage when nothing is shared. */
function prefixTotal(prefix: Usage[], count: number): Usage {
  return count > 0 ? (prefix[count - 1] ?? EMPTY_USAGE) : EMPTY_USAGE;
}

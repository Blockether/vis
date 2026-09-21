/**
 * Addresses that did not answer, so a background chore stops asking them again
 * on every wake.
 *
 * One machine is known by several addresses at once, and the silent ones do not
 * go away: a LAN address from another network, a tailnet IP of a machine that
 * was re-imaged, a DHCP lease that has moved on. Each question costs a full
 * request timeout, and the sweeps asked them again on every resume — a
 * diagnostics export from one phone held over 800 such attempts in fourteen
 * hours, none of which ever answered, while the reads that mattered queued
 * behind them.
 *
 * So a silent address is remembered and asked again later, with the gap
 * doubling while it stays silent. This memory is deliberately NOT durable: a
 * relaunch is a fair reason to try everything once more, and
 * `forgetUnreachableAddresses` clears it the moment the device joins a
 * different network, which is exactly when a silent address can answer again.
 */
import { normalizeAddress } from './endpoints';

/** Gap after the first silence. It doubles per miss, up to `MAX_BACKOFF_MS`. */
const FIRST_BACKOFF_MS = 60_000;
const MAX_BACKOFF_MS = 30 * 60_000;

const silent = new Map<string, { misses: number; nextProbeAt: number }>();

/** Is this address worth one request right now? */
export function isProbeDue(url: string, now: number = Date.now()): boolean {
  const held = silent.get(normalizeAddress(url));
  return !held || now >= held.nextProbeAt;
}

/** This address did not answer: ask it again after the next gap. */
export function noteUnreachable(url: string, now: number = Date.now()): void {
  const key = normalizeAddress(url);
  const misses = (silent.get(key)?.misses ?? 0) + 1;
  const gap = Math.min(FIRST_BACKOFF_MS * 2 ** (misses - 1), MAX_BACKOFF_MS);
  silent.set(key, { misses, nextProbeAt: now + gap });
}

/** This address answered: it is an ordinary address again. */
export function noteReachable(url: string): void {
  silent.delete(normalizeAddress(url));
}

/** A different network revives every silent address at once. */
export function forgetUnreachableAddresses(): void {
  silent.clear();
}

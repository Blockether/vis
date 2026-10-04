// Keep failed connection attempts across app restarts. Three consecutive failures
// hide a machine until it answers. Starting another probe does not restore it.
// The session list reads this state synchronously, before its first render.

/** A machine's last failure and consecutive failed attempts. */
export interface MachineOutage {
  why: string;
  at: number;
  misses: number;
}

const STORAGE_KEY = 'vis.fleet-outage.v1';
const HIDING_MISSES = 3;

function storage(): Storage | null {
  try {
    return globalThis.localStorage ?? null;
  } catch {
    // An unavailable store cannot preserve failures across app restarts.
    return null;
  }
}

function readAll(): Record<string, MachineOutage> {
  try {
    const raw = storage()?.getItem(STORAGE_KEY);
    if (!raw) return {};
    const parsed: unknown = JSON.parse(raw);
    if (!parsed || typeof parsed !== 'object') return {};
    const out: Record<string, MachineOutage> = {};
    for (const [url, value] of Object.entries(parsed as Record<string, unknown>)) {
      const entry = value as Partial<MachineOutage> | null;
      if (!entry || typeof entry.why !== 'string' || typeof entry.at !== 'number') continue;
      const misses =
        typeof entry.misses === 'number' && Number.isSafeInteger(entry.misses) && entry.misses > 0
          ? Math.min(entry.misses, HIDING_MISSES)
          : 1;
      out[url] = { why: entry.why, at: entry.at, misses };
    }
    return out;
  } catch {
    return {};
  }
}

function writeAll(all: Record<string, MachineOutage>): void {
  try {
    // Time and failures on other machines must never restore an unavailable machine.
    storage()?.setItem(STORAGE_KEY, JSON.stringify(all));
  } catch {
    // An unavailable store must not prevent the session list from loading.
  }
}

/** Return the last failure, or null if the machine has no recorded failure. */
export function machineOutage(url: string): string | null {
  return readAll()[url]?.why ?? null;
}

/** Keep a repeatedly unavailable machine out of the switch until it answers. */
export function isMachineHidden(url: string): boolean {
  return (readAll()[url]?.misses ?? 0) >= HIDING_MISSES;
}

/** Record one failed attempt and return the consecutive count, capped at three. */
export function rememberMachineOutage(url: string, why: string): number {
  const all = readAll();
  const held = all[url];
  const misses = Math.min((held?.misses ?? 0) + 1, HIDING_MISSES);
  if (held?.why === why && held.misses === misses) return misses;
  all[url] = { why, at: Date.now(), misses };
  writeAll(all);
  return misses;
}

/** A successful response restores the machine and resets its failure count. */
export function clearMachineOutage(url: string): void {
  const all = readAll();
  if (!(url in all)) return;
  delete all[url];
  writeAll(all);
}

// MACHINES THIS DEVICE HAS ALREADY FOUND DARK, kept where the NEXT run can read them.
//
// A gateway that is not answering is drained out of the sessions list: no section, no
// rows, and a tile that is the retry rather than a place to go (`scopedMachines`,
// `MachineTab isDown`). That verdict was a module-level `Map` in `SessionsScreen`, so it
// lived exactly as long as the JavaScript context did — and on iOS/Android the OS kills a
// backgrounded webview routinely, which makes "open the app" a COLD start. Reported: a
// machine that had been off for hours came back as a machine nobody had ever tried, raised
// and pressable in the strip, and only fell out of the fleet again once its socket ran out
// the transport's 30s deadline — every single launch.
//
// So the verdict is SAVED. What it means is unchanged: a machine known dark starts drained
// and walks back in when it ANSWERS, never because it is being asked again — the poll
// reconnects it beside the fleet's own load, and one answer clears the entry.
//
// `localStorage` and not Capacitor Preferences: the sessions list seeds its fleet in a React
// state initializer, which cannot await (the same reason `snapshot-store.ts` and
// `notify-verdict.ts` are here). It is remembered, never authoritative — a device with no
// storage simply goes back to trying every machine from scratch.

/** One machine's last confirmed darkness: the transport's own reason, and when. */
export interface MachineOutage {
  why: string;
  at: number;
}

const STORAGE_KEY = 'vis.fleet-outage.v1';

// A machine that was unpaired months ago must not keep a row here forever, and a phone that
// has met a lot of gateways must not grow this without bound. Both are swept on write.
const MAX_AGE_MS = 30 * 24 * 60 * 60 * 1000;
const MAX_ENTRIES = 32;

function storage(): Storage | null {
  try {
    return globalThis.localStorage ?? null;
  } catch {
    // Private mode / disabled storage: every machine is simply untried again.
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
      out[url] = { why: entry.why, at: entry.at };
    }
    return out;
  } catch {
    return {};
  }
}

function writeAll(all: Record<string, MachineOutage>): void {
  const fresh = Object.entries(all)
    .filter(([, entry]) => Date.now() - entry.at < MAX_AGE_MS)
    .sort(([, a], [, b]) => b.at - a.at)
    .slice(0, MAX_ENTRIES);
  try {
    storage()?.setItem(STORAGE_KEY, JSON.stringify(Object.fromEntries(fresh)));
  } catch {
    // A full or unavailable store costs one launch's worth of probing, nothing more.
  }
}

/** Why this device last found that machine dark, or `null` if it has not. */
export function machineOutage(url: string): string | null {
  return readAll()[url]?.why ?? null;
}

/** This machine's darkness is CONFIRMED — remember it for the runs after this one. */
export function rememberMachineOutage(url: string, why: string): void {
  const all = readAll();
  if (all[url]?.why === why) return;
  all[url] = { why, at: Date.now() };
  writeAll(all);
}

/** It spoke. Nothing about a machine that answers is dark. */
export function clearMachineOutage(url: string): void {
  const all = readAll();
  if (!(url in all)) return;
  delete all[url];
  writeAll(all);
}

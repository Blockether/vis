// What the notifications row already said, per machine — read SYNCHRONOUSLY.
//
// Reported: opening Settings makes the notifications row flash. The panel's whole
// verdict is assembled from four asynchronous answers (the machine's device list,
// the OS permission, this device's stored switch, the ids it is registered under),
// so its first frame could only say `Checking…` — a pulsing amber `Connect` that
// turns into a quiet `Disconnect` a moment later, every single time the dialog is
// opened, on a question whose answer had not changed since the last open.
//
// The answer is one boolean per gateway and it is durable enough to paint first:
// what changes it is a press in this panel, or a sweep this app runs itself. So it
// is remembered when it settles and re-read on the next mount, which makes the
// first frame the settled one and the network round trip a silent revalidation.
//
// `localStorage` and not Capacitor Preferences: a React state initializer cannot
// await, and only `localStorage` answers synchronously (same reason as
// `snapshot-store.ts`). It is a cache — a browser that has none simply goes back
// to `Checking…`.

const STORAGE_KEY = 'vis.notify-verdicts.v1';

function readAll(): Record<string, boolean> {
  try {
    const raw = globalThis.localStorage?.getItem(STORAGE_KEY);
    if (!raw) return {};
    const parsed: unknown = JSON.parse(raw);
    if (!parsed || typeof parsed !== 'object') return {};
    const out: Record<string, boolean> = {};
    for (const [url, value] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof value === 'boolean') out[url] = value;
    }
    return out;
  } catch {
    return {};
  }
}

function writeAll(all: Record<string, boolean>): void {
  try {
    globalThis.localStorage?.setItem(STORAGE_KEY, JSON.stringify(all));
  } catch {
    // A full or unavailable store costs a `Checking…` frame, nothing more.
  }
}

/**
 * Whether this device was connected to that machine's notifications the last time
 * the panel settled — `null` when this device has never been told.
 */
export function cachedNotifyVerdict(url: string): boolean | null {
  const held = readAll()[url];
  return typeof held === 'boolean' ? held : null;
}

/** Record the verdict the panel just settled on for one machine. */
export function rememberNotifyVerdict(url: string, isOn: boolean): void {
  const all = readAll();
  if (all[url] === isOn) return;
  all[url] = isOn;
  writeAll(all);
}

/** Drop a machine's verdict — it was forgotten, so its answer is gone with it. */
export function forgetNotifyVerdict(url: string): void {
  const all = readAll();
  if (!(url in all)) return;
  delete all[url];
  writeAll(all);
}

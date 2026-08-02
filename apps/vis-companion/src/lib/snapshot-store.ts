// Durable mirror of the gateway client's per-session snapshot cache.
//
// `gateway.ts` keeps the last payload it saw per gateway+resource in a plain
// in-memory Map, so a screen that REMOUNTS repaints instantly. That cache dies
// with the JavaScript context — and on iOS/Android the OS kills a backgrounded
// webview routinely, so "open the app" is almost always a COLD start: no rows,
// the loading veil, and a full transcript page pulled over the phone's network
// before anything is on screen.
//
// This module writes the same snapshots through to `localStorage` (the only
// storage a React initializer can read SYNCHRONOUSLY — Capacitor Preferences is
// async, and an async read cannot seed a first frame). A cold start then paints
// the last known transcript immediately and revalidates underneath: the meta row
// is one tiny request, and `transcriptIfMoved` downloads NOTHING when no turn
// was persisted since.
//
// What is deliberately NOT persisted:
//   • the queued backlog — a queue row that drained while the app was dead would
//     come back from disk as a ghost;
//   • live SSE frames — the in-flight turn is replayed by the gateway from its
//     `turn.started`, which is exactly the "only the newest stuff" a cold client
//     is missing;
//   • the full settings sheet and capabilities — cheap to refetch and
//     version-sensitive. The by-id toggles and model picks below ARE persisted:
//     they are two short strings each and they name what the composer footer
//     paints on its first frame.

/** How much of a session's history one snapshot holds (mirrors gateway.ts). */
export interface HeldWindow {
  offset: number;
  total: number;
}

/** The three live caches `gateway.ts` owns, mirrored as one unit. */
export interface SnapshotStores {
  snapshots: Map<string, unknown>;
  stamps: Map<string, string>;
  windows: Map<string, HeldWindow>;
}

const STORAGE_KEY = 'vis.snapshots.v1';

/**
 * Resource kinds worth surviving an app kill. `setting`/`model`/`model-default`
 * are tiny and are what the composer footer paints before anything else: a cold
 * start would otherwise show a nameless "model" chip and no reasoning chip at
 * all until a `/v1/router` probe (seconds on a cold daemon) came back. `theme`
 * is the palette catalog of one gateway: without it the application settings
 * dialog opens on the bundled pair and jumps once every paired machine answers.
 */
const DURABLE_KINDS = new Set([
  'sessions',
  'session',
  'transcript',
  'setting',
  'model',
  'model-default',
  'theme',
]);

// A transcript page is 24 turns and a single turn can be hundreds of kilobytes
// of tool output. Persist only the newest few per session: enough to paint the
// screen you left, cheap enough that a dozen sessions still fit. Everything
// older is one "load earlier" away and stays on the gateway.
const MAX_TURNS_PER_SESSION = 8;

// localStorage quotas start around 5 MB and a QuotaExceededError is thrown for
// the WHOLE write, so stay well under and shed history instead of losing the
// entire cache.
const MAX_BYTES = 1_000_000;

// Snapshots are written in bursts (list, meta and transcript land together).
// Coalesce them; the hide listeners below cover a kill inside the window.
const FLUSH_DEBOUNCE_MS = 400;

interface PersistedShape {
  v: number;
  snapshots: Record<string, unknown>;
  stamps: Record<string, string>;
  windows: Record<string, HeldWindow>;
}

function kindOf(key: string): string {
  return key.split('\u0000')[1] ?? '';
}

function storage(): Storage | null {
  try {
    return globalThis.localStorage ?? null;
  } catch {
    // Private mode / disabled storage: the memory cache still works.
    return null;
  }
}

/**
 * Seed the in-memory caches from the last session's persisted copy. Never
 * overwrites an entry the running app already holds — the live value is always
 * fresher than the disk one.
 */
export function hydrateSnapshots(stores: SnapshotStores): void {
  const store = storage();
  const raw = store?.getItem(STORAGE_KEY);
  if (!raw) return;
  try {
    const parsed = JSON.parse(raw) as Partial<PersistedShape>;
    for (const [key, value] of Object.entries(parsed.snapshots ?? {})) {
      if (!stores.snapshots.has(key)) stores.snapshots.set(key, value);
    }
    for (const [key, value] of Object.entries(parsed.stamps ?? {})) {
      if (typeof value === 'string' && !stores.stamps.has(key)) stores.stamps.set(key, value);
    }
    for (const [key, value] of Object.entries(parsed.windows ?? {})) {
      if (value && typeof value.offset === 'number' && !stores.windows.has(key)) {
        stores.windows.set(key, { offset: value.offset, total: value.total ?? 0 });
      }
    }
  } catch {
    // A corrupt blob must never keep the app from starting.
    store?.removeItem(STORAGE_KEY);
  }
}

function serialize(stores: SnapshotStores): string {
  const snapshots: Record<string, unknown> = {};
  const stamps: Record<string, string> = {};
  const windows: Record<string, HeldWindow> = {};
  // Map iteration order IS the LRU order in gateway.ts: oldest first, so the
  // budget loop below sheds the least recently used session first.
  const transcriptKeys: string[] = [];
  for (const [key, value] of stores.snapshots) {
    const kind = kindOf(key);
    if (!DURABLE_KINDS.has(kind)) continue;
    if (kind !== 'transcript') {
      snapshots[key] = value;
      continue;
    }
    const turns = Array.isArray(value) ? (value as unknown[]) : [];
    const kept = turns.slice(-MAX_TURNS_PER_SESSION);
    const dropped = turns.length - kept.length;
    snapshots[key] = kept;
    transcriptKeys.push(key);
    const held = stores.windows.get(key);
    // The window counts rows that exist BEFORE the ones we hold, so trimming the
    // head has to be added back — otherwise "load earlier" would skip them.
    if (held) windows[key] = { offset: held.offset + dropped, total: held.total };
    const stamp = stores.stamps.get(key);
    if (stamp) stamps[key] = stamp;
  }

  let payload = JSON.stringify({ v: 1, snapshots, stamps, windows } satisfies PersistedShape);
  for (const key of transcriptKeys) {
    if (payload.length <= MAX_BYTES) break;
    delete snapshots[key];
    delete stamps[key];
    delete windows[key];
    payload = JSON.stringify({ v: 1, snapshots, stamps, windows } satisfies PersistedShape);
  }
  return payload;
}

let timer: ReturnType<typeof setTimeout> | null = null;

/** Write the durable snapshots now. Safe to call from a hide/pagehide handler. */
export function flushSnapshots(stores: SnapshotStores): void {
  if (timer !== null) {
    clearTimeout(timer);
    timer = null;
  }
  const store = storage();
  if (!store) return;
  try {
    store.setItem(STORAGE_KEY, serialize(stores));
  } catch {
    // Out of quota (or a hostile embedder): keep the small rows, drop history —
    // a session list that paints instantly is worth more than nothing at all.
    try {
      const lean: SnapshotStores = {
        snapshots: new Map(
          Array.from(stores.snapshots).filter(([key]) => kindOf(key) !== 'transcript'),
        ),
        stamps: new Map(),
        windows: new Map(),
      };
      store.setItem(STORAGE_KEY, serialize(lean));
    } catch {
      /* Give up silently: persistence is an optimisation, never a requirement. */
    }
  }
}

/** Coalesced write-through for a snapshot that just changed. */
export function scheduleSnapshotFlush(stores: SnapshotStores): void {
  if (timer !== null || !storage()) return;
  timer = setTimeout(() => {
    timer = null;
    flushSnapshots(stores);
  }, FLUSH_DEBOUNCE_MS);
}

let installed = false;

/**
 * Flush on the last moment the platform gives us. A backgrounded webview can be
 * killed without running another timer, so the pending debounce would be lost
 * exactly in the case this cache exists for.
 */
export function installSnapshotFlushOnHide(stores: SnapshotStores): void {
  if (installed || typeof document === 'undefined') return;
  installed = true;
  const flush = () => flushSnapshots(stores);
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') flush();
  });
  globalThis.addEventListener?.('pagehide', flush);
}

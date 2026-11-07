// Synchronous localStorage mirror of durable gateway snapshots for cold first paint.
// Persist settled transcript windows and small machine facts, then revalidate. Queued
// backlog and live SSE frames stay transient so stale rows cannot return after restart.
//
// Each snapshot is one storage item, and a flush writes only the items that changed.
// When the whole cache was one item, each changed session row wrote all of the cache
// again: about 1.8 MB for each poll. The storage log of the desktop app grew by some
// megabytes each minute and by gigabytes in a long run.

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

// The index lists the key of each saved snapshot, oldest first. It changes only when
// a key comes or goes, or when a hide writes the LRU order. The item key is
// `ITEM_PREFIX` and the JSON of the snapshot key. The item value is the body.
// Another tab or an interrupted flush can leave items of different times. Thus each
// item is complete, and a validator that describes other items must check them
// (see `listDigest` in `gateway.ts`).
const INDEX_KEY = 'vis.snapshots.v2';
const ITEM_PREFIX = `${INDEX_KEY}:`;
const EMPTY_INDEX = '{"v":2,"keys":[]}';

// The single item of earlier versions. The first flush of this version removes it.
// Thus if it is there, it is the newest copy: hydration reads it before the index.
const LEGACY_KEY = 'vis.snapshots.v1';

/**
 * Resource kinds worth surviving an app kill. `setting`/`model`/`model-default`
 * are tiny and are what the composer footer paints before anything else: a cold
 * start would otherwise show a nameless "model" chip and no reasoning chip at
 * all until a `/v1/router` probe (seconds on a cold daemon) came back. `theme`
 * is the palette catalog of one gateway: without it the application settings
 * dialog opens on the bundled pair and jumps once every paired machine answers.
 * `sessions-pin` is the session list's tiny head-window ETag, paired with its
 * persisted rows so a cold client can revalidate rather than download them again.
 */
const DURABLE_KINDS = new Set([
  'sessions',
  'sessions-pin',
  // The head window can contain only one busy project. Keep every project header
  // and each visited project's first page beside it for a complete cold paint, with
  // the first page of that project's bands, so the rows filed under them paint named.
  'projects-overview',
  'project-head',
  'project-groups',
  'session',
  'transcript',
  'setting',
  'model',
  'model-default',
  'theme',
  // The notifications panel's device list: reopened constantly, changed rarely,
  // and its absence is the `Checking…` flash the panel is not allowed to have.
  'devices',
  // A machine too old to carry `/v1/devices` says so once; remembering the
  // refusal is what stops the panel painting itself and then deleting itself.
  'devices-unsupported',
  // The rest of what the settings dialog opens on. Each one is small, changes
  // about once a week, and its absence is a panel that paints empty and fills
  // itself a round trip later — reported as the settings screen flickering.
  // `router` is also the expensive one to re-ask: a live auth + limits probe
  // per provider on the daemon.
  'settings',
  'capabilities',
  'mcp-servers',
  'router',
]);

// A transcript page is 24 turns and a single turn can be hundreds of kilobytes
// of tool output. Persist only the newest few per session: enough to paint the
// screen you left, cheap enough that a dozen sessions still fit. Everything
// older is one "load earlier" away and stays on the gateway.
const MAX_TURNS_PER_SESSION = 8;

// localStorage quotas start at about 5 MB. Stay well under, and shed history before
// the small rows. The budget counts the index and each item, with their keys.
const MAX_BYTES = 1_000_000;

// Snapshots are written in bursts (list, meta and transcript land together).
// Coalesce them; the hide listeners below cover a kill inside the window.
const FLUSH_DEBOUNCE_MS = 400;

// Polls change some rows every few seconds. A timer writes at most once in this
// interval. The hide listeners below write the last changes.
const MIN_FLUSH_INTERVAL_MS = 30_000;

interface PersistedShape {
  v: number;
  snapshots: Record<string, unknown>;
  stamps: Record<string, string>;
  windows: Record<string, HeldWindow>;
}

/** The body of one item. Only a transcript has a stamp and a window. */
interface ItemBody {
  value: unknown;
  stamp?: unknown;
  window?: Partial<HeldWindow> | null;
}

/** One snapshot as the next flush saves it. */
interface Planned {
  key: string;
  /** The storage key of its item. */
  name: string;
  body: string;
}

// The body of each item that storage holds now, as this module wrote or read it.
// A flush compares with it.
const stored = new Map<string, string>();
let storedIndex: string | null = null;
let storedKeys = new Set<string>();
// The first flush also removes the legacy item and the items that no index lists.
let swept = false;
let lastWriteAt = Number.NEGATIVE_INFINITY;

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

function itemName(key: string): string {
  return ITEM_PREFIX + JSON.stringify(key);
}

/** The keys of an index, or `null` if it is not a valid index. */
function indexKeys(raw: string): string[] | null {
  try {
    const parsed = JSON.parse(raw) as { v?: unknown; keys?: unknown } | null;
    if (parsed?.v !== 2 || !Array.isArray(parsed.keys)) return null;
    return parsed.keys.filter((key: unknown): key is string => typeof key === 'string');
  } catch {
    // A corrupt index must never keep the app from starting. The next flush replaces it.
    return null;
  }
}

function parseBody(body: string): ItemBody | null {
  try {
    const parsed = JSON.parse(body) as ItemBody | null;
    return parsed && typeof parsed === 'object' && 'value' in parsed ? parsed : null;
  } catch {
    return null;
  }
}

/** Put one restored snapshot into the live caches. A live entry always wins. */
function restore(stores: SnapshotStores, key: string, held: ItemBody): void {
  if (!stores.snapshots.has(key)) stores.snapshots.set(key, held.value);
  if (typeof held.stamp === 'string' && !stores.stamps.has(key)) stores.stamps.set(key, held.stamp);
  const window = held.window;
  if (window && typeof window.offset === 'number' && !stores.windows.has(key)) {
    stores.windows.set(key, { offset: window.offset, total: window.total ?? 0 });
  }
}

/**
 * Seed the in-memory caches from the last session's persisted copy. Never
 * overwrites an entry the running app already holds — the live value is always
 * fresher than the disk one.
 */
export function hydrateSnapshots(stores: SnapshotStores): void {
  const store = storage();
  if (!store) return;
  if (store.getItem(LEGACY_KEY) !== null) {
    hydrateLegacy(store, stores);
    return;
  }
  const raw = store.getItem(INDEX_KEY);
  const keys = raw === null ? null : indexKeys(raw);
  if (keys === null) return;
  storedIndex = raw;
  storedKeys = new Set(keys);
  for (const key of keys) {
    const body = store.getItem(itemName(key));
    const held = body === null ? null : parseBody(body);
    if (body === null || !held) continue;
    stored.set(key, body);
    restore(stores, key, held);
  }
}

/** Read the single item of an earlier version. The first flush moves it to items. */
function hydrateLegacy(store: Storage, stores: SnapshotStores): void {
  const raw = store.getItem(LEGACY_KEY);
  if (!raw) return;
  try {
    const parsed = JSON.parse(raw) as Partial<PersistedShape>;
    const stamps = parsed.stamps ?? {};
    const windows = parsed.windows ?? {};
    for (const [key, value] of Object.entries(parsed.snapshots ?? {})) {
      restore(stores, key, { value, stamp: stamps[key], window: windows[key] });
    }
  } catch {
    // A corrupt blob must never keep the app from starting.
  }
}

// Gateway reconciliation replaces changed transcript arrays instead of mutating
// them. Reuse their encoded tail when a small session poll triggers another flush.
// Weak keys release evicted history; oversized tails retain only a rejection marker.
const encodedTranscripts = new WeakMap<unknown[], string | null>();

function encodeTranscript(turns: unknown[]): string | null {
  const cached = encodedTranscripts.get(turns);
  if (cached !== undefined) return cached;
  const encoded = JSON.stringify(turns.slice(-MAX_TURNS_PER_SESSION));
  const bounded = encoded.length <= MAX_BYTES ? encoded : null;
  encodedTranscripts.set(turns, bounded);
  return bounded;
}

/** The cost of one item in the budget: its key, its value and its entry in the index. */
function cost(item: Planned, first: boolean): number {
  const json = item.name.length - ITEM_PREFIX.length;
  return item.name.length + item.body.length + json + (first ? 0 : 1);
}

/** What the next flush saves: each small snapshot, then the newest history that fits. */
function plan(stores: SnapshotStores): { small: Planned[]; history: Planned[] } {
  const small: Planned[] = [];
  const transcriptKeys: string[] = [];
  let length = INDEX_KEY.length + EMPTY_INDEX.length;
  for (const [key, value] of stores.snapshots) {
    const kind = kindOf(key);
    if (!DURABLE_KINDS.has(kind)) continue;
    if (kind === 'transcript') {
      transcriptKeys.push(key);
      continue;
    }
    const json = JSON.stringify(value);
    if (json === undefined) continue;
    const item = { key, name: itemName(key), body: `{"value":${json}}` };
    length += cost(item, small.length === 0);
    small.push(item);
  }
  // Newest first. Admit the newest suffix within budget, rather than encoding the
  // entire cache again after each eviction. Once one entry cannot fit, no older
  // entry survives.
  const history: Planned[] = [];
  for (const key of transcriptKeys.reverse()) {
    if (length >= MAX_BYTES) break;
    const value = stores.snapshots.get(key);
    const turns = Array.isArray(value) ? (value as unknown[]) : [];
    const encoded = encodeTranscript(turns);
    if (encoded === null) break;
    const stamp = stores.stamps.get(key);
    const held = stores.windows.get(key);
    // Trimming the head moves the persisted window's start, not its total.
    const dropped = Math.max(0, turns.length - MAX_TURNS_PER_SESSION);
    const stampJson = stamp ? `,"stamp":${JSON.stringify(stamp)}` : '';
    const windowJson = held
      ? `,"window":${JSON.stringify({ offset: held.offset + dropped, total: held.total })}`
      : '';
    const item = { key, name: itemName(key), body: `{"value":${encoded}${stampJson}${windowJson}}` };
    const added = cost(item, small.length + history.length === 0);
    if (length + added > MAX_BYTES) break;
    length += added;
    history.push(item);
  }
  return { small, history };
}

/** Remove the legacy item and each item that `names` does not list. */
function sweep(store: Storage, names: Set<string>): boolean {
  const stray: string[] = [];
  for (let index = 0; index < store.length; index += 1) {
    const name = store.key(index);
    if (name !== null && (name === LEGACY_KEY || (name.startsWith(ITEM_PREFIX) && !names.has(name))))
      stray.push(name);
  }
  for (const name of stray) store.removeItem(name);
  return stray.length > 0;
}

let timer: ReturnType<typeof setTimeout> | null = null;

/**
 * Write the items that changed, then the index if its keys changed. `force` also
 * writes an index that only changed its order: each read moves an entry in the LRU
 * order, and a timer must not write the index for that alone.
 */
function writeSnapshots(stores: SnapshotStores, force: boolean): void {
  if (timer !== null) {
    clearTimeout(timer);
    timer = null;
  }
  const store = storage();
  if (!store) return;
  const { small, history } = plan(stores);
  let wrote = false;
  const save = (item: Planned): boolean => {
    if (stored.get(item.key) === item.body) return true;
    try {
      store.setItem(item.name, item.body);
    } catch {
      return false;
    }
    stored.set(item.key, item.body);
    wrote = true;
    return true;
  };
  const drop = (key: string): void => {
    if (!stored.delete(key)) return;
    store.removeItem(itemName(key));
    wrote = true;
  };
  const wanted = new Set([...small, ...history].map((item) => item.name));
  // Remove first: this gives room to the writes below.
  for (const key of Array.from(stored.keys())) {
    if (!wanted.has(itemName(key))) drop(key);
  }
  if (!swept) {
    swept = true;
    if (sweep(store, wanted)) wrote = true;
  }
  // Out of quota (or a hostile embedder): keep the small rows, drop history —
  // a session list that paints instantly is worth more than nothing at all.
  let full = false;
  const shed = (): void => {
    if (full) return;
    full = true;
    for (const item of history) drop(item.key);
  };
  for (const item of small) {
    if (save(item)) continue;
    shed();
    // Do not keep an old copy of a snapshot that changed.
    if (!save(item)) drop(item.key);
  }
  // Newest first: once storage refuses one entry, no older entry stays.
  for (const item of history) {
    if (!full && save(item)) continue;
    full = true;
    drop(item.key);
  }
  const listed = [...small, ...[...history].reverse()].filter(
    (item) => stored.get(item.key) === item.body,
  );
  const index = `{"v":2,"keys":[${listed.map((item) => item.name.slice(ITEM_PREFIX.length)).join(',')}]}`;
  const keysChanged =
    listed.length !== storedKeys.size || listed.some((item) => !storedKeys.has(item.key));
  if (index !== storedIndex && (keysChanged || force)) {
    try {
      store.setItem(INDEX_KEY, index);
      storedIndex = index;
      storedKeys = new Set(listed.map((item) => item.key));
      wrote = true;
    } catch {
      // Keep the old index: hydration ignores the new keys. The next flush tries again.
    }
  }
  if (wrote) lastWriteAt = Date.now();
}

/** Write the durable snapshots now. Safe to call from a hide/pagehide handler. */
export function flushSnapshots(stores: SnapshotStores): void {
  writeSnapshots(stores, true);
}

/** Coalesced write-through for a snapshot that just changed. */
export function scheduleSnapshotFlush(stores: SnapshotStores): void {
  if (timer !== null || !storage()) return;
  const wait = Math.max(FLUSH_DEBOUNCE_MS, lastWriteAt + MIN_FLUSH_INTERVAL_MS - Date.now());
  timer = setTimeout(() => {
    timer = null;
    writeSnapshots(stores, false);
  }, wait);
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

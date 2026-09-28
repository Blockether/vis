/**
 * Opt-in memory instrumentation for finding out what keeps the app's memory alive.
 *
 * Off by default, and inert while off: nothing is patched and no cache registers a
 * reporter. One remembered flag turns it on: Settings → Diagnostics → Show memory
 * overlay, or `?perf=1` in the address; Hide memory overlay or `?perf=0` turns it off.
 * The probes install at startup, so a switch applies when the page loads. The `perf`
 * build (`npm run perf`) always has it on. It answers
 * two questions with live numbers instead of guesses:
 *
 * - Which platform resources stay registered: event listeners (including those on
 *   elements that have left the document), intervals, pending timeouts, observers
 *   and object URLs, grouped by the function that registered them.
 * - Which of the app's own caches hold how much, per session: the heatmap rows.
 *
 * The probes patch browser prototypes, so `perf-boot.ts` installs them from the
 * first module `main.tsx` evaluates. A listener added before that is invisible.
 */

const STORAGE_KEY = 'vis.perf';

/** The `perf` build (`npm run perf`), which always has instrumentation on. */
export const PERF_BUILD = import.meta.env.MODE === 'perf';

/**
 * Is instrumentation on for this page load? A `?perf=1` or `?perf=0` switch in the
 * address decides this load and is stored for the next ones.
 */
export function perfEnabled(
  search: string = typeof location === 'undefined' ? '' : location.search,
  mode: string = import.meta.env.MODE,
): boolean {
  if (mode === 'perf') return true;
  const requested = new URLSearchParams(search).get('perf');
  if (requested !== null) {
    const isOn = requested !== '0' && requested !== 'off';
    setPerfEnabled(isOn);
    return isOn;
  }
  try {
    return localStorage.getItem(STORAGE_KEY) === '1';
  } catch {
    return false;
  }
}

/**
 * Remember whether instrumentation starts with the next page load. Answers `false` when
 * this device refused to store the choice.
 */
export function setPerfEnabled(on: boolean): boolean {
  try {
    if (on) localStorage.setItem(STORAGE_KEY, '1');
    else localStorage.removeItem(STORAGE_KEY);
    return true;
  } catch {
    return false;
  }
}

/**
 * Load the page again so it starts with the stored choice. A `?perf=` switch left in the
 * address would decide instead, so the new address drops it.
 */
export function reloadWithStoredPerf(
  page: Pick<Location, 'href' | 'reload' | 'replace'> = location,
): void {
  const url = new URL(page.href);
  if (!url.searchParams.has('perf')) {
    page.reload();
    return;
  }
  url.searchParams.delete('perf');
  page.replace(url.href);
}

// ---------------------------------------------------------------------------
// Sites: who registered a resource.

const PROBE_FRAME = /visPerf/;

/**
 * The first stack frame outside the probes, as `function (file:line)`. The perf
 * build is not minified, so these are the app's own function names.
 */
export function siteOf(stack: string | undefined): string {
  for (const line of (stack ?? '').split('\n').slice(1)) {
    if (PROBE_FRAME.test(line)) continue;
    const trimmed = line.trim();
    // V8: `at name (url:line:col)` / `at url:line:col`; WebKit and Gecko: `name@url:line:col`.
    const v8 = /^at (?:async )?(?:(.+?) \()?(.+?):(\d+):\d+\)?$/.exec(trimmed);
    const other = /^(.*?)@(.+?):(\d+):\d+$/.exec(trimmed);
    const match = v8 ?? other;
    if (!match) continue;
    const file = match[2].slice(match[2].lastIndexOf('/') + 1);
    return `${match[1] || '(anonymous)'} (${file}:${match[3]})`;
  }
  return '(unknown)';
}

// ---------------------------------------------------------------------------
// Event listeners.

type Listener = EventListenerOrEventListenerObject;

interface ListenerGroup {
  target: string;
  type: string;
  site: string;
  live: number;
  added: number;
}

interface Registration {
  actual: Listener;
  group: ListenerGroup;
}

/**
 * The listeners one target holds, by group. It never references the target, so the
 * collection watch can hand it back once the target is garbage-collected.
 */
interface TargetRecord {
  live: number;
  groups: Map<ListenerGroup, number>;
  /** Elements only: the detached-element count needs them. */
  element?: WeakRef<Element>;
}

/**
 * Reports targets the garbage collector reclaimed, so their listeners stop counting.
 * The app uses the browser's `FinalizationRegistry`; tests pass one they can trigger.
 */
export interface CollectionWatch {
  register(target: object, held: object, token: object): void;
  unregister(token: object): unknown;
}

export type WatchCollection = (collected: (held: object) => void) => CollectionWatch | null;

const watchCollection: WatchCollection = (collected) =>
  typeof FinalizationRegistry === 'undefined' ? null : new FinalizationRegistry<object>(collected);

/** One listener group as the report shows it. */
export interface ListenerGroupReport {
  target: string;
  type: string;
  site: string;
  live: number;
  added: number;
}

/** A counter keyed by where the resource was created. */
export interface SiteCount {
  site: string;
  live: number;
}

/** What one observer class is watching right now. */
export interface ObserverReport {
  kind: string;
  observers: number;
  targets: number;
  detached: number;
}

/** One cache's holding for one session (or the machine, without `session`). */
export interface MemoryCell {
  /** The heatmap column: which cache holds it. */
  source: string;
  /** The heatmap row. Absent for machine-wide data. */
  session?: string;
  /** Session title when the reporting cache knows it. */
  title?: string;
  /** Approximate retained bytes (see `approxBytes`). */
  bytes: number;
  /** Items held: events, listeners, rows. */
  entries: number;
}

export type MemorySource = () => Iterable<MemoryCell>;

export interface PerfReport {
  at: number;
  /** Chromium's `performance.memory`; `null` in WebKit and Gecko. */
  heap: { used: number; total: number; limit: number } | null;
  domNodes: number;
  listeners: {
    live: number;
    /** Listeners on elements that are alive but no longer in the document. */
    detached: number;
    detachedByTarget: SiteCount[];
    groups: ListenerGroupReport[];
  };
  intervals: { live: number; sites: SiteCount[] };
  timeouts: { pending: number; sites: SiteCount[] };
  observers: ObserverReport[];
  objectUrls: { live: number; bytes: number };
  cells: MemoryCell[];
}

interface ProbeState {
  groups: Map<string, ListenerGroup>;
  registrations: WeakMap<EventTarget, Map<string, Map<Listener, Registration>>>;
  targets: WeakMap<EventTarget, TargetRecord>;
  elements: Set<WeakRef<Element>>;
  collection: CollectionWatch | null;
  intervals: Map<number, string>;
  timeouts: Map<number, string>;
  observers: Map<string, Set<WeakRef<object>>>;
  observed: WeakMap<object, Set<WeakRef<Node>>>;
  objectUrls: Map<string, number>;
}

let probes: ProbeState | null = null;
const sources = new Map<string, MemorySource>();

/** Are the probes installed in this page? */
export function perfActive(): boolean {
  return probes !== null;
}

function targetLabel(target: EventTarget, scope: typeof globalThis): string {
  if (target === scope) return 'window';
  if ('document' in scope && target === scope.document) return 'document';
  if (typeof Element !== 'undefined' && target instanceof Element) return target.tagName.toLowerCase();
  return target.constructor?.name || 'EventTarget';
}

function captureOf(options: boolean | EventListenerOptions | undefined): boolean {
  return typeof options === 'boolean' ? options : Boolean(options?.capture);
}

function trackTarget(state: ProbeState, target: EventTarget, group: ListenerGroup, delta: number): void {
  let record = state.targets.get(target);
  if (!record) {
    if (delta <= 0) return;
    record = { live: 0, groups: new Map() };
    if (typeof Element !== 'undefined' && target instanceof Element) {
      record.element = new WeakRef(target);
      state.elements.add(record.element);
    }
    state.targets.set(target, record);
    state.collection?.register(target, record, record);
  }
  group.live += delta;
  record.live += delta;
  const held = (record.groups.get(group) ?? 0) + delta;
  if (held > 0) record.groups.set(group, held);
  else record.groups.delete(group);
  if (record.live > 0) return;
  state.targets.delete(target);
  state.collection?.unregister(record);
  if (record.element) state.elements.delete(record.element);
}

/** The target went away with listeners attached: they went with it. */
function settleCollected(state: ProbeState, record: TargetRecord): void {
  for (const [group, held] of record.groups) group.live -= held;
  record.groups.clear();
  record.live = 0;
  if (record.element) state.elements.delete(record.element);
}

/**
 * Patch the platform so every listener, interval, timeout, observer and object URL
 * is counted where it was created. Returns the undo, for tests; the app never undoes
 * it. Idempotent.
 */
export function installPerfProbes(
  scope: typeof globalThis = globalThis,
  watch: WatchCollection = watchCollection,
): () => void {
  // The probes hold what they count weakly; a WebView without `WeakRef` keeps them off.
  if (probes || typeof WeakRef === 'undefined') return () => {};
  const state: ProbeState = {
    groups: new Map(),
    registrations: new WeakMap(),
    targets: new WeakMap(),
    elements: new Set(),
    collection: null,
    intervals: new Map(),
    timeouts: new Map(),
    observers: new Map(),
    observed: new WeakMap(),
    objectUrls: new Map(),
  };
  state.collection = watch((held) => settleCollected(state, held as TargetRecord));
  probes = state;
  const undo: Array<() => void> = [];

  const proto = scope.EventTarget.prototype;
  const nativeAdd = proto.addEventListener;
  const nativeRemove = proto.removeEventListener;

  const release = (target: EventTarget, slot: Map<Listener, Registration>, listener: Listener) => {
    const registration = slot.get(listener);
    if (!registration) return;
    slot.delete(listener);
    trackTarget(state, target, registration.group, -1);
  };

  proto.addEventListener = function visPerfAddEventListener(
    this: EventTarget,
    type: string,
    listener: Listener | null,
    options?: boolean | AddEventListenerOptions,
  ): void {
    const flags = typeof options === 'object' && options !== null ? options : undefined;
    if (!listener || flags?.signal?.aborted) {
      nativeAdd.call(this, type, listener, options);
      return;
    }
    const target = this;
    const key = `${type}\u0000${captureOf(options)}`;
    let byType = state.registrations.get(target);
    if (!byType) {
      byType = new Map();
      state.registrations.set(target, byType);
    }
    let slot = byType.get(key);
    if (!slot) {
      slot = new Map();
      byType.set(key, slot);
    }
    const known = slot.get(listener);
    if (known) {
      // The platform ignores a repeat of the same listener: so do the counts.
      nativeAdd.call(target, type, known.actual, options);
      return;
    }
    const label = targetLabel(target, scope);
    const site = siteOf(new Error().stack);
    const groupKey = `${label}\u0000${type}\u0000${site}`;
    let group = state.groups.get(groupKey);
    if (!group) {
      group = { target: label, type, site, live: 0, added: 0 };
      state.groups.set(groupKey, group);
    }
    const owned = slot;
    let actual: Listener = listener;
    if (flags?.once) {
      const settle = () => release(target, owned, listener);
      actual =
        typeof listener === 'function'
          ? function visPerfOnce(this: unknown, event: Event) {
              settle();
              return listener.call(this, event);
            }
          : {
              handleEvent(event: Event) {
                settle();
                return listener.handleEvent(event);
              },
            };
    }
    slot.set(listener, { actual, group });
    group.added += 1;
    trackTarget(state, target, group, 1);
    if (flags?.signal) {
      // Held weakly: the probe must not keep a target alive longer than the page does.
      const targetRef = new WeakRef(target);
      const listenerRef = new WeakRef(listener);
      const onAbort = () => {
        const alive = targetRef.deref();
        const own = listenerRef.deref();
        const current = alive ? state.registrations.get(alive)?.get(key) : undefined;
        if (alive && own && current) release(alive, current, own);
      };
      try {
        nativeAdd.call(flags.signal, 'abort', onAbort, { once: true });
      } catch {
        // A signal from another realm refuses this realm's method; its own is unpatched.
        flags.signal.addEventListener('abort', onAbort, { once: true });
      }
    }
    nativeAdd.call(target, type, actual, options);
  };

  proto.removeEventListener = function visPerfRemoveEventListener(
    this: EventTarget,
    type: string,
    listener: Listener | null,
    options?: boolean | EventListenerOptions,
  ): void {
    const slot = listener
      ? state.registrations.get(this)?.get(`${type}\u0000${captureOf(options)}`)
      : undefined;
    const registration = listener ? slot?.get(listener) : undefined;
    if (slot && listener && registration) {
      release(this, slot, listener);
      nativeRemove.call(this, type, registration.actual, options);
      return;
    }
    nativeRemove.call(this, type, listener, options);
  };
  undo.push(() => {
    proto.addEventListener = nativeAdd;
    proto.removeEventListener = nativeRemove;
  });

  // Timers. Browsers share one id space between timeouts and intervals, and either
  // clear function cancels either kind.
  const timers = scope as unknown as {
    setInterval: (handler: TimerHandler, timeout?: number, ...rest: unknown[]) => number;
    clearInterval: (id?: number) => void;
    setTimeout: (handler: TimerHandler, timeout?: number, ...rest: unknown[]) => number;
    clearTimeout: (id?: number) => void;
  };
  const nativeSetInterval = timers.setInterval;
  const nativeClearInterval = timers.clearInterval;
  const nativeSetTimeout = timers.setTimeout;
  const nativeClearTimeout = timers.clearTimeout;
  const forget = (id: number | undefined) => {
    if (id === undefined) return;
    state.intervals.delete(id);
    state.timeouts.delete(id);
  };
  timers.setInterval = function visPerfSetInterval(handler, timeout, ...rest) {
    const id = nativeSetInterval.call(scope, handler, timeout, ...rest);
    state.intervals.set(id, siteOf(new Error().stack));
    return id;
  };
  timers.setTimeout = function visPerfSetTimeout(handler, timeout, ...rest) {
    if (typeof handler !== 'function') return nativeSetTimeout.call(scope, handler, timeout, ...rest);
    const site = siteOf(new Error().stack);
    const id = nativeSetTimeout.call(
      scope,
      function visPerfTimeout(this: unknown, ...values: unknown[]) {
        state.timeouts.delete(id);
        return (handler as (...values: unknown[]) => unknown).apply(this, values);
      },
      timeout,
      ...rest,
    );
    state.timeouts.set(id, site);
    return id;
  };
  timers.clearInterval = function visPerfClearInterval(id) {
    forget(id);
    nativeClearInterval.call(scope, id);
  };
  timers.clearTimeout = function visPerfClearTimeout(id) {
    forget(id);
    nativeClearTimeout.call(scope, id);
  };
  undo.push(() => {
    timers.setInterval = nativeSetInterval;
    timers.clearInterval = nativeClearInterval;
    timers.setTimeout = nativeSetTimeout;
    timers.clearTimeout = nativeClearTimeout;
  });

  // Observers hold every element they observe until it is unobserved.
  for (const kind of ['ResizeObserver', 'IntersectionObserver', 'MutationObserver'] as const) {
    const ctor = (scope as unknown as Record<string, { prototype: Record<string, unknown> } | undefined>)[
      kind
    ];
    if (!ctor?.prototype) continue;
    const observerProto = ctor.prototype as {
      observe: (target: Node, options?: unknown) => void;
      unobserve?: (target: Node) => void;
      disconnect: () => void;
    };
    const nativeObserve = observerProto.observe;
    const nativeUnobserve = observerProto.unobserve;
    const nativeDisconnect = observerProto.disconnect;
    const live = new Set<WeakRef<object>>();
    state.observers.set(kind, live);
    observerProto.observe = function visPerfObserve(this: object, target: Node, options?: unknown) {
      let targets = state.observed.get(this);
      if (!targets) {
        targets = new Set();
        state.observed.set(this, targets);
        live.add(new WeakRef(this));
      }
      if (![...targets].some((ref) => ref.deref() === target)) targets.add(new WeakRef(target));
      nativeObserve.call(this, target, options);
    };
    if (nativeUnobserve) {
      observerProto.unobserve = function visPerfUnobserve(this: object, target: Node) {
        const targets = state.observed.get(this);
        for (const ref of targets ?? []) if (ref.deref() === target) targets?.delete(ref);
        nativeUnobserve.call(this, target);
      };
    }
    observerProto.disconnect = function visPerfDisconnect(this: object) {
      state.observed.get(this)?.clear();
      nativeDisconnect.call(this);
    };
    undo.push(() => {
      observerProto.observe = nativeObserve;
      if (nativeUnobserve) observerProto.unobserve = nativeUnobserve;
      observerProto.disconnect = nativeDisconnect;
    });
  }

  // Object URLs pin their Blob in memory until they are revoked.
  const url = scope.URL as typeof URL | undefined;
  if (url?.createObjectURL && url.revokeObjectURL) {
    const nativeCreate = url.createObjectURL;
    const nativeRevoke = url.revokeObjectURL;
    url.createObjectURL = function visPerfCreateObjectURL(object: Blob | MediaSource) {
      const made = nativeCreate.call(url, object);
      state.objectUrls.set(made, typeof Blob !== 'undefined' && object instanceof Blob ? object.size : 0);
      return made;
    };
    url.revokeObjectURL = function visPerfRevokeObjectURL(address: string) {
      state.objectUrls.delete(address);
      nativeRevoke.call(url, address);
    };
    undo.push(() => {
      url.createObjectURL = nativeCreate;
      url.revokeObjectURL = nativeRevoke;
    });
  }

  if (typeof window !== 'undefined') {
    const hook = window as unknown as { __visPerf?: unknown };
    hook.__visPerf = { report: perfReport };
    undo.push(() => delete hook.__visPerf);
  }

  return () => {
    for (const step of undo.reverse()) step();
    if (probes === state) probes = null;
  };
}

// ---------------------------------------------------------------------------
// The app's own caches.

/**
 * Let a cache report what it holds. A no-op unless instrumentation is on, so a
 * normal session pays nothing and retains nothing for it.
 */
export function registerMemorySource(id: string, collect: MemorySource): () => void {
  if (!probes) return () => {};
  sources.set(id, collect);
  return () => {
    if (sources.get(id) === collect) sources.delete(id);
  };
}

let ownerSerial = 0;

/**
 * Let `owner` report what it holds for as long as it lives. It is held weakly: the
 * overlay must not be what keeps a replaced client or a disposed hub alive. Like
 * `registerMemorySource`, it does nothing unless instrumentation is on.
 */
export function registerMemoryOwner<T extends object>(
  label: string,
  owner: T,
  collect: (owner: T) => Iterable<MemoryCell>,
): () => void {
  if (!probes) return () => {};
  const ref = new WeakRef(owner);
  ownerSerial += 1;
  const stop = registerMemorySource(`${label} ${ownerSerial}`, () => {
    const alive = ref.deref();
    if (!alive) stop();
    return alive ? collect(alive) : [];
  });
  return stop;
}

const measured = new WeakMap<object, { bytes: number; size: number }>();

function sizeHint(value: object): number {
  if (Array.isArray(value)) return value.length;
  if (value instanceof Map || value instanceof Set) return value.size;
  return -1;
}

/**
 * Roughly how many bytes a value keeps alive: strings by length, 8 bytes per
 * number or reference slot, a small header per object. It is a ranking signal
 * for the heatmap, not an allocator's figure. Payloads are replaced rather than
 * edited in place, so a top-level result is remembered per object.
 */
export function approxBytes(value: unknown): number {
  if (value === null || typeof value !== 'object') return primitiveBytes(value);
  const cached = measured.get(value);
  const hint = sizeHint(value);
  if (cached && cached.size === hint) return cached.bytes;
  const seen = new Set<object>();
  const stack: unknown[] = [value];
  let bytes = 0;
  while (stack.length > 0) {
    const next = stack.pop();
    if (next === null || typeof next !== 'object') {
      bytes += primitiveBytes(next);
      continue;
    }
    if (seen.has(next)) continue;
    seen.add(next);
    if (typeof Blob !== 'undefined' && next instanceof Blob) {
      bytes += next.size;
    } else if (Array.isArray(next)) {
      bytes += 16 + next.length * 8;
      for (const item of next) stack.push(item);
    } else if (next instanceof Map) {
      bytes += 32 + next.size * 24;
      for (const [key, item] of next) stack.push(key, item);
    } else if (next instanceof Set) {
      bytes += 32 + next.size * 16;
      for (const item of next) stack.push(item);
    } else if (next instanceof Promise || typeof (next as { then?: unknown }).then === 'function') {
      bytes += 32;
    } else {
      const keys = Object.keys(next);
      bytes += 24 + keys.length * 8;
      for (const key of keys) stack.push((next as Record<string, unknown>)[key]);
    }
  }
  measured.set(value, { bytes, size: hint });
  return bytes;
}

function primitiveBytes(value: unknown): number {
  if (typeof value === 'string') return 16 + value.length;
  if (typeof value === 'number' || typeof value === 'bigint') return 8;
  return 0;
}

// ---------------------------------------------------------------------------
// The report.

function siteCounts(sites: Iterable<string>): SiteCount[] {
  const counts = new Map<string, number>();
  for (const site of sites) counts.set(site, (counts.get(site) ?? 0) + 1);
  return [...counts]
    .map(([site, live]) => ({ site, live }))
    .sort((a, b) => b.live - a.live || a.site.localeCompare(b.site));
}

/** Everything the probes and the registered caches know, right now. */
export function perfReport(): PerfReport {
  const state = probes;
  const memory = (performance as Performance & {
    memory?: { usedJSHeapSize: number; totalJSHeapSize: number; jsHeapSizeLimit: number };
  }).memory;
  const cells: MemoryCell[] = [];
  for (const [id, collect] of [...sources]) {
    try {
      for (const cell of collect()) cells.push(cell);
    } catch {
      sources.delete(id);
    }
  }
  let detached = 0;
  const detachedTargets: string[] = [];
  const groups: ListenerGroupReport[] = [];
  const observers: ObserverReport[] = [];
  if (state) {
    for (const ref of [...state.elements]) {
      const element = ref.deref();
      const count = element ? (state.targets.get(element)?.live ?? 0) : 0;
      if (!element || count === 0) {
        state.elements.delete(ref);
        continue;
      }
      if (element.isConnected) continue;
      detached += count;
      for (let index = 0; index < count; index += 1) detachedTargets.push(element.tagName.toLowerCase());
    }
    for (const group of state.groups.values()) {
      if (group.live > 0 || group.added > 1) groups.push({ ...group });
    }
    groups.sort((a, b) => b.live - a.live || b.added - a.added);
    for (const [kind, live] of state.observers) {
      let count = 0;
      let targets = 0;
      let lost = 0;
      for (const ref of [...live]) {
        const observer = ref.deref();
        if (!observer) {
          live.delete(ref);
          continue;
        }
        const observed = state.observed.get(observer);
        for (const targetRef of [...(observed ?? [])]) {
          const node = targetRef.deref();
          if (!node) {
            observed?.delete(targetRef);
            continue;
          }
          targets += 1;
          if (!node.isConnected) lost += 1;
        }
        if (observed?.size) count += 1;
      }
      observers.push({ kind, observers: count, targets, detached: lost });
    }
  }
  const objectBytes = state ? [...state.objectUrls.values()].reduce((sum, size) => sum + size, 0) : 0;
  return {
    at: Date.now(),
    heap: memory
      ? { used: memory.usedJSHeapSize, total: memory.totalJSHeapSize, limit: memory.jsHeapSizeLimit }
      : null,
    domNodes: typeof document === 'undefined' ? 0 : document.getElementsByTagName('*').length,
    listeners: {
      live: groups.reduce((sum, group) => sum + group.live, 0),
      detached,
      detachedByTarget: siteCounts(detachedTargets),
      groups,
    },
    intervals: {
      live: state?.intervals.size ?? 0,
      sites: siteCounts(state?.intervals.values() ?? []),
    },
    timeouts: {
      pending: state?.timeouts.size ?? 0,
      sites: siteCounts(state?.timeouts.values() ?? []),
    },
    observers,
    objectUrls: { live: state?.objectUrls.size ?? 0, bytes: objectBytes },
    cells,
  };
}

// ---------------------------------------------------------------------------
// The heatmap: sessions × caches.

export type HeatMetric = 'bytes' | 'entries';

export interface HeatRow {
  /** Session id, or `null` for the machine-wide row. */
  session: string | null;
  title: string;
  total: number;
  values: Record<string, number>;
}

export interface Heatmap {
  columns: string[];
  rows: HeatRow[];
  /** Data that belongs to no session: lists, models, settings. */
  machine: HeatRow;
  /** Largest single cell, the top of the colour scale. */
  peak: number;
  /** Sessions beyond `limit`, folded out of `rows`. */
  hidden: number;
}

/** Fold cells into rows per session, heaviest first. */
export function buildHeatmap(cells: MemoryCell[], metric: HeatMetric, limit = 16): Heatmap {
  const rows = new Map<string, HeatRow>();
  const machine: HeatRow = { session: null, title: 'Machine-wide', total: 0, values: {} };
  const columnTotals = new Map<string, number>();
  for (const cell of cells) {
    const amount = metric === 'bytes' ? cell.bytes : cell.entries;
    let row = machine;
    if (cell.session) {
      row = rows.get(cell.session) ?? { session: cell.session, title: '', total: 0, values: {} };
      rows.set(cell.session, row);
      if (!row.title && cell.title) row.title = cell.title;
    }
    row.values[cell.source] = (row.values[cell.source] ?? 0) + amount;
    row.total += amount;
    columnTotals.set(cell.source, (columnTotals.get(cell.source) ?? 0) + amount);
  }
  const columns = [...columnTotals]
    .filter(([, total]) => total > 0)
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .map(([column]) => column);
  const sorted = [...rows.values()]
    .filter((row) => row.total > 0)
    .sort((a, b) => b.total - a.total || (a.session ?? '').localeCompare(b.session ?? ''));
  const shown = sorted.slice(0, limit);
  for (const row of shown) if (!row.title) row.title = row.session ?? '';
  let peak = 0;
  for (const row of [...shown, machine]) {
    for (const value of Object.values(row.values)) peak = Math.max(peak, value);
  }
  return { columns, rows: shown, machine, peak, hidden: sorted.length - shown.length };
}

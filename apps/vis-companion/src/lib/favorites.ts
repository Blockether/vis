// Starred sessions, per gateway, kept across screens and app restarts.
//
// A favorite is a HUMAN's decision, so it outranks every heuristic the list
// has: live, unread, unsent words and the age cutoff all move rows around on
// their own, and a starred row must not move with them. It is pinned to the
// top of its project and it is never collapsed away.
//
// The star is stored as a RANK, not a boolean, and the rank is a monotonic
// counter rather than a clock: two stars tapped inside the same millisecond
// would tie, and a tie is exactly how "deterministic ordering" dies. Ranks are
// only ever compared, never displayed, so gaps left by unstarring are fine.

import { useEffect, useSyncExternalStore } from 'react';
import { Preferences } from '@capacitor/preferences';
import { bridged } from './bridge';

const FAVORITES_KEY = 'vis.sessionFavorites';

/** Rank → session key. Insertion order is meaningless; the numbers are the order. */
export type FavoriteStore = Record<string, number>;

/**
 * One star per (gateway, session): the same sid on another machine is another
 * thread, exactly as with draft messages.
 */
export function favoriteKey(gatewayBase: string, sid: string): string {
  return `${gatewayBase}\u0000${sid}`;
}

let store: FavoriteStore = {};
let hydrated = false;
let hydration: Promise<FavoriteStore> | null = null;

// Readers take a SNAPSHOT replaced on every change: `useSyncExternalStore`
// compares by identity, and the store is mutated in place.
let snapshot: FavoriteStore = store;
const listeners = new Set<() => void>();

function announce(): void {
  snapshot = { ...store };
  for (const listener of listeners) listener();
}

function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}

function parseStore(raw: string | null): FavoriteStore {
  if (!raw) return {};
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    const out: FavoriteStore = {};
    for (const [key, value] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof value !== 'number' || !Number.isFinite(value)) continue;
      out[key] = value;
    }
    return out;
  } catch {
    return {};
  }
}

/**
 * Load the stars once. Concurrent callers — the list and an open session both
 * ask — share the SAME promise, so storage is never read twice or raced.
 */
export async function hydrateFavorites(): Promise<FavoriteStore> {
  if (hydrated) return store;
  hydration ??= (async () => {
    // Bounded: a silent native bridge must not leave the list unstarred
    // forever (see `lib/bridge.ts`); localStorage holds the same value.
    const raw = await bridged(
      async () => (await Preferences.get({ key: FAVORITES_KEY })).value ?? null,
      () => {
        try {
          return globalThis.localStorage?.getItem(FAVORITES_KEY) ?? null;
        } catch {
          return null;
        }
      },
    );
    // A star tapped while we were reading is NEWER than disk, so it is rebased
    // above everything that came back rather than merged on top of it: two
    // stars sharing a rank would be a tie, and ties are what the ordering
    // promises never to have.
    const disk = parseStore(raw);
    const offset = Math.max(0, ...Object.values(disk));
    for (const key of Object.keys(store)) store[key] += offset;
    store = { ...disk, ...store };
    hydrated = true;
    announce();
    return store;
  })();
  return hydration;
}

/**
 * When this session was starred, or null. The number is an ORDER, not a time:
 * compare it, never render it.
 */
export function favoriteRank(key: string): number | null {
  const rank = store[key];
  return typeof rank === 'number' ? rank : null;
}


/** The stars held right now, for callers that already have a snapshot in hand. */

/**
 * Star or unstar one session; returns the new state.
 *
 * A fresh star always lands BELOW every existing one, so adding the tenth
 * favorite never reshuffles the nine above it.
 */
export function toggleFavorite(key: string): boolean {
  if (key in store) {
    delete store[key];
    persist();
    announce();
    return false;
  }
  store[key] = nextRank();
  persist();
  announce();
  return true;
}

function nextRank(): number {
  let max = 0;
  for (const rank of Object.values(store)) {
    if (rank > max) max = rank;
  }
  return max + 1;
}

/** Forget stars whose sessions are gone. */
export function forgetFavorites(keys: Iterable<string>): void {
  let changed = false;
  for (const key of keys) {
    if (!(key in store)) continue;
    delete store[key];
    changed = true;
  }
  if (!changed) return;
  persist();
  announce();
}

/**
 * Write through immediately. Starring is a deliberate, rare tap — nothing like
 * typing — so there is no debounce to lose on the way out of the app.
 */
/**
 * Write through immediately. Starring is a deliberate, rare tap — nothing like
 * typing — so there is no debounce to lose on the way out of the app.
 *
 * A star tapped before the first read WAITS for it: writing straight away would
 * put this one star on disk on top of every star already stored there.
 */
function persist(): void {
  if (!hydrated) {
    void hydrateFavorites().then(writeThrough);
    return;
  }
  writeThrough();
}

function writeThrough(): void {
  const value = JSON.stringify(store);
  try {
    globalThis.localStorage?.setItem(FAVORITES_KEY, value);
  } catch {
    // Private-mode/quota: the plugin write below is still worth attempting.
  }
  void bridged(
    async () => {
      await Preferences.set({ key: FAVORITES_KEY, value });
    },
    // Already mirrored to localStorage above.
    () => undefined,
  );
}

/** The stars this device is holding, re-read on every change. */
export function useFavorites(): FavoriteStore {
  useEffect(() => {
    void hydrateFavorites();
  }, []);
  return useSyncExternalStore(
    subscribe,
    () => snapshot,
    () => snapshot,
  );
}

/** Test seam: drop everything this module is holding, including the hydration. */

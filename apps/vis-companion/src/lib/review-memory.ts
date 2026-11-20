/** Review work survives navigation, scoped to one gateway, session and app. */
import { createContext, useCallback, useContext, useLayoutEffect, useMemo, useState, useSyncExternalStore, type Dispatch, type RefObject, type SetStateAction } from 'react';

export const ReviewMemoryContext = createContext<string | null>(null);
/** The identity of an app that outlives its view: gateway, session and app key. */
export const ReviewPrefsContext = createContext<string | null>(null);

interface Cell<T> {
  read: () => T;
  write: Dispatch<SetStateAction<T>>;
  subscribe: (notify: () => void) => () => void;
}

function cell<T>(initial: T | (() => T)): Cell<T> {
  let value = typeof initial === 'function' ? (initial as () => T)() : initial;
  const listeners = new Set<() => void>();
  return {
    read: () => value,
    write: (next) => {
      const updated = typeof next === 'function' ? (next as (held: T) => T)(value) : next;
      if (Object.is(updated, value)) return;
      value = updated;
      listeners.forEach((notify) => notify());
    },
    subscribe: (notify) => {
      listeners.add(notify);
      return () => listeners.delete(notify);
    },
  };
}

// UI state and the last acknowledged view live here. The gateway owns saved work.
// Memory dies with this page, so one browser visit never leaves draft text on disk.
const visit = new Map<string, Cell<unknown>>();

function heldCell<T>(key: string | null, initial: T | (() => T)): Cell<T> {
  if (key === null) return cell(initial);
  let held = visit.get(key);
  if (!held) {
    held = cell(initial) as Cell<unknown>;
    visit.set(key, held);
  }
  return held as Cell<T>;
}

/** A shared value for this visit. Null keys remain local to their component. */
export function useVisitState<T>(key: string | null, initial: T | (() => T)) {
  // The initial value is used only when this identity has no value yet.
  const [local] = useState(() => cell(initial));
  const held = useMemo(() => key === null ? local : heldCell(key, local.read()), [key, local]);
  const value = useSyncExternalStore(held.subscribe, held.read, held.read);
  return [value, held.write] as const;
}

/** A node's state is isolated from apps with the same node IDs in other sessions. */
export function useReviewState<T>(name: string, initial: T | (() => T)) {
  const identity = useContext(ReviewMemoryContext);
  return useVisitState(identity === null ? null : JSON.stringify([identity, name]), initial);
}

// View choices, such as hidden test files, are kept on this device after a reload.
const PREFS_KEY = 'vis.reviewPrefs';
// The oldest choice goes first, so throwaway sessions cannot grow the entry without bound.
const KEEP_PREFS = 200;

function readPrefs(): Record<string, boolean> {
  try {
    const parsed: unknown = JSON.parse(globalThis.localStorage?.getItem(PREFS_KEY) ?? 'null');
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    return Object.fromEntries(Object.entries(parsed).filter(([, value]) => typeof value === 'boolean'));
  } catch {
    return {};
  }
}

function writePref(key: string, value: boolean): void {
  const prefs = readPrefs();
  delete prefs[key];
  prefs[key] = value;
  const keys = Object.keys(prefs);
  for (const stale of keys.slice(0, Math.max(0, keys.length - KEEP_PREFS))) delete prefs[stale];
  try {
    globalThis.localStorage?.setItem(PREFS_KEY, JSON.stringify(prefs));
  } catch {
    // Private mode or quota: the choice then lasts for this visit only.
  }
}

/** A node's view choice for this visit, which also survives a reload of the page. */
export function useReviewPref(name: string, initial: boolean) {
  const lasting = useContext(ReviewPrefsContext);
  const key = lasting === null ? null : JSON.stringify([lasting, name]);
  const [value, write] = useReviewState(name, () => (key === null ? undefined : readPrefs()[key]) ?? initial);
  const choose = useCallback(
    (next: boolean) => {
      write(next);
      if (key !== null) writePref(key, next);
    },
    [write, key],
  );
  return [value, choose] as const;
}

const scrolls = new Map<string, number>();

/** Restore after the app arrives, not while its loading placeholder is mounted. */
export function useReviewScroll(key: string, ref: RefObject<HTMLDivElement | null>, ready: boolean) {
  useLayoutEffect(() => {
    if (ready && ref.current) ref.current.scrollTop = scrolls.get(key) ?? 0;
  }, [key, ready, ref]);
  return () => {
    if (ready && ref.current) scrolls.set(key, ref.current.scrollTop);
  };
}

import { useSyncExternalStore } from 'react';
import type { Session } from './types';

/**
 * Per-session READ MARKS — "how many turns had this session finished the last
 * time I actually looked at it".
 *
 * A turn that completes while the session screen is closed is the one thing the
 * list has no way of announcing: the row's relative timestamp moves, which is
 * invisible unless you already knew the old value. The mark is the cheap fix —
 * the gateway's `turn_count` at the moment the transcript was on screen. Any
 * later growth is an answer the user has not read.
 *
 * Marks live in localStorage, keyed by session id, and are deliberately NOT
 * synced to the gateway: "did I read this" is a property of this device, not of
 * the session.
 */

const KEY = 'vis.session-read.v1';

/** Session id → turn count that was on screen when it was last read. */
type Marks = Record<string, number>;

let marks: Marks | null = null;
const listeners = new Set<() => void>();
let version = 0;

function load(): Marks {
  if (marks) return marks;
  marks = {};
  try {
    const raw = localStorage.getItem(KEY);
    if (raw) {
      const parsed: unknown = JSON.parse(raw);
      if (parsed && typeof parsed === 'object' && !Array.isArray(parsed)) {
        for (const [sid, value] of Object.entries(parsed as Record<string, unknown>)) {
          if (typeof value === 'number' && Number.isFinite(value)) marks[sid] = value;
        }
      }
    }
  } catch {
    // Private mode, quota, corrupt JSON — an empty map is a safe read state.
  }
  return marks;
}

function persist(): void {
  try {
    localStorage.setItem(KEY, JSON.stringify(load()));
  } catch {
    // Never let a storage failure break the list.
  }
}

function announce(): void {
  version += 1;
  for (const listener of listeners) listener();
}

/** Turns the gateway says this session has finished. */
export function sessionTurnCount(session: Session): number {
  const count = Number(session.turn_count ?? 0);
  return Number.isFinite(count) && count > 0 ? count : 0;
}

/**
 * Record a session as read at `turns` (defaults to the row's own count).
 * Idempotent, and never moves a mark backwards — a stale meta row arriving
 * after a fresh one must not resurrect an unread badge.
 */
export function markSessionRead(sid: string, turns: number): void {
  const store = load();
  const next = Number.isFinite(turns) && turns > 0 ? turns : 0;
  if ((store[sid] ?? -1) >= next) return;
  store[sid] = next;
  persist();
  announce();
}

/**
 * Seed marks for sessions this device has never seen. Without this, the first
 * list load after install would paint EVERY session unread, which is noise, not
 * a signal. Only genuinely new rows are seeded; existing marks are untouched.
 */
export function seedReadMarks(sessions: readonly Session[]): void {
  const store = load();
  let changed = false;
  for (const session of sessions) {
    if (store[session.id] !== undefined) continue;
    store[session.id] = sessionTurnCount(session);
    changed = true;
  }
  if (!changed) return;
  persist();
  announce();
}

/**
 * Does this row carry an answer the user has not seen? Only a session that is
 * NOT currently running counts: a turn still in flight has no answer yet, and
 * flagging it would make the badge mean "busy" instead of "unread".
 */
function isSessionUnread(session: Session): boolean {
  if (session.live === true || session.status === 'running') return false;
  const seen = load()[session.id];
  if (seen === undefined) return false;
  return sessionTurnCount(session) > seen;
}

/** How many unread answers a session is holding (1+ when unread). */
export function unreadTurnCount(session: Session): number {
  if (!isSessionUnread(session)) return 0;
  return sessionTurnCount(session) - (load()[session.id] ?? 0);
}

function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}

/**
 * Re-render on any mark change. Returns an opaque version counter — read the
 * marks through `isSessionUnread` after calling it.
 */
export function useReadMarks(): number {
  return useSyncExternalStore(
    subscribe,
    () => version,
    () => version,
  );
}

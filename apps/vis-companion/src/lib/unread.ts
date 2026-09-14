import { Preferences } from '@capacitor/preferences';
import { useSyncExternalStore } from 'react';
import { bridged } from './bridge';
import type { Session, TranscriptTurn } from './types';

/**
 * Per-session read marks count completed answers addressed to the human.
 * Council coordination and subagent results do not advance this watermark.
 * The gateway owns answer_count; turn_count only invalidates transcript caches.
 *
 * Marks belong to this device, not to the gateway. localStorage supplies the
 * synchronous first frame; Capacitor Preferences is the durable copy that survives
 * iOS recycling or resetting the webview.
 */

const KEY = 'vis.session-answer-read.v1';

/** Session id → human-answer count visible when the session was last read. */
type Marks = Record<string, number>;

let marks: Marks | null = null;
let hydration: Promise<void> | null = null;
const listeners = new Set<() => void>();
let version = 0;

function localGet(): string | null {
  try {
    return globalThis.localStorage?.getItem(KEY) ?? null;
  } catch {
    return null;
  }
}

function localSet(value: string): void {
  try {
    globalThis.localStorage?.setItem(KEY, value);
  } catch {
    // Private mode / quota: the Preferences write is the durable one anyway.
  }
}

function parse(raw: string | null): Marks {
  if (!raw) return {};
  try {
    const parsed: unknown = JSON.parse(raw);
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    const store: Marks = {};
    for (const [sid, value] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof value === 'number' && Number.isFinite(value)) store[sid] = value;
    }
    return store;
  } catch {
    return {};
  }
}

function load(): Marks {
  if (marks === null) marks = parse(localGet());
  return marks;
}

/** Write both halves without putting a native bridge round trip in a press path. */
function persist(): void {
  const value = JSON.stringify(load());
  localSet(value);
  void bridged(
    async () => {
      await Preferences.set({ key: KEY, value });
    },
    () => undefined,
  );
}

/** Read marks only move forward, so the larger watermark is always the newer one. */
function mergeMarks(incoming: Marks): boolean {
  const store = load();
  let changed = false;
  for (const [sid, value] of Object.entries(incoming)) {
    if ((store[sid] ?? -1) >= value) continue;
    store[sid] = value;
    changed = true;
  }
  return changed;
}

/** Restore the durable half once, before an unseen row is seeded as already read. */
export async function hydrateReadMarks(): Promise<void> {
  if (hydration) return hydration;
  hydration = (async () => {
    const raw = await bridged(
      async () => (await Preferences.get({ key: KEY })).value ?? null,
      localGet,
    );
    const changed = mergeMarks(parse(raw));
    // This also migrates read marks created by releases that only used localStorage.
    persist();
    if (changed) announce();
  })();
  return hydration;
}

function announce(): void {
  version += 1;
  for (const listener of listeners) listener();
}

/** Settled human answers, already filtered by the gateway. */
export function answeredTurnCount(session: Session | null | undefined): number {
  const count = Number(session?.answer_count ?? 0);
  return Number.isFinite(count) && count > 0 ? count : 0;
}

/**
 * Answers the reader can currently see, including a settled running-turn bubble whose
 * transcript row has not reached the gateway response yet.
 */
export function visibleAnsweredTurnCount(
  session: Session | null | undefined,
  turns: readonly TranscriptTurn[],
  runningTurnStatus: string | null | undefined,
  runningRequestKind: string | null | undefined = session?.running_request_kind,
): number {
  const settledTranscriptTurns = turns.filter(
    (turn) => turn.request_kind !== 'council' && !['running', 'pending', 'cancelled', 'interrupted'].includes(turn.status ?? ''),
  ).length;
  const settledRunningTurn = runningRequestKind !== 'council' && runningTurnStatus != null
    && !['running', 'pending', 'cancelled', 'interrupted'].includes(runningTurnStatus) ? 1 : 0;
  return Math.max(answeredTurnCount(session), settledTranscriptTurns + settledRunningTurn);
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
 * Seed marks for sessions this device has never seen. The durable copy must be
 * restored first: seeding against an empty webview store would turn every unread
 * answer into an already-read one on restart.
 */
export async function seedReadMarks(sessions: readonly Session[]): Promise<void> {
  await hydrateReadMarks();
  const store = load();
  let changed = false;
  for (const session of sessions) {
    if (store[session.id] !== undefined) continue;
    store[session.id] = answeredTurnCount(session);
    changed = true;
  }
  if (!changed) return;
  persist();
  announce();
}

/** Has this device established a read watermark for the session? */
export function hasSessionReadMark(sid: string): boolean {
  return load()[sid] !== undefined;
}

/**
 * Does this row carry an answer the user has not seen? Only a session that is
 * NOT currently running counts: a turn still in flight has no answer yet, and
 * flagging it would make the badge mean "busy" instead of "unread".
 */
function isSessionUnread(session: Session): boolean {
  if (session.live) return false;
  const seen = load()[session.id];
  if (seen === undefined) return false;
  return answeredTurnCount(session) > seen;
}

/** How many unread answers a session is holding (1+ when unread). */
export function unreadTurnCount(session: Session): number {
  if (!isSessionUnread(session)) return 0;
  return answeredTurnCount(session) - (load()[session.id] ?? 0);
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

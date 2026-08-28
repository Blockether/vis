import { Preferences } from '@capacitor/preferences';
import { useSyncExternalStore } from 'react';
import { bridged } from './bridge';
import type { Session, TranscriptTurn } from './types';

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
 * Marks belong to this device, not to the gateway. localStorage supplies the
 * synchronous first frame; Capacitor Preferences is the durable copy that survives
 * iOS recycling or resetting the webview.
 */

const KEY = 'vis.session-read.v1';

/** Session id → turn count that was on screen when it was last read. */
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

/**
 * Turns this session has ANSWERED — the only count an unread mark may use.
 *
 * The gateway persists a turn row the moment the turn STARTS (status
 * `running`), so `turn_count` moves at submit, not at completion, and never
 * moves again when the answer lands (proven against a live gateway: 7 -> 8 the
 * instant the turn went live, still 8 after it finished). Marking a session
 * read at the raw count while you watch your own turn run therefore already
 * covers the answer that arrives minutes later — every turn you started
 * yourself was silently pre-read, which is exactly "I was notified but the list
 * shows no badge".
 *
 * A session has at most one persisted in-flight turn (queued messages are not
 * written until they start), so discounting the live one is exact.
 */
export function answeredTurnCount(session: Session | null | undefined): number {
  if (!session) return 0;
  const count = Number(session.turn_count ?? 0);
  const total = Number.isFinite(count) && count > 0 ? count : 0;
  const inFlight = session.live === true || session.status === 'running' ? 1 : 0;
  return Math.max(0, total - inFlight);
}

/**
 * Answers the reader can currently see, including a settled live bubble whose
 * transcript row has not reached the gateway response yet.
 */
export function visibleAnsweredTurnCount(
  session: Session | null | undefined,
  turns: readonly TranscriptTurn[],
  liveStatus: string | null | undefined,
): number {
  const settledTranscriptTurns = turns.filter(
    (turn) => turn.status !== 'running' && turn.status !== 'pending',
  ).length;
  const settledLiveTurn = liveStatus != null && liveStatus !== 'running' ? 1 : 0;
  return Math.max(
    answeredTurnCount(session),
    settledTranscriptTurns + settledLiveTurn,
  );
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
  if (session.live === true || session.status === 'running') return false;
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

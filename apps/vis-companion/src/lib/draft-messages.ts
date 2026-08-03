// Unsent composer text, per session, kept across screens and app restarts.
//
// Named DRAFT MESSAGE, never just "draft": a draft in this system is an
// isolated agent workspace (`/draft new`, `/draft apply`), a different thing.
//
// What you typed is YOURS: leaving the session for the list, backgrounding the
// app, or killing it outright must never eat it. React state alone dies with the
// screen, so the composer mirrors into an in-memory store that is written
// through to persistent storage.
//
// The memory mirror is the read path, which is what makes restore synchronous
// after the first hydrate: reopening a session must paint the draft message on
// the FIRST frame, not flash an empty box and fill it a tick later. Writes are
// debounced (typing must not hit the disk per keystroke) and flushed on every
// way out of the app — visibility change, pagehide, unload.

import { useEffect, useSyncExternalStore } from 'react';
import { Preferences } from '@capacitor/preferences';
import { bridged } from './bridge';
import type { ComposerPaste } from './paste';

const DRAFT_MESSAGES_KEY = 'vis.draftMessages';
/** Sessions that keep a draft message. Oldest-touched entries drop past this. */
const MAX_DRAFT_MESSAGES = 40;
/** Per-message ceiling for collapsed paste bodies; beyond it the paste is dropped. */
const MAX_PASTE_CHARS = 200_000;
const WRITE_DEBOUNCE_MS = 400;

export interface DraftMessage {
  text: string;
  /** Collapsed pastes referenced by `[Pasted #N]` tokens in `text`. */
  pastes: ComposerPaste[];
  /** Highest paste id handed out, so a restored composer keeps numbering. */
  counter: number;
  /** Last touched, for pruning. */
  at: number;
}

export type DraftMessageStore = Record<string, DraftMessage>;

export const EMPTY_DRAFT_MESSAGE: DraftMessage = {
  text: '',
  pastes: [],
  counter: 0,
  at: 0,
};

/**
 * One draft message per (gateway, session): the same sid on another machine is
 * another thread.
 */
export function draftMessageKey(gatewayBase: string, sid: string): string {
  return `${gatewayBase}\u0000${sid}`;
}

let store: DraftMessageStore | null = null;
// Whether disk has been read. Distinct from `store` being non-null: a write can
// create the in-memory store BEFORE hydration, and that must not be mistaken for
// "there is nothing on disk".
let hydrated = false;
let hydration: Promise<DraftMessageStore> | null = null;
let writeTimer: ReturnType<typeof setTimeout> | null = null;
let dirty = false;

// Readers outside the composer — the sessions list asks which empty rows are
// still holding unsent words. They read a SNAPSHOT, replaced on every change,
// because the store itself is mutated in place and its identity never moves.
let snapshot: DraftMessageStore = {};
const listeners = new Set<() => void>();

function announce(): void {
  snapshot = { ...(store ?? {}) };
  for (const listener of listeners) listener();
}

export function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}

function parseStore(raw: string | null): DraftMessageStore {
  if (!raw) return {};
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    const out: DraftMessageStore = {};
    for (const [key, value] of Object.entries(parsed as Record<string, unknown>)) {
      const message = value as Partial<DraftMessage>;
      if (!message || typeof message.text !== 'string') continue;
      out[key] = {
        text: message.text,
        pastes: Array.isArray(message.pastes) ? (message.pastes as ComposerPaste[]) : [],
        counter: typeof message.counter === 'number' ? message.counter : 0,
        at: typeof message.at === 'number' ? message.at : 0,
      };
    }
    return out;
  } catch {
    return {};
  }
}

/**
 * Load the store once. Safe to call from every session screen: concurrent
 * callers share the SAME promise, so draft messages are never read twice or raced.
 */
export async function hydrateDraftMessages(): Promise<DraftMessageStore> {
  if (hydrated && store) return store;
  hydration ??= (async () => {
    let raw: string | null = null;
    // Bounded: a silent native bridge must not leave the composer without its
    // draft forever (see `lib/bridge.ts`); localStorage holds the same value.
    raw = await bridged(
      async () => (await Preferences.get({ key: DRAFT_MESSAGES_KEY })).value ?? null,
      () => {
        try {
          return globalThis.localStorage?.getItem(DRAFT_MESSAGES_KEY) ?? null;
        } catch {
          return null;
        }
      },
    );
    // A write that landed while we were reading wins: it is newer than disk.
    store = { ...parseStore(raw), ...(store ?? {}) };
    hydrated = true;
    announce();
    return store;
  })();
  return hydration;
}

/** The draft message for one session, or the empty one. Synchronous after hydration. */
export function peekDraftMessage(key: string): DraftMessage {
  return store?.[key] ?? EMPTY_DRAFT_MESSAGE;
}

/** The draft message for one session, hydrating storage first if needed. */
export async function readDraftMessage(key: string): Promise<DraftMessage> {
  const loaded = await hydrateDraftMessages();
  return loaded[key] ?? EMPTY_DRAFT_MESSAGE;
}

/**
 * Record what is in the composer right now. Empty text with no pastes REMOVES
 * the entry — a cleared composer must not resurrect on the next visit.
 */
export function writeDraftMessage(
  key: string,
  message: { text: string; pastes?: Iterable<ComposerPaste>; counter?: number },
): void {
  const current = (store ??= {});
  const text = message.text;
  const pastes = Array.from(message.pastes ?? []).filter(
    (paste) => text.includes(paste.token) && paste.content.length <= MAX_PASTE_CHARS,
  );
  if (!text.trim() && !pastes.length) {
    if (!(key in current)) return;
    delete current[key];
  } else {
    const previous = current[key];
    if (
      previous
      && previous.text === text
      && previous.pastes.length === pastes.length
      && previous.pastes.every((paste, i) => paste.token === pastes[i]?.token)
    ) return;
    current[key] = { text, pastes, counter: message.counter ?? 0, at: Date.now() };
  }
  dirty = true;
  schedule();
  announce();
}

/** Forget one session's draft message (it was sent, or its session is gone). */
export function clearDraftMessage(key: string): void {
  writeDraftMessage(key, { text: '' });
}

function prune(current: DraftMessageStore): DraftMessageStore {
  const keys = Object.keys(current);
  if (keys.length <= MAX_DRAFT_MESSAGES) return current;
  const kept = keys
    .sort((a, b) => (current[b]?.at ?? 0) - (current[a]?.at ?? 0))
    .slice(0, MAX_DRAFT_MESSAGES);
  const out: DraftMessageStore = {};
  for (const key of kept) out[key] = current[key];
  return out;
}

function schedule(): void {
  if (writeTimer) clearTimeout(writeTimer);
  writeTimer = setTimeout(() => void flushDraftMessages(), WRITE_DEBOUNCE_MS);
}

/**
 * Persist now. Writes localStorage SYNCHRONOUSLY before awaiting the plugin:
 * the last flush before the app dies happens inside `pagehide`, where an awaited
 * write is not guaranteed to finish, and losing that flush is exactly the bug.
 */
export async function flushDraftMessages(): Promise<void> {
  if (writeTimer) {
    clearTimeout(writeTimer);
    writeTimer = null;
  }
  if (!dirty || !store) return;
  dirty = false;
  store = prune(store);
  const value = JSON.stringify(store);
  try {
    globalThis.localStorage?.setItem(DRAFT_MESSAGES_KEY, value);
  } catch {
    // Private-mode/quota: the plugin write below is still worth attempting.
  }
  await bridged(
    async () => {
      await Preferences.set({ key: DRAFT_MESSAGES_KEY, value });
    },
    // Already mirrored to localStorage above.
    () => undefined,
  );
}

let listening = false;

/** Flush on every exit the platform gives us. Idempotent. */
export function watchDraftMessageExits(): void {
  if (listening || typeof document === 'undefined') return;
  listening = true;
  const flush = () => void flushDraftMessages();
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') flush();
  });
  globalThis.addEventListener?.('pagehide', flush);
  globalThis.addEventListener?.('beforeunload', flush);
}

/**
 * The draft messages this device is holding, re-read on every change.
 *
 * The sessions list needs them: a session you typed into and left is EMPTY on
 * the gateway, and an empty session is hidden — so without this the words (and
 * the session that owns them) were unreachable from the list. Hydration is
 * kicked off here too, because the list may be the first screen to ask.
 */
export function useDraftMessages(): DraftMessageStore {
  useEffect(() => {
    void hydrateDraftMessages();
  }, []);
  return useSyncExternalStore(
    subscribe,
    () => snapshot,
    () => snapshot,
  );
}

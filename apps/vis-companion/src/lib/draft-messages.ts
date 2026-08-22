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
//
// A reader OUTSIDE the composer — the sessions list, which stays mounted behind
// the open transcript — is woken when a session STARTS or STOPS holding unsent
// work, and again whenever the words are persisted (every pause in typing, and
// every way out of the screen). It is never woken per keystroke: its answer is
// fleet-wide, so a character re-ran the filter and the sort of every machine
// and re-rendered every project group for a screen nobody is looking at.
//
// The staged BYTES are a SECOND key, written only when the staged set itself
// changes. A photo is megabytes of base64; re-serializing it into localStorage
// and across the native bridge into UserDefaults every time typing paused stalled
// the composer for seconds on iOS, to store a payload identical to the one
// already there.

import { useEffect, useSyncExternalStore } from 'react';
import { Preferences } from '@capacitor/preferences';
import { bridged } from './bridge';
import type { ComposerPaste } from './paste';
import type { PendingAttachment } from './attachments';

const DRAFT_MESSAGES_KEY = 'vis.draftMessages';
/** The staged bytes, apart from the words that are rewritten on every pause. */
const DRAFT_ATTACHMENTS_KEY = 'vis.draftAttachments';
/** Sessions that keep a draft message. Oldest-touched entries drop past this. */
const MAX_DRAFT_MESSAGES = 40;
/** Per-message ceiling for collapsed paste bodies; beyond it the paste is dropped. */
const MAX_PASTE_CHARS = 200_000;
/**
 * Attachment payload the store may PERSIST, newest draft message first. Memory
 * keeps every attachment you picked; storage is finite, and blowing its quota on
 * photos would take the typed words down with them.
 */
const MAX_STORED_ATTACHMENT_CHARS = 4_000_000;
const WRITE_DEBOUNCE_MS = 400;

export interface DraftMessage {
  text: string;
  /** Collapsed pastes referenced by `[Pasted #N]` tokens in `text`. */
  pastes: ComposerPaste[];
  /** Highest paste id handed out, so a restored composer keeps numbering. */
  counter: number;
  /** Images and files staged in the composer, unsent work exactly like the text. */
  attachments: PendingAttachment[];
  /** Last touched, for pruning. */
  at: number;
}

export type DraftMessageStore = Record<string, DraftMessage>;

export const EMPTY_DRAFT_MESSAGE: DraftMessage = {
  text: '',
  pastes: [],
  attachments: [],
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
// The bytes signature already on disk (see `persistable`). Typing changes the
// words several times a second and the staged pictures not at all; this is what
// keeps the expensive half of the write out of that loop.
let storedBytes = '';

// Readers outside the composer — the sessions list asks which empty rows are
// still holding unsent words. They read a SNAPSHOT, replaced whenever they are
// woken (see the notification contract above), because the store itself is
// mutated in place and its identity never moves.
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

/** The bytes half of the store: attachment id -> base64 data URL. */
function parseBytes(raw: string | null): StoredBytes {
  if (!raw) return {};
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
    const out: StoredBytes = {};
    for (const [id, value] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof value === 'string' && value) out[id] = value;
    }
    return out;
  } catch {
    return {};
  }
}

/**
 * Attachments a previous run persisted, re-joined with their bytes. `previewUrl`
 * IS the base64 data URL, so it is rebuilt here instead of being stored twice —
 * and a descriptor whose bytes did not survive the budget is DROPPED, because a
 * composer chip with nothing behind it is worse than no chip at all.
 */
function parseAttachments(value: unknown, bytes: StoredBytes): PendingAttachment[] {
  if (!Array.isArray(value)) return [];
  const out: PendingAttachment[] = [];
  for (const item of value) {
    const attachment = (item ?? {}) as Partial<PendingAttachment>;
    if (typeof attachment.id !== 'string') continue;
    const base64 = bytes[attachment.id];
    if (!base64) continue;
    if (typeof attachment.filename !== 'string' || typeof attachment.media_type !== 'string') continue;
    out.push({
      id: attachment.id,
      filename: attachment.filename,
      media_type: attachment.media_type,
      base64,
      previewUrl: base64,
      size: typeof attachment.size === 'number' ? attachment.size : base64.length,
    });
  }
  return out;
}

function parseStore(raw: string | null, bytes: StoredBytes): DraftMessageStore {
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
        attachments: parseAttachments((message as { attachments?: unknown }).attachments, bytes),
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
    // Both halves at once: two bounded bridge calls in sequence would double
    // what a silent bridge costs the first composer to open.
    const [raw, rawBytes] = await Promise.all([
      read(DRAFT_MESSAGES_KEY),
      read(DRAFT_ATTACHMENTS_KEY),
    ]);
    const onDisk = parseStore(raw, parseBytes(rawBytes));
    // The signature names what DISK holds. Anything already staged in a live
    // composer is newer than disk and unwritten, so it is not counted here.
    storedBytes = persistable(onDisk).signature;
    // A write that landed while we were reading wins: it is newer than disk.
    store = { ...onDisk, ...(store ?? {}) };
    hydrated = true;
    announce();
    return store;
  })();
  return hydration;
}

/**
 * One stored key. Bounded: a silent native bridge must not leave the composer
 * without its draft forever (see `lib/bridge.ts`); localStorage holds the same
 * value.
 */
function read(key: string): Promise<string | null> {
  return bridged(
    async () => (await Preferences.get({ key })).value ?? null,
    () => {
      try {
        return globalThis.localStorage?.getItem(key) ?? null;
      } catch {
        return null;
      }
    },
  );
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
 * Record what is in the composer right now. Empty text with no pastes and no
 * attachments REMOVES the entry — a cleared composer must not resurrect on the
 * next visit.
 */
export function writeDraftMessage(
  key: string,
  message: {
    text: string;
    pastes?: Iterable<ComposerPaste>;
    attachments?: Iterable<PendingAttachment>;
    counter?: number;
  },
): void {
  const current = (store ??= {});
  // What a reader outside this composer renders: is this session holding unsent
  // work? Compared across the write below, it is the only thing a keystroke can
  // tell them that they did not already know.
  const held = draftMessageHasUnsent(current[key]);
  const text = message.text;
  const pastes = Array.from(message.pastes ?? []).filter(
    (paste) => text.includes(paste.token) && paste.content.length <= MAX_PASTE_CHARS,
  );
  const attachments = Array.from(message.attachments ?? []);
  if (!text.trim() && !pastes.length && !attachments.length) {
    if (!(key in current)) return;
    delete current[key];
  } else {
    const previous = current[key];
    if (
      previous
      && previous.text === text
      && sameKeys(previous.pastes.map((paste) => paste.token), pastes.map((paste) => paste.token))
      && sameKeys(
        previous.attachments.map((attachment) => attachment.id),
        attachments.map((attachment) => attachment.id),
      )
    ) return;
    current[key] = {
      text,
      pastes,
      attachments,
      counter: message.counter ?? 0,
      at: Date.now(),
    };
  }
  dirty = true;
  schedule();
  if (draftMessageHasUnsent(current[key]) !== held) announce();
}

function sameKeys(a: string[], b: string[]): boolean {
  return a.length === b.length && a.every((key, i) => key === b[i]);
}

/** Forget one session's draft message (it was sent, or its session is gone). */
export function clearDraftMessage(key: string): void {
  writeDraftMessage(key, { text: '' });
}

/**
 * This draft message is unsent work. An attachment with no words counts: the
 * picture you picked before walking away is as unsent as a typed sentence, and
 * a list that only looks at `text` drops the session holding it.
 */
export function draftMessageHasUnsent(message: DraftMessage | undefined): boolean {
  return Boolean(message && (message.text.trim() || message.attachments.length > 0));
}

/**
 * Sessions of ONE gateway holding unsent words right now, ascending by id.
 *
 * The single fact about the navigator list the gateway cannot know. It rides
 * down with every list read (`dirty=`), so the row of an untitled session whose
 * composer is full is KEPT and banded by whoever owns the order, instead of
 * being hidden there and rescued back here. Sorted, because the same overlay
 * has to be the same STRING: it is part of the key a window's validator is
 * pinned under.
 */
export function dirtySessionIds(gatewayBase: string): string[] {
  const prefix = `${gatewayBase}\u0000`;
  const ids: string[] = [];
  for (const [key, message] of Object.entries(store ?? {})) {
    if (!key.startsWith(prefix)) continue;
    if (draftMessageHasUnsent(message)) ids.push(key.slice(prefix.length));
  }
  return ids.sort();
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

/** What a message stores about an attachment: everything except the bytes. */
type StoredAttachment = Omit<PendingAttachment, 'previewUrl' | 'base64'>;
type StoredMessage = Omit<DraftMessage, 'attachments'> & { attachments: StoredAttachment[] };
/** The bytes, by attachment id — the half of the store that typing never touches. */
type StoredBytes = Record<string, string>;

/**
 * What goes to disk, split by how often it changes: the words (rewritten on
 * every pause in typing) and the bytes (rewritten only when a picture is staged
 * or removed).
 *
 * Attachments are base64, so a couple of photos dwarf every word in the store:
 * past the budget the NEWEST draft messages keep their attachments and older
 * ones are persisted as text alone. Memory is untouched — dropping a picture
 * from storage must never drop it from a composer you are still looking at.
 *
 * `signature` names the bytes payload without holding it: the same ids, of the
 * same lengths, in the same order produce byte-identical JSON, which is what
 * lets the flush skip writing it again.
 */
function persistable(current: DraftMessageStore): {
  messages: Record<string, StoredMessage>;
  bytes: StoredBytes;
  signature: string;
} {
  const messages: Record<string, StoredMessage> = {};
  const bytes: StoredBytes = {};
  const staged: string[] = [];
  let budget = MAX_STORED_ATTACHMENT_CHARS;
  const newestFirst = Object.keys(current)
    .sort((a, b) => (current[b]?.at ?? 0) - (current[a]?.at ?? 0));
  for (const key of newestFirst) {
    const message = current[key];
    const attachments: StoredAttachment[] = [];
    for (const attachment of message.attachments) {
      if (attachment.base64.length > budget) continue;
      budget -= attachment.base64.length;
      bytes[attachment.id] = attachment.base64;
      staged.push(`${attachment.id}:${attachment.base64.length}`);
      attachments.push({
        id: attachment.id,
        filename: attachment.filename,
        media_type: attachment.media_type,
        size: attachment.size,
      });
    }
    messages[key] = { ...message, attachments };
  }
  return { messages, bytes, signature: staged.join(',') };
}

function schedule(): void {
  if (writeTimer) clearTimeout(writeTimer);
  writeTimer = setTimeout(() => void flushDraftMessages(), WRITE_DEBOUNCE_MS);
}

/**
 * Mirror one key into localStorage, SYNCHRONOUSLY. The last flush before the
 * app dies happens inside `pagehide`, where even a microtask may not get to
 * run, and losing that flush is exactly the bug.
 */
function mirror(key: string, value: string): void {
  try {
    globalThis.localStorage?.setItem(key, value);
  } catch {
    // Private-mode/quota: the plugin write is still worth attempting.
  }
}

/** The durable half of the same write, bounded like every bridge call. */
function push(key: string, value: string): Promise<void> {
  return bridged(
    async () => {
      await Preferences.set({ key, value });
    },
    // Already mirrored to localStorage above.
    () => undefined,
  );
}

/**
 * Persist now. The words go every time; the BYTES only when the staged set
 * itself changed.
 *
 * Typing beside a staged photo changes the text and nothing else, so
 * re-serializing that photo and pushing the same megabytes through
 * `localStorage` and the native bridge into `UserDefaults` at every pause in
 * typing bought a payload byte-identical to the one already on disk — and cost
 * the composer a visible stall on the phone every time.
 *
 * This is also where the WORDS reach a reader outside the composer: persisting
 * is the one moment they are settled — every pause in typing, and every way out
 * of the screen — so the sessions list is woken here rather than per keystroke.
 */
export async function flushDraftMessages(): Promise<void> {
  if (writeTimer) {
    clearTimeout(writeTimer);
    writeTimer = null;
  }
  if (!dirty || !store) return;
  dirty = false;
  store = prune(store);
  announce();
  const { messages, bytes, signature } = persistable(store);
  const words = JSON.stringify(messages);
  // Bytes FIRST: a kill between the two writes may then orphan bytes — harmless,
  // and replaced by the next write of the set — but never leaves a descriptor
  // pointing at a picture that is not there.
  const staged = signature === storedBytes ? null : JSON.stringify(bytes);
  if (staged !== null) mirror(DRAFT_ATTACHMENTS_KEY, staged);
  mirror(DRAFT_MESSAGES_KEY, words);
  if (staged !== null) {
    storedBytes = signature;
    await push(DRAFT_ATTACHMENTS_KEY, staged);
  }
  await push(DRAFT_MESSAGES_KEY, words);
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

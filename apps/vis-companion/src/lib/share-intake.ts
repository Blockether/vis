// Links and text handed to vis by the SYSTEM share sheet (iOS), an Android
// `ACTION_SEND`, or a Shortcuts/Siri run.
//
// All three arrive the same way: the native side rewrites whatever it was given
// into `vis://share?url=…&text=…&title=…` and opens it. That keeps one code path
// on the web side and one contract to test — the platforms differ only in how
// they get to that URL.
//
// A share is a HANDOFF, not a message: the payload has to survive the trip from
// "tapped Share in Safari" to "the session screen finished mounting", which
// crosses a cold start on every platform. So it lands in a pending slot backed
// by storage the moment it arrives, and only landing in a composer removes it.
//
// Listeners exist for the warm case (the app is already on a session): the drop
// is delivered immediately instead of waiting for a remount.

import { Preferences } from '@capacitor/preferences';
import { bridged } from './bridge';

const PENDING_SHARE_KEY = 'vis.pendingShare';

/** A share nobody came back for. Long enough to survive a weekend offline. */
const MAX_AGE_MS = 7 * 24 * 60 * 60 * 1000;

/** Ceiling for one shared body; past it the text is truncated, never dropped. */
const MAX_SHARE_CHARS = 100_000;

export interface SharedPayload {
  /** The shared link, when the source gave one. */
  url?: string;
  /** Selected/typed text, or a note attached to the link. */
  text?: string;
  /** Page or document title, when the source gave one. */
  title?: string;
  /** When it arrived, for staleness. */
  at?: number;
}

/**
 * Read a `vis://share?…` handoff. Returns null for every other URL, including
 * `vis://gateway` pairing links, so callers can chain the deep-link handlers.
 *
 * Accepts `vis://share` and `vis:///share` — WebKit and Android normalise the
 * slashes differently. Query keys beyond url/text/title (the `at` nonce the
 * native senders add so repeat shares are distinct URLs) are ignored.
 */
export function parseShareLink(raw: string): SharedPayload | null {
  if (!raw) return null;
  let parsed: URL;
  try {
    parsed = new URL(raw);
  } catch {
    return null;
  }
  const path = `${parsed.host}${parsed.pathname}`.replace(/^\/+|\/+$/g, '');
  if (path !== 'share') return null;
  const params = parsed.searchParams;
  const share = normalizeShare({
    url: params.get('url') ?? undefined,
    text: params.get('text') ?? undefined,
    title: params.get('title') ?? undefined,
  });
  // `vis://share` with nothing on it is a launch, not a handoff.
  return share.url || share.text || share.title ? share : null;
}

function clip(value: string | undefined): string | undefined {
  if (typeof value !== 'string') return undefined;
  const trimmed = value.trim();
  if (!trimmed) return undefined;
  return trimmed.length > MAX_SHARE_CHARS ? trimmed.slice(0, MAX_SHARE_CHARS) : trimmed;
}

function normalizeShare(share: SharedPayload): SharedPayload {
  const url = clip(share.url);
  const text = clip(share.text);
  const title = clip(share.title);
  const out: SharedPayload = {};
  if (url) out.url = url;
  // Sharing a link from Safari sends the URL as `text` as well; keeping both
  // would paste the same link twice.
  if (text && text !== url) out.text = text;
  if (title && title !== url && title !== text) out.title = title;
  return out;
}

/**
 * What a share looks like in the composer: the link on its own line so it stays
 * clickable and greppable, the title as context, free text as written.
 */
export function formatShare(share: SharedPayload): string {
  const lines: string[] = [];
  if (share.title) lines.push(share.title);
  if (share.url) lines.push(share.url);
  if (share.text) lines.push(share.text);
  return lines.join('\n');
}

/**
 * Fold a share into whatever is already typed. Never overwrites the composer:
 * dumping ten links in a row must accumulate, and a half-written prompt must
 * survive the interruption.
 */
export function appendSharedText(existing: string, share: SharedPayload): string {
  const addition = formatShare(share);
  if (!addition) return existing;
  if (!existing.trim()) return addition;
  return `${existing.replace(/\s+$/, '')}\n${addition}`;
}

type ShareListener = (share: SharedPayload) => void;

const listeners = new Set<ShareListener>();
let pending: SharedPayload | null = null;
let hydrated = false;
let hydration: Promise<SharedPayload | null> | null = null;

function parseStored(raw: string | null): SharedPayload | null {
  if (!raw) return null;
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return null;
    const share = normalizeShare(parsed as SharedPayload);
    if (!share.url && !share.text && !share.title) return null;
    const at = (parsed as SharedPayload).at;
    if (typeof at === 'number' && Date.now() - at > MAX_AGE_MS) return null;
    return { ...share, at: typeof at === 'number' ? at : Date.now() };
  } catch {
    return null;
  }
}

async function persist(share: SharedPayload | null): Promise<void> {
  const value = share ? JSON.stringify(share) : null;
  try {
    if (value === null) globalThis.localStorage?.removeItem(PENDING_SHARE_KEY);
    else globalThis.localStorage?.setItem(PENDING_SHARE_KEY, value);
  } catch {
    // Private-mode/quota: the plugin write below is still worth attempting.
  }
  await bridged(
    async () => {
      if (value === null) await Preferences.remove({ key: PENDING_SHARE_KEY });
      else await Preferences.set({ key: PENDING_SHARE_KEY, value });
    },
    // Already mirrored to localStorage above.
    () => undefined,
  );
}

/**
 * Write the pending slot out. NEVER before the stored copy has been read back:
 * a drop that arrives during a cold start must not overwrite the share still
 * sitting on disk from the last launch.
 */
async function flush(): Promise<void> {
  if (!hydrated) await hydratePendingShare();
  await persist(pending);
}

/**
 * Load a share that arrived before this webview existed. Concurrent callers
 * share the SAME promise, so a cold-start handoff is never read twice.
 */
export async function hydratePendingShare(): Promise<SharedPayload | null> {
  if (hydrated) return pending;
  hydration ??= (async () => {
    const raw = await bridged(
      async () => (await Preferences.get({ key: PENDING_SHARE_KEY })).value ?? null,
      () => {
        try {
          return globalThis.localStorage?.getItem(PENDING_SHARE_KEY) ?? null;
        } catch {
          return null;
        }
      },
    );
    hydrated = true;
    const stored = parseStored(raw);
    if (stored) {
      // A share can land WHILE the read is in flight. Neither may win: the
      // parked one is older, so it leads, and the live drop follows it.
      const live = pending;
      pending = live
        ? { text: appendSharedText(formatShare(stored), live), at: Date.now() }
        : stored;
    }
    return pending;
  })();
  return hydration;
}

/**
 * A share arrived. Held until a composer takes it, and handed to every live
 * listener now. Multiple shares before a drain COALESCE — dumping links while
 * the session screen is closed must not lose all but the last one.
 */
export function receiveSharedText(share: SharedPayload): SharedPayload | null {
  const normalized = normalizeShare(share);
  if (!normalized.url && !normalized.text && !normalized.title) return null;
  const merged: SharedPayload = pending
    ? { text: appendSharedText(formatShare(pending), normalized), at: Date.now() }
    : { ...normalized, at: Date.now() };
  pending = merged;
  void flush();
  for (const listener of listeners) listener(merged);
  return merged;
}

/** The share still owed a composer, if any. Synchronous after hydration. */
export function peekPendingShare(): SharedPayload | null {
  return pending;
}

/** The share landed in a composer. Removes it so it is pasted exactly once. */
export function takePendingShare(): SharedPayload | null {
  const share = pending;
  pending = null;
  void flush();
  return share;
}

/** Watch for shares that arrive while a screen is already mounted. */
export function onSharedText(handler: ShareListener): () => void {
  listeners.add(handler);
  return () => {
    listeners.delete(handler);
  };
}

/** Tests only: forget in-memory state so each case starts clean. */
export function resetShareIntakeForTests(): void {
  listeners.clear();
  pending = null;
  hydrated = false;
  hydration = null;
}

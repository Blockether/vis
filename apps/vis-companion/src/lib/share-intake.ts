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

/** Files staged by the native side; more than this and the composer refuses. */
const MAX_SHARE_FILES = 8;

/**
 * One file the OS handed over, already COPIED by the native side into a place
 * this process can read: the Android cache dir, the iOS App Group container.
 * A `content://` URI or a share extension's own temp URL is unreadable from
 * here, so the path in a `vis://share` link is always a staged copy we own —
 * and delete the moment its bytes are in the composer.
 */
export interface SharedFile {
  /** Absolute path (or `file://` URL) of the staged copy. */
  path: string;
  /** The name the source showed the human. */
  name: string;
  /** The media type the platform claimed, when it claimed one. */
  type?: string;
}
export interface SharedPayload {
  /** The shared link, when the source gave one. */
  url?: string;
  /** Selected/typed text, or a note attached to the link. */
  text?: string;
  /** Page or document title, when the source gave one. */
  title?: string;
  /** When it arrived, for staleness. */
  at?: number;
  /** Staged files, in the order the OS listed them. */
  files?: SharedFile[];
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
  // Files ride as repeated `file=` params, with `name=`/`type=` aligned by
  // INDEX when the platform knew them — one memo and one photo arriving in the
  // same share needs no nested encoding, and a sender that knows only paths
  // still produces a share this reads.
  const paths = params.getAll('file');
  const names = params.getAll('name');
  const types = params.getAll('type');
  const share = normalizeShare({
    url: params.get('url') ?? undefined,
    text: params.get('text') ?? undefined,
    title: params.get('title') ?? undefined,
    files: paths.map((path, at) => ({
      path,
      name: names[at] ?? '',
      type: types[at] ?? undefined,
    })),
  });
  // `vis://share` with nothing on it is a launch, not a handoff.
  return hasSharedContent(share) ? share : null;
}

function clip(value: string | undefined): string | undefined {
  if (typeof value !== 'string') return undefined;
  const trimmed = value.trim();
  if (!trimmed) return undefined;
  return trimmed.length > MAX_SHARE_CHARS ? trimmed.slice(0, MAX_SHARE_CHARS) : trimmed;
}

/** The name to show when the sender gave none: whatever the path ends with. */
function fileName(path: string, given: string | undefined): string {
  const named = clip(given);
  if (named) return named;
  const tail = path.split(/[?#]/u)[0]?.split('/').pop() ?? '';
  try {
    return decodeURIComponent(tail) || 'shared-file';
  } catch {
    return tail || 'shared-file';
  }
}

function normalizeFiles(files: SharedFile[] | undefined): SharedFile[] {
  if (!Array.isArray(files)) return [];
  const seen = new Set<string>();
  const out: SharedFile[] = [];
  for (const file of files) {
    const path = clip(file?.path);
    // The same file shared twice is one attachment; the staged copy is one file
    // on disk and reading it twice would attach the memo two times.
    if (!path || seen.has(path)) continue;
    seen.add(path);
    const type = clip(file?.type);
    const name = fileName(path, file?.name);
    out.push(type ? { path, name, type } : { path, name });
    if (out.length === MAX_SHARE_FILES) break;
  }
  return out;
}

/** Is there anything to hand a composer? Files count, not just words. */
export function hasSharedContent(share: SharedPayload | null | undefined): boolean {
  return !!share && !!(share.url || share.text || share.title || share.files?.length);
}
function normalizeShare(share: SharedPayload): SharedPayload {
  const url = clip(share.url);
  const text = clip(share.text);
  const title = clip(share.title);
  const files = normalizeFiles(share.files);
  const out: SharedPayload = {};
  if (files.length) out.files = files;
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
 * The share in the words a band can hold — the file when there is one, the
 * count when there are several, the link otherwise. Files lead: a memo is what
 * the human watched leave the other app.
 */
export function shareSummary(share: SharedPayload | null | undefined): string {
  if (!hasSharedContent(share)) return '';
  const files = share?.files ?? [];
  if (files.length === 1) return files[0]?.name ?? '';
  if (files.length > 1) return `${files.length} files`;
  return share?.title ?? share?.url ?? share?.text ?? '';
}

/**
 * Fold two shares into one. Text accumulates the way a second dropped link
 * does; files accumulate as files, because a memo cannot be pasted into a
 * sentence.
 */
function mergeShares(older: SharedPayload, newer: SharedPayload): SharedPayload {
  const text = appendSharedText(formatShare(older), newer);
  const files = normalizeFiles([...(older.files ?? []), ...(newer.files ?? [])]);
  const merged: SharedPayload = { at: Date.now() };
  if (text) merged.text = text;
  if (files.length) merged.files = files;
  return merged;
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

// `null` says a composer TOOK the parked share — the screens that report one
// have to stop reporting it, and they learn that here rather than by polling.
type ShareListener = (share: SharedPayload | null) => void;

const listeners = new Set<ShareListener>();
let pending: SharedPayload | null = null;
// A share names a payload, never a destination. Until the human has picked one —
// a row in the list, a fresh session, a fork — no composer may take it, or the
// session that happens to be mounted swallows the memo before the list can even
// offer the choice.
let claimed = false;
let hydrated = false;
let hydration: Promise<SharedPayload | null> | null = null;

function parseStored(raw: string | null): SharedPayload | null {
  if (!raw) return null;
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return null;
    const share = normalizeShare(parsed as SharedPayload);
    if (!hasSharedContent(share)) return null;
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
      pending = live ? mergeShares(stored, live) : stored;
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
  if (!hasSharedContent(normalized)) return null;
  const merged: SharedPayload = pending
    ? mergeShares(pending, normalized)
    : { ...normalized, at: Date.now() };
  pending = merged;
  // A new payload is a new question. Whatever destination was named applies to
  // what was already taken, not to what just landed.
  claimed = false;
  void flush();
  notify(merged);
  return merged;
}

/** The share still owed a composer, if any. Synchronous after hydration. */
export function peekPendingShare(): SharedPayload | null {
  return pending;
}

function notify(share: SharedPayload | null): void {
  for (const listener of listeners) listener(share);
}
/**
 * The human named a destination for the parked share. Only after this may a
 * composer take it; the notify is what lets the session that is ALREADY open
 * drain without remounting.
 */
export function claimPendingShare(): SharedPayload | null {
  if (!pending) return null;
  claimed = true;
  notify(pending);
  return pending;
}

/**
 * The share landed in a composer. Removes it so it is pasted exactly once, and
 * answers null while no destination has been claimed.
 */
export function takePendingShare(): SharedPayload | null {
  // Always through `flush()`, even on an empty take: a take can happen before the
  // cold-start read has landed, and `flush()` is what waits for it — returning
  // early here would leave a parked share unhydrated and lost.
  if (!claimed) {
    void flush();
    return null;
  }
  const share = pending;
  pending = null;
  claimed = false;
  void flush();
  // No notify on an empty take, though: a listener that drains on notify would
  // answer its own message forever.
  if (!share) return null;
  notify(null);
  return share;
}

/**
 * The human threw the share away. Unlike a take this needs no claim — refusing
 * a payload is a destination too — and it answers what was dropped so the staged
 * copies can go with it.
 */
export function dropPendingShare(): SharedPayload | null {
  const share = pending;
  pending = null;
  claimed = false;
  void flush();
  if (!share) return null;
  notify(null);
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
  claimed = false;
  hydrated = false;
  hydration = null;
}

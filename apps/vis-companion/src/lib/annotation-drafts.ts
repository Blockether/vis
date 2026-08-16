// Comments a reader ADDED to a document but has not SAVED back yet, kept on the
// device.
//
// Saving a note is a POST that becomes the next VERSION of that artifact, so
// "Add comment" and "Save" are deliberately two presses: three remarks ship as
// one revision instead of three. Everything between those presses lived in React
// state alone — and a document is read on a phone, where leaving is one stray
// swipe: the overlay unmounts, the state dies with it, and the remark that was
// just typed is gone without ever saying so. Reported exactly that way: add a
// comment, leave the artifact by accident, come back, and it should tell you the
// work is still there and still unsaved.
//
// So the unsaved comment SET is mirrored here, keyed by the document it belongs
// to, and the annotator restores it on the next open and reports `Unsaved draft`
// until a save lands. Both stores are written for the same reason `lib/storage`
// writes both: localStorage answers SYNCHRONOUSLY, so a restored draft is on the
// first frame rather than a flash of the saved document corrected a tick later,
// while Preferences is the durable half that survives a webview data reset. The
// native call is bounded (`lib/bridge`) — a wedged bridge must never be the
// reason a press does nothing.

import { Preferences } from "@capacitor/preferences";
import { bridged } from "./bridge";
import type { MarkdownComment } from "./markdown-annotations";

const DRAFTS_KEY = "vis.annotationDrafts";

/** Documents that may hold a draft at once; the oldest touched fall off. */
export const MAX_DRAFT_DOCUMENTS = 20;

/** One document's unsaved comments, and when they were last touched. */
interface AnnotationDraft {
  comments: MarkdownComment[];
  at: number;
}

type DraftStore = Record<string, AnnotationDraft>;

// The read path, so a reopened document does not wait for storage. It is filled
// from localStorage on the first ask and kept in step with every write.
let memory: DraftStore | null = null;

function localGet(key: string): string | null {
  try {
    return globalThis.localStorage?.getItem(key) ?? null;
  } catch {
    return null;
  }
}

function localSet(key: string, value: string): void {
  try {
    globalThis.localStorage?.setItem(key, value);
  } catch {
    // Private mode / quota: the Preferences write is the durable one anyway.
  }
}

function parse(raw: string | null): DraftStore {
  if (!raw) return {};
  try {
    const parsed: unknown = JSON.parse(raw);
    if (!parsed || typeof parsed !== "object") return {};
    const store: DraftStore = {};
    for (const [key, value] of Object.entries(parsed as DraftStore)) {
      if (Array.isArray(value?.comments)) {
        store[key] = { comments: value.comments, at: value.at ?? 0 };
      }
    }
    return store;
  } catch {
    return {};
  }
}

function load(): DraftStore {
  if (memory === null) memory = parse(localGet(DRAFTS_KEY));
  return memory;
}

/** Per key, the entry that was touched last — the two stores may disagree. */
function newest(a: DraftStore, b: DraftStore): DraftStore {
  const merged: DraftStore = { ...a };
  for (const [key, entry] of Object.entries(b)) {
    const have = merged[key];
    if (!have || have.at < entry.at) merged[key] = entry;
  }
  return merged;
}

/** Keep the newest documents only: a store nobody prunes eventually blows quota. */
function pruned(store: DraftStore): DraftStore {
  const keys = Object.keys(store);
  if (keys.length <= MAX_DRAFT_DOCUMENTS) return store;
  const kept = keys
    .sort((one, two) => (store[two]?.at ?? 0) - (store[one]?.at ?? 0))
    .slice(0, MAX_DRAFT_DOCUMENTS);
  const next: DraftStore = {};
  for (const key of kept) next[key] = store[key];
  return next;
}

function persist(next: DraftStore): void {
  memory = next;
  const value = JSON.stringify(next);
  localSet(DRAFTS_KEY, value);
  void bridged(
    async () => {
      await Preferences.set({ key: DRAFTS_KEY, value });
    },
    () => undefined,
  );
}

/**
 * WHICH document this draft belongs to: a filename is not an identity — the same
 * `PLAN.md` exists in every session, and the same session exists on every machine
 * this device has paired.
 */
export function annotationDraftKey(
  gatewayBase: string,
  sid: string,
  iterationId: string,
  name: string,
): string {
  return [gatewayBase, sid, iterationId, name].join("\u0000");
}

/** The unsaved comments for one document, without waiting for any store. */
export function peekAnnotationDraft(key: string): MarkdownComment[] | null {
  return load()[key]?.comments ?? null;
}

/**
 * The same question, asked of the DURABLE store as well: localStorage is the one
 * a webview data reset empties, and the draft has to outlive that too. The answer
 * folds into the memory mirror, so the next open is synchronous again.
 */
export async function readAnnotationDraft(
  key: string,
): Promise<MarkdownComment[] | null> {
  const durable = await bridged(
    async () => (await Preferences.get({ key: DRAFTS_KEY })).value ?? null,
    () => localGet(DRAFTS_KEY),
  );
  const merged = newest(load(), parse(durable));
  memory = merged;
  localSet(DRAFTS_KEY, JSON.stringify(merged));
  return merged[key]?.comments ?? null;
}

export function writeAnnotationDraft(
  key: string,
  comments: MarkdownComment[],
): void {
  persist(pruned({ ...load(), [key]: { comments, at: Date.now() } }));
}

/** The draft is spent: it either saved, or it never differed from the document. */
export function clearAnnotationDraft(key: string): void {
  const store = load();
  if (!(key in store)) return;
  const next = { ...store };
  delete next[key];
  persist(next);
}

/**
 * Whether two comment sets say the same thing. A draft equal to what the file
 * already carries is not unsaved work — it is a leftover from the save that
 * landed, and it must not reopen the document as a draft.
 */
export function sameComments(
  one: readonly MarkdownComment[],
  two: readonly MarkdownComment[],
): boolean {
  return (
    one.length === two.length &&
    one.every(
      (entry, at) =>
        entry.quote === two[at]?.quote && entry.body === two[at]?.body,
    )
  );
}

/** Test seam: forget what this module cached without touching either store. */
export function resetAnnotationDraftCache(): void {
  memory = null;
}

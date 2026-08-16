// @vitest-environment jsdom
// THE DEVICE'S MEMORY OF REMARKS THAT WERE TYPED BUT NEVER SAVED.
//
// Regression, user report ("in the device memory, if I add a comment but accidentally leave
// the artifact and come back, it should say this is an unsaved draft"): everything between
// "Add comment" and "Save" lived in one component's state, and leaving a document on a phone
// is one stray swipe. These pin the two halves of the store — the synchronous mirror a
// reopened document reads on its first frame, and the durable one that outlives a webview
// data reset — and the rule that decides a leftover is not work.

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

// `vi.mock` factories run before this file's module scope exists; the fake native side has
// to be hoisted with them.
const native = vi.hoisted(() => ({
  store: new Map<string, string>(),
  /** A bridge that never answers back is the failure `lib/bridge` exists for. */
  isWedged: false,
}));

vi.mock("@capacitor/preferences", () => ({
  Preferences: {
    get: async ({ key }: { key: string }) =>
      native.isWedged
        ? new Promise<{ value: string | null }>(() => undefined)
        : { value: native.store.get(key) ?? null },
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
    remove: async ({ key }: { key: string }) => {
      native.store.delete(key);
    },
  },
}));

import {
  annotationDraftKey,
  clearAnnotationDraft,
  MAX_DRAFT_DOCUMENTS,
  peekAnnotationDraft,
  readAnnotationDraft,
  resetAnnotationDraftCache,
  sameComments,
  writeAnnotationDraft,
} from "./annotation-drafts";
import { BRIDGE_TIMEOUT_MS } from "./bridge";

/** Where both halves keep it; a storage key is a contract with every past install. */
const DRAFTS_KEY = "vis.annotationDrafts";

const key = annotationDraftKey("http://10.0.0.5:7777", "s1", "i1", "PLAN.md");
const remark = [{ quote: "We cut on Friday.", body: "Stale." }];

/** Let the write's own bridge call land: it is fired, never awaited. */
const settle = () => new Promise((done) => setTimeout(done, 0));

beforeEach(() => {
  native.store.clear();
  native.isWedged = false;
  globalThis.localStorage.clear();
  resetAnnotationDraftCache();
});

afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe("the unsaved-comment store", () => {
  it("keys a draft by the document, not by its filename", () => {
    // The same `PLAN.md` exists in every session, and the same session exists on every
    // machine this device has paired with.
    expect(annotationDraftKey("http://10.0.0.5:7777", "s2", "i1", "PLAN.md")).not.toBe(key);
    expect(annotationDraftKey("http://10.0.0.5:7777", "s1", "i2", "PLAN.md")).not.toBe(key);
    expect(annotationDraftKey("http://10.0.0.6:7777", "s1", "i1", "PLAN.md")).not.toBe(key);
    expect(annotationDraftKey("http://10.0.0.5:7777", "s1", "i1", "PLAN.md")).toBe(key);
  });

  it("answers on the first frame, without waiting for any store", () => {
    expect(peekAnnotationDraft(key)).toBeNull();
    writeAnnotationDraft(key, remark);
    expect(peekAnnotationDraft(key)).toEqual(remark);

    // Even after this module forgets everything it cached: localStorage answers
    // SYNCHRONOUSLY, so a restored draft is on the first frame rather than a flash of the
    // saved document corrected a tick later.
    resetAnnotationDraftCache();
    expect(peekAnnotationDraft(key)).toEqual(remark);
  });

  it("reaches the durable store too, so a webview data reset is survivable", async () => {
    writeAnnotationDraft(key, remark);
    await settle();
    expect(native.store.get(DRAFTS_KEY)).toContain("Stale.");

    globalThis.localStorage.clear();
    resetAnnotationDraftCache();
    await expect(readAnnotationDraft(key)).resolves.toEqual(remark);
  });

  it("takes whichever store was touched last", async () => {
    writeAnnotationDraft(key, remark);
    await settle();
    // Another install of the app wrote this document later than this one did.
    native.store.set(
      DRAFTS_KEY,
      JSON.stringify({
        [key]: { comments: [{ quote: "", body: "Newer." }], at: Date.now() + 60_000 },
      }),
    );
    resetAnnotationDraftCache();

    const newest = [{ quote: "", body: "Newer." }];
    await expect(readAnnotationDraft(key)).resolves.toEqual(newest);
    // And the answer folds back into the mirror, so the next open is synchronous again.
    expect(peekAnnotationDraft(key)).toEqual(newest);
  });

  it("answers from the mirror when the bridge never replies", async () => {
    writeAnnotationDraft(key, remark);
    await settle();
    native.isWedged = true;
    resetAnnotationDraftCache();

    vi.useFakeTimers();
    const pending = readAnnotationDraft(key);
    await vi.advanceTimersByTimeAsync(BRIDGE_TIMEOUT_MS + 10);
    await expect(pending).resolves.toEqual(remark);
  });

  it("forgets a draft that is spent, in both halves", async () => {
    writeAnnotationDraft(key, remark);
    await settle();
    clearAnnotationDraft(key);
    await settle();

    expect(peekAnnotationDraft(key)).toBeNull();
    expect(native.store.get(DRAFTS_KEY)).not.toContain("Stale.");
    expect(globalThis.localStorage.getItem(DRAFTS_KEY)).not.toContain("Stale.");
  });

  it("keeps the newest documents only, so the store cannot grow without end", () => {
    // Distinct instants: the prune keeps by `at`, and a loop runs inside one millisecond.
    let tick = 1_000;
    vi.spyOn(Date, "now").mockImplementation(() => (tick += 1_000));

    const named = (name: string) =>
      annotationDraftKey("http://10.0.0.5:7777", "s1", "i1", name);
    for (let index = 0; index <= MAX_DRAFT_DOCUMENTS; index += 1) {
      writeAnnotationDraft(named(`note-${index}.md`), [
        { quote: "", body: `remark ${index}` },
      ]);
    }

    const kept = JSON.parse(globalThis.localStorage.getItem(DRAFTS_KEY) ?? "{}") as Record<
      string,
      unknown
    >;
    expect(Object.keys(kept)).toHaveLength(MAX_DRAFT_DOCUMENTS);
    expect(peekAnnotationDraft(named(`note-${MAX_DRAFT_DOCUMENTS}.md`))).not.toBeNull();
    expect(peekAnnotationDraft(named("note-0.md"))).toBeNull();
  });
});

// A DRAFT EQUAL TO THE FILE IS NOT WORK. Without this the document that just saved reopens
// reporting unsaved work forever, because the draft it left behind says exactly what the
// file now carries.
describe("telling a draft from a leftover", () => {
  it("compares remarks by what they say, in the order they say it", () => {
    expect(sameComments([], [])).toBe(true);
    expect(sameComments(remark, [{ quote: "We cut on Friday.", body: "Stale." }])).toBe(true);
    expect(sameComments(remark, [{ quote: "We cut on Friday.", body: "stale." }])).toBe(false);
    expect(sameComments(remark, [{ quote: "We ship on Friday.", body: "Stale." }])).toBe(false);
    expect(sameComments(remark, [])).toBe(false);

    const two = [
      { quote: "one", body: "first" },
      { quote: "two", body: "second" },
    ];
    expect(sameComments(two, [...two].reverse())).toBe(false);
  });
});

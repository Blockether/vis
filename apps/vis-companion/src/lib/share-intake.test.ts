// A share is a handoff across a process boundary: the system share sheet, an
// Android `ACTION_SEND`, or a Shortcuts run hands vis a payload and then the
// app may be COLD STARTED to receive it. Everything below exists because that
// trip is where shares get lost — duplicated links, a drop racing the read of
// the parked one, a take that fires before storage has been read back.
//
// The native side is covered by the platform builds; this pins the web-side
// contract every platform funnels into.

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

// `vi.mock` factories run at import time, before module-scope `const`s of this
// file exist — the shared state has to be hoisted with them.
const native = vi.hoisted(() => ({
  store: new Map<string, string>(),
  getCalls: 0,
  /** Set by a test to stall or break the bridge for the next `get`. */
  onGet: null as null | (() => Promise<void>),
}));

vi.mock('@capacitor/preferences', () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => {
      native.getCalls += 1;
      if (native.onGet) await native.onGet();
      return { value: native.store.get(key) ?? null };
    },
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
    remove: async ({ key }: { key: string }) => {
      native.store.delete(key);
    },
  },
}));

import {
  appendSharedText,
  formatShare,
  hydratePendingShare,
  onSharedText,
  parseShareLink,
  peekPendingShare,
  receiveSharedText,
  resetShareIntakeForTests,
  takePendingShare,
  shareSummary,
} from './share-intake';
import type { SharedPayload } from './share-intake';

const KEY = 'vis.pendingShare';
const DAY_MS = 24 * 60 * 60 * 1000;

/** Let the fire-and-forget persistence inside receive/take reach storage. */
const settle = () => new Promise((resolve) => setTimeout(resolve, 0));

const makeLocalStorage = () => {
  const map = new Map<string, string>();
  return {
    getItem: (key: string) => map.get(key) ?? null,
    setItem: (key: string, value: string) => {
      map.set(key, value);
    },
    removeItem: (key: string) => {
      map.delete(key);
    },
    clear: () => {
      map.clear();
    },
    key: (index: number) => [...map.keys()][index] ?? null,
    get length() {
      return map.size;
    },
  } as unknown as Storage;
};

beforeEach(() => {
  native.store.clear();
  native.getCalls = 0;
  native.onGet = null;
  globalThis.localStorage = makeLocalStorage();
  resetShareIntakeForTests();
});

afterEach(() => {
  resetShareIntakeForTests();
});

describe('parseShareLink', () => {
  it('reads a shared link', () => {
    expect(parseShareLink('vis://share?url=https%3A%2F%2Fexample.com%2Fa')).toEqual({
      url: 'https://example.com/a',
    });
  });

  it('reads shared text and title', () => {
    expect(parseShareLink('vis://share?text=hello%20there&title=Notes')).toEqual({
      text: 'hello there',
      title: 'Notes',
    });
  });

  it('accepts the triple-slash form WebKit and Android disagree about', () => {
    expect(parseShareLink('vis:///share?text=hi')).toEqual({ text: 'hi' });
  });

  it('ignores the `at` nonce the native senders add to defeat deep-link dedupe', () => {
    expect(parseShareLink('vis://share?text=hi&at=1730000000000&extra=x')).toEqual({ text: 'hi' });
  });

  it('leaves other vis deep links to their own handlers', () => {
    expect(parseShareLink('vis://gateway?host=10.0.0.5&token=t')).toBeNull();
  });

  it('does not claim https links: no universal link is registered', () => {
    expect(parseShareLink('https://gateway.example.com/share?text=hi')).toBeNull();
  });

  it('returns null for garbage', () => {
    expect(parseShareLink('not a url')).toBeNull();
    expect(parseShareLink('')).toBeNull();
  });

  it('treats a bare launch as no handoff', () => {
    expect(parseShareLink('vis://share')).toBeNull();
    expect(parseShareLink('vis://share?text=%20%20')).toBeNull();
  });

  it('drops the duplicate link Safari sends as both url and text', () => {
    const link = 'https://example.com/a';
    const raw = `vis://share?url=${encodeURIComponent(link)}&text=${encodeURIComponent(link)}`;
    expect(parseShareLink(raw)).toEqual({ url: link });
  });

  it('drops a title that only repeats the url or the text', () => {
    const link = 'https://example.com/a';
    const raw = `vis://share?url=${encodeURIComponent(link)}&title=${encodeURIComponent(link)}`;
    expect(parseShareLink(raw)).toEqual({ url: link });
  });

  it('trims surrounding whitespace', () => {
    expect(parseShareLink('vis://share?text=%20%20hi%20%20')).toEqual({ text: 'hi' });
  });

  it('clips an oversized body instead of dropping the share', () => {
    const huge = 'x'.repeat(100_001);
    const share = parseShareLink(`vis://share?text=${huge}`);
    expect(share?.text).toHaveLength(100_000);
  });
});

// Files a share sheet handed over. The native side stages a copy and names it
// here; `file`, `name` and `type` are read back BY POSITION, because a share of
// four photos is four of each and only their order pairs them.
describe('shared files', () => {
  it('reads file, name and type index aligned', () => {
    const share = parseShareLink(
      'vis://share?file=%2Ftmp%2Fa%2Fmemo.m4a&name=memo.m4a&type=audio%2Fmp4'
        + '&file=%2Ftmp%2Fb%2Fshot.png&name=shot.png&type=image%2Fpng',
    );

    expect(share?.files).toEqual([
      { path: '/tmp/a/memo.m4a', name: 'memo.m4a', type: 'audio/mp4' },
      { path: '/tmp/b/shot.png', name: 'shot.png', type: 'image/png' },
    ]);
  });

  // Android's document provider often claims nothing at all; the app types the
  // file by its extension later, so a missing type is absent, never guessed here.
  it('keeps a file whose type the platform did not know', () => {
    const share = parseShareLink('vis://share?file=%2Ftmp%2Fa%2Fmemo.m4a&name=memo.m4a');

    expect(share?.files).toEqual([{ path: '/tmp/a/memo.m4a', name: 'memo.m4a' }]);
  });

  it('names a file after its path when the sender gave no name', () => {
    const share = parseShareLink('vis://share?file=file%3A%2F%2F%2Ftmp%2FVoice%2520Memo.m4a');

    expect(share?.files?.[0]?.name).toBe('Voice Memo.m4a');
  });

  // The same staged copy read twice would attach the memo twice.
  it('drops a repeated path and caps the batch', () => {
    const many = Array.from({ length: 12 }, (_, index) => `file=%2Ftmp%2F${index}.png`).join('&');
    const share = parseShareLink(`vis://share?${many}&file=%2Ftmp%2F0.png`);

    expect(share?.files).toHaveLength(8);
    expect(share?.files?.[0]?.path).toBe('/tmp/0.png');
  });

  // A photo shared with no caption is still a share: the old rule ("no words, no
  // share") would have thrown the picture away.
  it('is a share even with no words at all', () => {
    expect(parseShareLink('vis://share?file=%2Ftmp%2Fa.png')).toEqual({
      files: [{ path: '/tmp/a.png', name: 'a.png' }],
    });
  });

  it('summarises a share the way a band can hold it', () => {
    const one = parseShareLink('vis://share?file=%2Ftmp%2Fmemo.m4a');
    const three = parseShareLink('vis://share?file=%2Fa.png&file=%2Fb.png&file=%2Fc.png');
    const link = parseShareLink('vis://share?url=https%3A%2F%2Fexample.com%2Fa');

    expect(shareSummary(one)).toBe('memo.m4a');
    expect(shareSummary(three)).toBe('3 files');
    expect(shareSummary(link)).toBe('https://example.com/a');
    expect(shareSummary(null)).toBe('');
  });

  // Two shares before a composer drains them: words accumulate as words, files as
  // files — a memo cannot be pasted into a sentence.
  it('accumulates files across shares that arrive before a drain', () => {
    receiveSharedText({ text: 'look', files: [{ path: '/tmp/a.png', name: 'a.png' }] });
    receiveSharedText({ files: [{ path: '/tmp/b.png', name: 'b.png' }] });

    const pending = peekPendingShare();
    expect(pending?.files?.map((file) => file.name)).toEqual(['a.png', 'b.png']);
    expect(pending?.text).toBe('look');
  });
});

describe('formatShare / appendSharedText', () => {
  it('puts the title first, then the link on its own line, then the text', () => {
    expect(formatShare({ title: 'T', url: 'https://e.com', text: 'note' })).toBe(
      'T\nhttps://e.com\nnote',
    );
  });

  it('formats an empty share as an empty string', () => {
    expect(formatShare({})).toBe('');
  });

  it('uses the share as-is when the composer is empty or blank', () => {
    expect(appendSharedText('', { text: 'hi' })).toBe('hi');
    expect(appendSharedText('   \n', { text: 'hi' })).toBe('hi');
  });

  it('never overwrites a half-written prompt', () => {
    expect(appendSharedText('draft  \n\n', { url: 'https://e.com' })).toBe('draft\nhttps://e.com');
  });

  it('leaves the composer untouched when there is nothing to add', () => {
    expect(appendSharedText('draft', {})).toBe('draft');
  });
});

describe('pending slot', () => {
  it('holds a share until a composer takes it', () => {
    const received = receiveSharedText({ url: 'https://e.com' });
    expect(received?.url).toBe('https://e.com');
    expect(typeof received?.at).toBe('number');
    expect(peekPendingShare()?.url).toBe('https://e.com');
  });

  it('coalesces shares dumped before a drain', () => {
    receiveSharedText({ url: 'https://one.com' });
    receiveSharedText({ url: 'https://two.com' });
    expect(peekPendingShare()?.text).toBe('https://one.com\nhttps://two.com');
  });

  it('pastes exactly once', () => {
    receiveSharedText({ text: 'hi' });
    expect(takePendingShare()?.text).toBe('hi');
    expect(takePendingShare()).toBeNull();
    expect(peekPendingShare()).toBeNull();
  });

  it('ignores an empty share', () => {
    receiveSharedText({ text: 'kept' });
    expect(receiveSharedText({ text: '   ' })).toBeNull();
    expect(peekPendingShare()?.text).toBe('kept');
  });

  it('delivers to a live listener and stops on unsubscribe', () => {
    const seen: Array<SharedPayload | null> = [];
    const off = onSharedText((share) => seen.push(share));
    receiveSharedText({ text: 'one' });
    off();
    receiveSharedText({ text: 'two' });
    expect(seen).toHaveLength(1);
    expect(seen[0]?.text).toBe('one');
  });

  it('parks the share in native storage and in the localStorage mirror', async () => {
    receiveSharedText({ url: 'https://e.com' });
    await settle();
    expect(JSON.parse(native.store.get(KEY) ?? 'null')).toMatchObject({ url: 'https://e.com' });
    expect(JSON.parse(globalThis.localStorage.getItem(KEY) ?? 'null')).toMatchObject({
      url: 'https://e.com',
    });
  });

  it('clears both copies once taken', async () => {
    receiveSharedText({ url: 'https://e.com' });
    await settle();
    takePendingShare();
    await settle();
    expect(native.store.has(KEY)).toBe(false);
    expect(globalThis.localStorage.getItem(KEY)).toBeNull();
  });
});

describe('hydration across a cold start', () => {
  it('picks up a share parked before this webview existed', async () => {
    native.store.set(KEY, JSON.stringify({ url: 'https://parked.com', at: Date.now() }));
    expect(await hydratePendingShare()).toMatchObject({ url: 'https://parked.com' });
    expect(peekPendingShare()?.url).toBe('https://parked.com');
  });

  it('reads storage once no matter how many callers race the boot', async () => {
    native.store.set(KEY, JSON.stringify({ text: 'parked', at: Date.now() }));
    const [a, b] = await Promise.all([hydratePendingShare(), hydratePendingShare()]);
    expect(a).toBe(b);
    expect(native.getCalls).toBe(1);
    expect(await hydratePendingShare()).toBe(a);
    expect(native.getCalls).toBe(1);
  });

  it('drops a share nobody came back for in a week', async () => {
    native.store.set(KEY, JSON.stringify({ text: 'ancient', at: Date.now() - 8 * DAY_MS }));
    expect(await hydratePendingShare()).toBeNull();
  });

  it('survives corrupt or foreign stored values', async () => {
    for (const raw of ['{not json', '[1,2]', 'null', '{}', '{"text":"   "}']) {
      resetShareIntakeForTests();
      native.store.set(KEY, raw);
      expect(await hydratePendingShare()).toBeNull();
    }
  });

  it('falls back to the localStorage mirror when the native bridge fails', async () => {
    native.onGet = () => Promise.reject(new Error('bridge is wedged'));
    globalThis.localStorage.setItem(KEY, JSON.stringify({ text: 'mirrored', at: Date.now() }));
    expect(await hydratePendingShare()).toMatchObject({ text: 'mirrored' });
  });

  it('RACE: a drop arriving mid-read does not evict the parked share', async () => {
    native.store.set(KEY, JSON.stringify({ text: 'parked', at: Date.now() - 1000 }));
    let release = () => {};
    native.onGet = () => new Promise<void>((resolve) => (release = resolve));

    const hydrating = hydratePendingShare();
    receiveSharedText({ text: 'live' });
    release();
    await hydrating;
    await settle();

    // Older first, live drop after it — neither is allowed to win outright.
    expect(peekPendingShare()?.text).toBe('parked\nlive');
    expect(JSON.parse(native.store.get(KEY) ?? 'null')).toMatchObject({ text: 'parked\nlive' });
  });

  it('RACE: a take before hydration cannot erase the parked share', async () => {
    native.store.set(KEY, JSON.stringify({ text: 'parked', at: Date.now() }));
    expect(takePendingShare()).toBeNull();
    await settle();
    expect(peekPendingShare()?.text).toBe('parked');
    expect(native.store.has(KEY)).toBe(true);
  });

  it('does not resurrect a share that was already taken', async () => {
    native.store.set(KEY, JSON.stringify({ text: 'parked', at: Date.now() }));
    await hydratePendingShare();
    expect(takePendingShare()?.text).toBe('parked');
    await settle();
    expect(await hydratePendingShare()).toBeNull();
    expect(native.store.has(KEY)).toBe(false);
  });
});

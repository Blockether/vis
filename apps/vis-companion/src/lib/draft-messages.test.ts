// The sessions list has to know which empty rows are holding unsent words, and
// it lives outside the composer that writes them. These pin the store's
// notification contract: without it a row typed into stays hidden until the
// next full reload, which is the bug that stranded the words in the first place.

import { beforeEach, describe, expect, it, vi } from 'vitest';

// `vi.mock` factories run at import time, before module-scope `const`s of this
// file exist — the shared state has to be hoisted with them.
const native = vi.hoisted(() => ({ store: new Map<string, string>() }));

vi.mock('@capacitor/preferences', () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({ value: native.store.get(key) ?? null }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
    remove: async ({ key }: { key: string }) => {
      native.store.delete(key);
    },
  },
}));

import {
  clearDraftMessage,
  draftMessageKey,
  hydrateDraftMessages,
  peekDraftMessage,
  subscribe,
  writeDraftMessage,
} from './draft-messages';

const key = draftMessageKey('http://studio.local:7890', 'abc');

describe('draft message subscribers', () => {
  beforeEach(async () => {
    native.store.clear();
    await hydrateDraftMessages();
    clearDraftMessage(key);
  });

  it('notifies on write and on clear, with the new text already readable', async () => {
    const seen: string[] = [];
    const stop = subscribe(() => {
      seen.push(peekDraftMessage(key).text);
    });

    writeDraftMessage(key, { text: 'half a thought' });
    expect(seen).toEqual(['half a thought']);

    clearDraftMessage(key);
    expect(seen).toEqual(['half a thought', '']);

    stop();
    writeDraftMessage(key, { text: 'after unsubscribe' });
    expect(seen).toHaveLength(2);
  });

  it('publishes a NEW store object per change so a snapshot reader re-renders', async () => {
    const snapshots: unknown[] = [];
    const stop = subscribe(() => {
      snapshots.push(peekDraftMessage(key));
    });
    writeDraftMessage(key, { text: 'one' });
    writeDraftMessage(key, { text: 'two' });
    stop();
    expect(snapshots).toHaveLength(2);
    expect(snapshots[0]).not.toBe(snapshots[1]);
  });
});

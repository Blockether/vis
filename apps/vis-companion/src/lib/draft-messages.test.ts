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
  draftMessageHasUnsent,
  draftMessageKey,
  flushDraftMessages,
  hydrateDraftMessages,
  peekDraftMessage,
  subscribe,
  writeDraftMessage,
} from './draft-messages';
import type { PendingAttachment } from './attachments';

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

// An unsent message is not only text. A picture staged in the composer was
// dropped the moment the screen unmounted: the store kept the words, forgot the
// image, and a session holding nothing but that image did not even count as
// unsent work — so its row was hidden as "empty".
describe('draft message attachments', () => {
  const image = (name: string): PendingAttachment => {
    const base64 = `data:image/png;base64,${name}`;
    return {
      id: `id-${name}`,
      filename: `${name}.png`,
      media_type: 'image/png',
      base64,
      previewUrl: base64,
      size: base64.length,
    };
  };

  beforeEach(async () => {
    native.store.clear();
    await hydrateDraftMessages();
    clearDraftMessage(key);
  });

  it('holds an image with no words, and calls it unsent', () => {
    writeDraftMessage(key, { text: '', attachments: [image('shot')] });
    expect(peekDraftMessage(key).attachments).toEqual([image('shot')]);
    expect(draftMessageHasUnsent(peekDraftMessage(key))).toBe(true);
  });

  it('forgets the message once the last word AND the last attachment are gone', () => {
    writeDraftMessage(key, { text: 'look', attachments: [image('shot')] });
    writeDraftMessage(key, { text: '', attachments: [] });
    expect(peekDraftMessage(key).attachments).toEqual([]);
    expect(draftMessageHasUnsent(peekDraftMessage(key))).toBe(false);
  });

  it('survives a restart, preview rebuilt from the stored data URL', async () => {
    writeDraftMessage(key, { text: 'look at this', attachments: [image('shot')] });
    await flushDraftMessages();

    vi.resetModules();
    const reloaded = await import('./draft-messages');
    const message = await reloaded.readDraftMessage(key);
    expect(message.text).toBe('look at this');
    expect(message.attachments).toEqual([image('shot')]);
  });
});

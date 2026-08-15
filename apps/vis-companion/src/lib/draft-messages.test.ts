// The sessions list has to know which empty rows are holding unsent words, and
// it lives outside the composer that writes them. These pin the store's
// notification contract: without it a row typed into stays hidden until the
// next full reload, which is the bug that stranded the words in the first place.

import { beforeEach, describe, expect, it, vi } from 'vitest';

// `vi.mock` factories run at import time, before module-scope `const`s of this
// file exist — the shared state has to be hoisted with them.
const native = vi.hoisted(() => ({
  store: new Map<string, string>(),
  /** Every native write, in order: which key it landed on and what it carried. */
  writes: [] as { key: string; value: string }[],
}));

vi.mock('@capacitor/preferences', () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({ value: native.store.get(key) ?? null }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
      native.writes.push({ key, value });
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
  type DraftMessage,
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

  // Regression, reported from the phone ("writing in the input of the companion
  // app hangs for half a second, many times, on iOS"): every keystroke woke the
  // sessions list, which stays mounted behind the open transcript, and it re-ran
  // its fleet-wide filter and sort per character for a screen nobody can see.
  // A reader outside the composer learns that a row turned dirty, and learns the
  // words when they are persisted — never once per character in between.
  it('wakes a reader when the row turns dirty and when the words land, not per keystroke', async () => {
    const snapshots: DraftMessage[] = [];
    const stop = subscribe(() => {
      snapshots.push(peekDraftMessage(key));
    });
    writeDraftMessage(key, { text: 'o' });
    writeDraftMessage(key, { text: 'on' });
    writeDraftMessage(key, { text: 'one' });
    expect(snapshots).toHaveLength(1);

    await flushDraftMessages();
    stop();
    expect(snapshots).toHaveLength(2);
    // Each wake carries a NEW object, or a memoised row would not repaint.
    expect(snapshots[0]).not.toBe(snapshots[1]);
    expect(snapshots[1].text).toBe('one');
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

// Regression: typing beside a staged photo re-wrote the photo. Every keystroke
// marked the store dirty, and the flush behind it serialized the whole base64
// back into localStorage AND pushed the same megabytes across the native bridge
// into UserDefaults — so writing a sentence next to an attached image stalled
// the iOS composer for seconds at every pause in typing, to store a payload
// identical to the one already on disk.
describe('draft message bytes', () => {
  const photo: PendingAttachment = (() => {
    const base64 = `data:image/jpeg;base64,${'A'.repeat(4096)}`;
    return {
      id: 'id-photo',
      filename: 'IMG_0421.jpeg',
      media_type: 'image/jpeg',
      base64,
      previewUrl: base64,
      size: 3072,
    };
  })();

  const written = () => native.writes.map((write) => write.key);

  beforeEach(async () => {
    native.store.clear();
    await hydrateDraftMessages();
    clearDraftMessage(key);
    await flushDraftMessages();
    native.store.clear();
    native.writes.length = 0;
  });

  it('writes the bytes once, and not again while only the words change', async () => {
    writeDraftMessage(key, { text: 'look at', attachments: [photo] });
    await flushDraftMessages();
    expect(written()).toContain('vis.draftAttachments');

    native.writes.length = 0;
    for (const text of ['look at t', 'look at th', 'look at this']) {
      writeDraftMessage(key, { text, attachments: [photo] });
      await flushDraftMessages();
    }
    expect(written()).toEqual(['vis.draftMessages', 'vis.draftMessages', 'vis.draftMessages']);
    expect(native.writes.some((write) => write.value.includes(photo.base64))).toBe(false);
  });

  it('writes them again the moment the staged set itself changes', async () => {
    writeDraftMessage(key, { text: 'look at this', attachments: [photo] });
    await flushDraftMessages();
    native.writes.length = 0;

    writeDraftMessage(key, { text: 'look at this', attachments: [] });
    await flushDraftMessages();
    expect(written()).toContain('vis.draftAttachments');
    expect(peekDraftMessage(key).attachments).toEqual([]);
  });
});

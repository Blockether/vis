// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import type { SnapshotStores } from './snapshot-store';

const indexKey = 'vis.snapshots.v2';
const key = (kind: string, id: string) => `http://gateway.example.com\u0000${kind}\u0000${id}`;
const item = (name: string) => `${indexKey}:${JSON.stringify(name)}`;
const stores = (): SnapshotStores => ({
  snapshots: new Map(),
  stamps: new Map(),
  windows: new Map(),
});
// What a cold start can read: each item that the index lists.
const persisted = () => {
  const index = JSON.parse(localStorage.getItem(indexKey)!) as { keys: string[] };
  const held = {
    snapshots: {} as Record<string, any>,
    stamps: {} as Record<string, unknown>,
    windows: {} as Record<string, unknown>,
  };
  for (const name of index.keys) {
    const body = JSON.parse(localStorage.getItem(item(name))!);
    held.snapshots[name] = body.value;
    if ('stamp' in body) held.stamps[name] = body.stamp;
    if ('window' in body) held.windows[name] = body.window;
  }
  return held;
};
// The characters that the cache keeps in storage: keys and values of the index and items.
const footprint = () => {
  let length = 0;
  for (let index = 0; index < localStorage.length; index += 1) {
    const name = localStorage.key(index)!;
    if (name.startsWith(indexKey)) length += name.length + localStorage.getItem(name)!.length;
  }
  return length;
};

beforeEach(() => {
  localStorage.clear();
  vi.resetModules();
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  vi.restoreAllMocks();
});

// The iOS diagnostic report includes a 346 ms input frame in a nearly empty chat.
// Its session poll still flushed every other session's cached tool output. The
// oversized-cache loop repeatedly encoded that output before dropping old rows.
describe('snapshot persistence during chat', () => {
  it('encodes a transcript at most once when applying the storage budget', async () => {
    const { flushSnapshots } = await import('./snapshot-store');
    const cache = stores();
    const reads = Array.from({ length: 10 }, () => vi.fn(() => 'x'.repeat(220_000)));
    reads.forEach((answer, index) => {
      const id = key('transcript', String(index));
      cache.snapshots.set(id, [
        {
          id: `turn-${index}`,
          get answer() {
            return answer();
          },
        },
      ]);
      cache.stamps.set(id, `stamp-${index}`);
      cache.windows.set(id, { offset: 0, total: 1 });
    });
    flushSnapshots(cache);

    expect(footprint()).toBeLessThanOrEqual(1_000_000);
    expect(Object.keys(persisted().snapshots)).toEqual(
      [6, 7, 8, 9].map((index) => key('transcript', String(index))),
    );
    expect(Object.keys(persisted().stamps)).toEqual(Object.keys(persisted().snapshots));
    expect(Object.keys(persisted().windows)).toEqual(Object.keys(persisted().snapshots));
    expect(Math.max(...reads.map((read) => read.mock.calls.length))).toBeLessThanOrEqual(1);
    expect(reads.slice(0, 5).every((read) => read.mock.calls.length === 0)).toBe(true);
    expect(cache.snapshots.size).toBe(10);
  });

  it('does not traverse unchanged history when an empty session is polled', async () => {
    const { flushSnapshots, scheduleSnapshotFlush } = await import('./snapshot-store');
    const cache = stores();
    const answer = vi.fn(() => 'Saved tool output '.repeat(20_000));
    const transcript = key('transcript', 'previous');
    cache.snapshots.set(transcript, [
      {
        get answer() {
          return answer();
        },
      },
    ]);
    flushSnapshots(cache);
    answer.mockClear();

    for (let poll = 0; poll < 3; poll += 1) {
      cache.snapshots.set(key('session', 'empty'), {
        id: 'empty',
        turn_count: 0,
        server_time_ms: poll,
      });
      scheduleSnapshotFlush(cache);
      await vi.advanceTimersByTimeAsync(30_000);
    }

    expect(persisted().snapshots[key('session', 'empty')].server_time_ms).toBe(2);
    expect(persisted().snapshots[transcript]).toHaveLength(1);
    expect(answer).not.toHaveBeenCalled();
  });

  it('does not repeatedly encode a transcript too large to persist', async () => {
    const { flushSnapshots } = await import('./snapshot-store');
    const cache = stores();
    const answer = vi.fn(() => 'x'.repeat(1_000_001));
    cache.snapshots.set(key('transcript', 'large'), [
      {
        get answer() {
          return answer();
        },
      },
    ]);
    cache.snapshots.set(key('session', 'empty'), { id: 'empty' });
    flushSnapshots(cache);
    answer.mockClear();
    flushSnapshots(cache);

    expect(Object.keys(persisted().snapshots)).toEqual([key('session', 'empty')]);
    expect(answer).not.toHaveBeenCalled();
  });

  it('restores trimmed windows, replacements and current stamps without changing live rows', async () => {
    const { flushSnapshots, hydrateSnapshots } = await import('./snapshot-store');
    const cache = stores();
    const transcript = key('transcript', 'history');
    const rows = Array.from({ length: 12 }, (_, index) => ({ id: `t${index}`, answer: 'Before' }));
    cache.snapshots.set(transcript, rows);
    cache.windows.set(transcript, { offset: 20, total: 32 });
    cache.stamps.set(transcript, 'first');
    flushSnapshots(cache);
    expect(persisted().snapshots[transcript]).toEqual(rows.slice(-8));
    expect(persisted().windows[transcript]).toEqual({ offset: 24, total: 32 });

    const updated = rows.map((row) =>
      row.id === 't11' ? { ...row, answer: 'After \"quoted\" \n żółć' } : row,
    );
    cache.snapshots.set(transcript, updated);
    cache.stamps.set(transcript, 'second');
    flushSnapshots(cache);
    const cold = stores();
    hydrateSnapshots(cold);
    expect(cold.snapshots.get(transcript)).toEqual(updated.slice(-8));
    expect(cold.stamps.get(transcript)).toBe('second');
    expect(rows).toHaveLength(12);

    // Window and stamp metadata can change even when the row identities do not.
    cache.windows.set(transcript, { offset: 21, total: 33 });
    cache.stamps.set(transcript, 'third');
    flushSnapshots(cache);
    expect(persisted().windows[transcript]).toEqual({ offset: 25, total: 33 });
    expect(persisted().stamps[transcript]).toBe('third');
    cache.snapshots.delete(transcript);
    flushSnapshots(cache);
    expect(persisted()).toEqual({ snapshots: {}, stamps: {}, windows: {} });
    expect(Array.from({ length: localStorage.length }, (_, index) => localStorage.key(index))).not.toContain(
      item(transcript),
    );
  });

  it('keeps LRU order across a cold start and honours later reads', async () => {
    const { flushSnapshots, hydrateSnapshots } = await import('./snapshot-store');
    const cache = stores();
    for (const id of ['first', 'second', 'third']) {
      cache.snapshots.set(key('transcript', id), [{ answer: 'x'.repeat(400_000) }]);
    }
    flushSnapshots(cache);
    const cold = stores();
    hydrateSnapshots(cold);
    expect([...cold.snapshots.keys()]).toEqual(
      ['second', 'third'].map((id) => key('transcript', id)),
    );

    const first = key('transcript', 'first');
    const value = cache.snapshots.get(first);
    cache.snapshots.delete(first);
    cache.snapshots.set(first, value);
    flushSnapshots(cache);
    expect(Object.keys(persisted().snapshots)).toEqual(
      ['third', 'first'].map((id) => key('transcript', id)),
    );
  });

  it('coalesces writes and flushes the pending cache on hide', async () => {
    const { scheduleSnapshotFlush, installSnapshotFlushOnHide } = await import('./snapshot-store');
    const cache = stores();
    cache.snapshots.set(key('session', 'empty'), { id: 'empty' });
    cache.snapshots.set(key('running-turn', 'empty'), { answer: 'Transient stream' });
    const writes = vi.spyOn(localStorage, 'setItem');
    installSnapshotFlushOnHide(cache);
    scheduleSnapshotFlush(cache);
    scheduleSnapshotFlush(cache);
    expect(writes).not.toHaveBeenCalled();
    window.dispatchEvent(new Event('pagehide'));
    expect(writes.mock.calls.map(([name]) => name)).toEqual([item(key('session', 'empty')), indexKey]);
    expect(Object.keys(persisted().snapshots)).toEqual([key('session', 'empty')]);
    await vi.advanceTimersByTimeAsync(400);
    expect(writes).toHaveBeenCalledTimes(2);
  });

  it.each(['plain', 'quoted"\\\nżółć'])(
    'accounts for keys, separators and metadata at the exact budget for %s',
    async (id) => {
      const { flushSnapshots } = await import('./snapshot-store');
      const cache = stores();
      const transcript = key('transcript', id);
      cache.snapshots.set(key('session', 'empty'), { id: 'empty' });
      cache.snapshots.set(transcript, [{ answer: '' }]);
      cache.stamps.set(transcript, 'stamp');
      cache.windows.set(transcript, { offset: 0, total: 1 });
      flushSnapshots(cache);
      const room = 1_000_000 - footprint();
      cache.snapshots.set(transcript, [{ answer: 'x'.repeat(room) }]);
      flushSnapshots(cache);
      expect(footprint()).toBe(1_000_000);
      expect(persisted().snapshots[transcript][0].answer).toHaveLength(room);

      cache.snapshots.set(transcript, [{ answer: 'x'.repeat(room + 1) }]);
      flushSnapshots(cache);
      expect(persisted().snapshots[transcript]).toBeUndefined();
      expect(persisted().stamps[transcript]).toBeUndefined();
      expect(persisted().windows[transcript]).toBeUndefined();
    },
  );

  it('preserves the small snapshots if storage rejects history', async () => {
    const { flushSnapshots } = await import('./snapshot-store');
    const cache = stores();
    cache.snapshots.set(key('transcript', 'history'), [{ answer: 'Remembered history' }]);
    cache.snapshots.set(key('session', 'empty'), { id: 'empty' });
    const writes = vi.spyOn(localStorage, 'setItem').mockImplementationOnce(() => {
      throw new DOMException('Full', 'QuotaExceededError');
    });
    flushSnapshots(cache);
    // The refused row is written again after the history gave its room, then the index.
    expect(writes).toHaveBeenCalledTimes(3);
    expect(Object.keys(persisted().snapshots)).toEqual([key('session', 'empty')]);
  });
});

// Reported in this Vis session: the WebKit storage log of the desktop app grew by about
// 3 MB each minute (2.5 GB in an 11-hour run). Each poll wrote all of the cache again
// (1.76 MB), also when no row changed or when only the LRU order moved.
describe('snapshot writes during polls', () => {
  it('writes only the snapshots that changed', async () => {
    const { flushSnapshots } = await import('./snapshot-store');
    const cache = stores();
    cache.snapshots.set(key('transcript', 'history'), [{ answer: 'x'.repeat(200_000) }]);
    cache.snapshots.set(key('sessions', 'head'), [{ id: 'one' }, { id: 'two' }]);
    cache.snapshots.set(key('session', 'one'), { id: 'one', server_time_ms: 0 });
    flushSnapshots(cache);
    const writes = vi.spyOn(localStorage, 'setItem');
    const written = () =>
      writes.mock.calls.reduce((length, [name, value]) => length + name.length + value.length, 0);

    // A read moves an entry in the LRU order, and a poll can save an equal row again.
    const rows = cache.snapshots.get(key('sessions', 'head'));
    cache.snapshots.delete(key('sessions', 'head'));
    cache.snapshots.set(key('sessions', 'head'), rows);
    cache.snapshots.set(key('session', 'one'), { id: 'one', server_time_ms: 0 });
    flushSnapshots(cache);
    expect(written()).toBeLessThan(1_000);

    writes.mockClear();
    cache.snapshots.set(key('session', 'one'), { id: 'one', server_time_ms: 1 });
    flushSnapshots(cache);
    expect(written()).toBeLessThan(1_000);
    expect(persisted().snapshots[key('session', 'one')].server_time_ms).toBe(1);
  });

  it('writes changed rows at most once in 30 seconds', async () => {
    const { flushSnapshots, scheduleSnapshotFlush } = await import('./snapshot-store');
    const cache = stores();
    const writes = vi.spyOn(localStorage, 'setItem');
    // A poll every 6 seconds for 90 seconds, and each poll changes the row.
    for (let poll = 0; poll < 15; poll += 1) {
      cache.snapshots.set(key('session', 'busy'), { id: 'busy', server_time_ms: poll });
      scheduleSnapshotFlush(cache);
      await vi.advanceTimersByTimeAsync(6_000);
    }
    // The first change at once, then one change in each 30 seconds: three writes of the
    // row and one of the index. Before, each poll wrote all of the cache.
    expect(writes).toHaveBeenCalledTimes(4);
    // A hide writes the last change at once.
    flushSnapshots(cache);
    expect(persisted().snapshots[key('session', 'busy')].server_time_ms).toBe(14);
  });

  it('moves the cache of an earlier version into items and removes it', async () => {
    const transcript = key('transcript', 'old');
    localStorage.setItem(
      'vis.snapshots.v1',
      JSON.stringify({
        v: 1,
        snapshots: { [key('session', 'old')]: { id: 'old' }, [transcript]: [{ answer: 'Kept' }] },
        stamps: { [transcript]: 'stamp' },
        windows: { [transcript]: { offset: 4, total: 12 } },
      }),
    );
    const { flushSnapshots, hydrateSnapshots } = await import('./snapshot-store');
    const cache = stores();
    hydrateSnapshots(cache);
    expect(cache.windows.get(transcript)).toEqual({ offset: 4, total: 12 });
    flushSnapshots(cache);

    expect(localStorage.getItem('vis.snapshots.v1')).toBeNull();
    expect(persisted()).toEqual({
      snapshots: { [key('session', 'old')]: { id: 'old' }, [transcript]: [{ answer: 'Kept' }] },
      stamps: { [transcript]: 'stamp' },
      windows: { [transcript]: { offset: 4, total: 12 } },
    });

    // The earlier version can run again and write its item. Then that item is the newest copy.
    const newer = { id: 'old', title: 'Newer' };
    localStorage.setItem(
      'vis.snapshots.v1',
      JSON.stringify({ v: 1, snapshots: { [key('session', 'old')]: newer }, stamps: {}, windows: {} }),
    );
    vi.resetModules();
    const again = await import('./snapshot-store');
    const cold = stores();
    again.hydrateSnapshots(cold);
    expect(cold.snapshots.get(key('session', 'old'))).toEqual(newer);
    again.flushSnapshots(cold);
    expect(localStorage.getItem('vis.snapshots.v1')).toBeNull();
    expect(persisted()).toEqual({ snapshots: { [key('session', 'old')]: newer }, stamps: {}, windows: {} });
  });
});

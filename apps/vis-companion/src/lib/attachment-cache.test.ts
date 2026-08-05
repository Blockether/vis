import { afterEach, describe, expect, it } from 'vitest';
import {
  ATTACHMENT_DISK_BUDGET,
  attachmentCacheStats,
  cacheVictims,
  clearAttachmentCache,
  readCachedAttachment,
  writeCachedAttachment,
  type CacheBudget,
} from './attachment-cache';
import {
  installFakeCacheStorage as installCache,
  uninstallFakeCacheStorage,
} from './attachment-cache.fixture';

afterEach(uninstallFakeCacheStorage);

const bytes = (size: number, fill = 7) => new Blob([new Uint8Array(size).fill(fill)]);

const small: CacheBudget = { maxBytes: 1000, maxEntries: 3, maxEntryBytes: 600 };

describe('cacheVictims', () => {
  const entry = (url: string, size: number, used: number, pinned?: boolean) => ({
    url,
    bytes: size,
    used,
    pinned,
  });

  it('keeps everything while both bounds hold', () => {
    expect(
      cacheVictims([entry('a', 10, 1), entry('b', 10, 2)], small),
    ).toEqual([]);
  });

  it('evicts least recently used first, until the BYTE bound holds', () => {
    const victims = cacheVictims(
      [entry('a', 400, 3), entry('b', 400, 1), entry('c', 400, 2)],
      small,
    );
    expect(victims).toEqual(['b']);
  });

  it('evicts on the COUNT bound even when every artifact is tiny', () => {
    // Four thumbnails weigh nothing at all: only the number is over.
    const victims = cacheVictims(
      [entry('a', 1, 4), entry('b', 1, 1), entry('c', 1, 2), entry('d', 1, 3)],
      small,
    );
    expect(victims).toEqual(['b']);
  });

  it('skips what is on screen rather than deferring it', () => {
    // `b` is the oldest AND the biggest — and it is being painted right now, so
    // revoking its URL would break a live <img>. The next-oldest goes instead.
    const victims = cacheVictims(
      [entry('a', 500, 3), entry('b', 500, 1, true), entry('c', 500, 2)],
      small,
    );
    expect(victims).toEqual(['c']);
  });

  it('can evict nothing at all when every entry is pinned', () => {
    const victims = cacheVictims(
      [entry('a', 900, 1, true), entry('b', 900, 2, true)],
      small,
    );
    expect(victims).toEqual([]);
  });

  it('treats an entry from a previous run as the oldest thing there is', () => {
    const victims = cacheVictims(
      [entry('a', 400, 0), entry('b', 400, 5), entry('stale', 400, -1)],
      small,
    );
    expect(victims).toEqual(['stale']);
  });
});

describe('the persistent attachment cache', () => {
  it('hands back the very bytes it was given, without a network in sight', async () => {
    installCache();
    const url = 'https://gw.example.com/v1/sessions/s1/iterations/i1/attachments/0';
    await writeCachedAttachment(url, bytes(2048, 3));

    const hit = await readCachedAttachment(url);
    expect(hit).not.toBeNull();
    expect(hit?.size).toBe(2048);
    expect(new Uint8Array(await hit!.arrayBuffer())[2047]).toBe(3);
  });

  it('misses on an artifact this device has never seen', async () => {
    installCache();
    expect(await readCachedAttachment('https://gw.example.com/never')).toBeNull();
  });

  it('refuses one artifact bigger than the per-entry ceiling', async () => {
    // A 200 MB clip must not evict a whole session of figures on its way past.
    const cache = installCache();
    await writeCachedAttachment('https://gw.example.com/huge', bytes(900), small);
    expect(cache.store.size).toBe(0);
  });

  it('stays inside its budget, dropping the least recently used', async () => {
    const cache = installCache();
    const url = (n: number) => `https://gw.example.com/page/${n}`;
    for (const n of [1, 2, 3]) await writeCachedAttachment(url(n), bytes(300), small);
    // Reading is USING: it saves the oldest entry from the next eviction.
    await readCachedAttachment(url(1));
    await writeCachedAttachment(url(4), bytes(300), small);

    expect([...cache.store.keys()].sort()).toEqual([url(1), url(3), url(4)]);
  });

  it('reports what the device is holding, and forgets it on demand', async () => {
    installCache();
    await writeCachedAttachment('https://gw.example.com/a', bytes(1024));
    await writeCachedAttachment('https://gw.example.com/b', bytes(2048));
    expect(await attachmentCacheStats()).toEqual({ entries: 2, bytes: 3072 });

    await clearAttachmentCache();
    expect(await attachmentCacheStats()).toEqual({ entries: 0, bytes: 0 });
  });

  it('degrades to a plain miss where there is no store at all', async () => {
    // A private window, an old webview, SSR: fetching again is the fallback,
    // and nothing here may throw on the way to it.
    expect(await readCachedAttachment('https://gw.example.com/a')).toBeNull();
    await expect(
      writeCachedAttachment('https://gw.example.com/a', bytes(16)),
    ).resolves.toBeUndefined();
    expect(await attachmentCacheStats()).toEqual({ entries: 0, bytes: 0 });
  });

  it('survives a store that throws on every call', async () => {
    (globalThis as { caches?: unknown }).caches = {
      open: async () => {
        throw new Error('QuotaExceededError');
      },
      delete: async () => {
        throw new Error('nope');
      },
    };
    expect(await readCachedAttachment('https://gw.example.com/a')).toBeNull();
    await expect(
      writeCachedAttachment('https://gw.example.com/a', bytes(16)),
    ).resolves.toBeUndefined();
    await expect(clearAttachmentCache()).resolves.toBeUndefined();
  });

  it('budgets a phone, not a server', () => {
    expect(ATTACHMENT_DISK_BUDGET.maxBytes).toBe(96 * 1024 * 1024);
    expect(ATTACHMENT_DISK_BUDGET.maxEntries).toBe(256);
    expect(ATTACHMENT_DISK_BUDGET.maxEntryBytes).toBeLessThan(
      ATTACHMENT_DISK_BUDGET.maxBytes,
    );
  });
});

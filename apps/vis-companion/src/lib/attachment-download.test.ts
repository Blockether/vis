// A produced picture is IMMUTABLE and downloading it is the expensive part, but
// the app kept its artifacts in a bounded map of object URLs in memory alone: a
// re-entered session, a re-opened app, or simply scrolling past a figure and
// back re-fetched the very same bytes over the phone's connection. These pin the
// three tiers of `GatewayClient.attachmentUrl` — this document's object URL, the
// bytes this DEVICE has, then and only then the gateway — so an artifact the
// device has already seen is never downloaded twice.
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import type { GatewayConn } from './types';
import { clearAttachmentCache } from './attachment-cache';
import {
  installFakeCacheStorage,
  installFakeObjectUrls,
  uninstallFakeCacheStorage,
  type FakeObjectUrls,
} from './attachment-cache.fixture';

// Same reason as `gateway-providers.test.ts`: `gateway.ts` arms timers and reads
// storage at import time, so the browser shims exist BEFORE the dynamic import.
const memory = new Map<string, string>();
Object.defineProperty(globalThis, 'localStorage', {
  configurable: true,
  value: {
    getItem: (key: string) => memory.get(key) ?? null,
    setItem: (key: string, value: string) => void memory.set(key, String(value)),
    removeItem: (key: string) => void memory.delete(key),
    clear: () => memory.clear(),
    key: () => null,
    length: 0,
  },
});
(globalThis as { window?: unknown }).window ??= globalThis;

const { GatewayClient } = await import('./gateway');

const CONN = {
  id: 'gw',
  name: 'workstation',
  url: 'http://gateway.example.com:7777',
  token: 'secret',
} as unknown as GatewayConn;

/** Every request the gateway was actually asked for, in order. */
let asked: string[] = [];
let auth: string[] = [];
let urls: FakeObjectUrls;

/** A gateway that always has the artifact, and counts who asks. */
function serve(size = 1024, status = 200): void {
  globalThis.fetch = (async (input: RequestInfo | URL, init?: RequestInit) => {
    asked.push(String(input));
    auth.push(String(new Headers(init?.headers).get('authorization')));
    return new Response(new Uint8Array(size).fill(9), {
      status,
      headers: { 'Content-Type': 'image/png' },
    });
  }) as typeof fetch;
}

beforeEach(async () => {
  installFakeCacheStorage();
  urls = installFakeObjectUrls();
  await clearAttachmentCache();
  asked = [];
  auth = [];
  serve();
});

afterEach(uninstallFakeCacheStorage);

describe('downloading one produced artifact', () => {
  it('asks the endpoint the descriptors index, with the bearer it demands', async () => {
    const client = new GatewayClient(CONN);
    await client.attachmentUrl('s 1', 'i/1', 3);

    expect(asked).toEqual([
      'http://gateway.example.com:7777/v1/sessions/s%201/iterations/i%2F1/attachments/3',
    ]);
    expect(auth).toEqual(['Bearer secret']);
    expect(client.attachmentEndpoint('s 1', 'i/1', 3)).toBe(asked[0]);
  });

  it('downloads it ONCE per document, however many tiles ask', async () => {
    const client = new GatewayClient(CONN);
    const [first, second] = await Promise.all([
      client.attachmentUrl('s1', 'i1', 0),
      client.attachmentUrl('s1', 'i1', 0),
    ]);
    const third = await client.attachmentUrl('s1', 'i1', 0);

    expect(asked).toHaveLength(1);
    expect(second).toBe(first);
    expect(third).toBe(first);
  });
  it('hands text readers the retained Blob without another request', async () => {
    const client = new GatewayClient(CONN);
    const [url, blob] = await Promise.all([
      client.attachmentUrl('s1', 'i1', 0),
      client.attachmentBlob('s1', 'i1', 0),
    ]);

    expect(asked).toHaveLength(1);
    expect(blob.size).toBe(1024);
    expect(urls.sizeOf(url)).toBe(blob.size);
  });
  it('never downloads it again on a device that already has the bytes', async () => {
    // The second client is the app re-opened, or the session re-entered: a new
    // document, an empty memory tier, the SAME immutable artifact.
    await new GatewayClient(CONN).attachmentUrl('s1', 'i1', 0);
    expect(asked).toHaveLength(1);

    const revisit = await new GatewayClient(CONN).attachmentUrl('s1', 'i1', 0);
    expect(asked).toHaveLength(1);
    // Same bytes, freshly wrapped for this document.
    expect(urls.sizeOf(revisit)).toBe(1024);
  });

  it('downloads only the artifacts this device has NOT seen', async () => {
    const client = new GatewayClient(CONN);
    for (const index of [0, 1, 2]) await client.attachmentUrl('s1', 'i1', index);
    expect(asked).toHaveLength(3);

    const later = new GatewayClient(CONN);
    for (const index of [0, 1, 2, 3]) await later.attachmentUrl('s1', 'i1', index);

    expect(asked).toHaveLength(4);
    expect(asked[3]).toBe(client.attachmentEndpoint('s1', 'i1', 3));
  });

  it('does not remember a failure as an answer', async () => {
    serve(0, 503);
    const client = new GatewayClient(CONN);
    await expect(client.attachmentUrl('s1', 'i1', 0)).rejects.toThrow('503');

    serve(2048);
    expect(urls.sizeOf(await client.attachmentUrl('s1', 'i1', 0))).toBe(2048);
    expect(asked).toHaveLength(2);
  });

  it('waits for bytes whose descriptor arrived before its durable row', async () => {
    let responses = 0;
    globalThis.fetch = (async () => {
      responses += 1;
      return responses === 1
        ? new Response(null, { status: 404 })
        : new Response(new Uint8Array(14791), {
            status: 200,
            headers: { 'Content-Type': 'image/png' },
          });
    }) as typeof fetch;

    const client = new GatewayClient(CONN);
    await expect(client.attachmentUrl('s1', 'landing', 0)).resolves.toBeTruthy();
    expect(responses).toBe(2);
  });

  it('paints even when the device refuses to keep anything', async () => {
    // Private window, quota exceeded, no `caches` at all: the picture still
    // arrives, it is simply fetched again next time.
    uninstallFakeCacheStorage();
    const client = new GatewayClient(CONN);
    expect(urls.sizeOf(await client.attachmentUrl('s1', 'i1', 0))).toBe(1024);
    expect(await new GatewayClient(CONN).attachmentUrl('s1', 'i1', 0)).toBeTruthy();
    expect(asked).toHaveLength(2);
  });
});

describe('the memory tier', () => {
  it('lets go of the oldest object URLs instead of growing without end', async () => {
    // Every live entry pins fully DECODED bytes for the lifetime of the
    // document — the exact memory curve iOS answers by killing the webview.
    const client = new GatewayClient(CONN);
    for (let index = 0; index < 30; index += 1) {
      await client.attachmentUrl('s1', 'i1', index);
    }

    expect(urls.made).toHaveLength(30);
    expect(urls.revoked).toEqual(urls.made.slice(0, 6));
  });

  it('costs a revoked picture a decode, never another download', async () => {
    const client = new GatewayClient(CONN);
    for (let index = 0; index < 30; index += 1) {
      await client.attachmentUrl('s1', 'i1', index);
    }
    const before = asked.length;

    // Scrolling back to the first figure: its object URL is long gone, but the
    // bytes are still on this device.
    expect(urls.sizeOf(await client.attachmentUrl('s1', 'i1', 0))).toBe(1024);
    expect(asked).toHaveLength(before);
  });
});

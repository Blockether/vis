// An HTML export has no gateway, so its artifact tiles showed a broken image where the
// session screen shows the picture. These pin that the web app's OWN client, pointed at
// `SNAPSHOT_GATEWAY`, reads each stored artifact from the export, answers an artifact the
// export does not hold as a gateway would (404), and leaves every other request alone.
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { clearAttachmentCache } from './attachment-cache';
import { installFakeCacheStorage, installFakeObjectUrls, uninstallFakeCacheStorage } from './attachment-cache.fixture';
import { parseTranscriptExport, SNAPSHOT_GATEWAY, snapshotFetch, type TranscriptExport } from './transcript-export';

// Same reason as `attachment-download.test.ts`: `gateway.ts` reads storage at import time.
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

const BYTES = [137, 80, 78, 71, 1, 2, 3];

const DATA: TranscriptExport = parseTranscriptExport(
  JSON.stringify({
    session: { id: 's 1' },
    turns: [],
    attachments: { 'i/1/3': { media_type: 'image/png', base64: btoa(String.fromCharCode(...BYTES)) } },
  }),
);

/** Every request that left the export page. */
let passed: string[] = [];

beforeEach(async () => {
  installFakeCacheStorage();
  installFakeObjectUrls();
  await clearAttachmentCache();
  passed = [];
  const network = (async (input: RequestInfo | URL) => {
    passed.push(String(input));
    return new Response('network');
  }) as typeof fetch;
  globalThis.fetch = snapshotFetch(DATA, network);
});

afterEach(uninstallFakeCacheStorage);

describe('an export answers the artifact requests of its page', () => {
  it('gives the client the stored bytes of an artifact that a step names', async () => {
    const client = new GatewayClient({ url: SNAPSHOT_GATEWAY });
    const blob = await client.attachmentBlob('s 1', 'i/1', 3);

    expect([...new Uint8Array(await blob.arrayBuffer())]).toEqual(BYTES);
    expect(blob.type).toBe('image/png');
    expect(passed).toEqual([]);
  });

  it('answers an artifact that the export does not hold with 404', async () => {
    const client = new GatewayClient({ url: SNAPSHOT_GATEWAY });
    const answer = await fetch(client.attachmentEndpoint('s 1', 'i/1', 4));

    expect(answer.status).toBe(404);
    expect(passed).toEqual([]);
  });

  it('passes a request to another address to the network', async () => {
    await fetch('https://example.com/font.woff2');

    expect(passed).toEqual(['https://example.com/font.woff2']);
  });
});

import { afterEach, describe, expect, it, vi } from 'vitest';
import { asc } from './asc.mjs';

// `asc` is the ONE place that holds a socket open to App Store Connect, so the only thing
// worth stubbing is the transport: every release path inherits whatever policy lives here.
const respond = (entry) => {
  if (entry instanceof Error) throw entry;
  return {
    ok: entry.status >= 200 && entry.status < 300,
    status: entry.status,
    statusText: entry.statusText ?? '',
    headers: new Headers(entry.headers ?? {}),
    text: async () => (typeof entry.body === 'string' ? entry.body : JSON.stringify(entry.body ?? {})),
  };
};

/** Queue one outcome per attempt: an Error is a transport failure, anything else a response. */
const transport = (...queue) => {
  const seen = [];
  globalThis.fetch = vi.fn(async (url, init) => {
    seen.push({ url, authorization: init.headers.Authorization, method: init.method, body: init.body });
    if (!queue.length) throw new Error(`unexpected extra request to ${url}`);
    return respond(queue.shift());
  });
  return seen;
};

const ok = (body = { data: { id: 'ok' } }) => ({ status: 200, body });
const waits = [];
const call = (mint, method = 'GET', path = '/v1/apps/42', body) =>
  asc(mint, method, path, body, { wait: async (ms) => void waits.push(ms) });

const realFetch = globalThis.fetch;
afterEach(() => {
  globalThis.fetch = realFetch;
  waits.length = 0;
  vi.restoreAllMocks();
});

describe('asc', () => {
  it('returns the parsed body and asks for a token once when Apple answers', async () => {
    const seen = transport(ok({ data: [{ id: 'app' }] }));
    const mint = vi.fn(() => 'tok');

    await expect(call(mint)).resolves.toEqual({ data: [{ id: 'app' }] });

    expect(mint).toHaveBeenCalledTimes(1);
    expect(seen[0].url).toBe('https://api.appstoreconnect.apple.com/v1/apps/42');
    expect(seen[0].authorization).toBe('Bearer tok');
    expect(waits).toEqual([]);
  });

  // Regression, session 0ec1e9f3-23d5-4070-a17e-46f8e7f514e8: one `TypeError: fetch failed`
  // — undici's wrapper around a dropped socket — threw straight out of `asc`, so the
  // TestFlight "What to Test" text of an already-uploaded build was simply never published
  // and the release reported `notes not published: fetch failed`.
  it('survives a dropped connection', async () => {
    const dropped = Object.assign(new TypeError('fetch failed'), { cause: { code: 'ECONNRESET' } });
    const seen = transport(dropped, ok());
    const mint = vi.fn(() => 'tok');

    await expect(call(mint)).resolves.toEqual({ data: { id: 'ok' } });

    expect(seen).toHaveLength(2);
    expect(waits).toEqual([1_000]);
  });

  it('replays a 401 with a FRESHLY minted token', async () => {
    const seen = transport({ status: 401, body: { errors: [{ title: 'Unauthorized', detail: 'expired' }] } }, ok());
    const minted = ['stale', 'fresh'];

    await expect(call(() => minted.shift())).resolves.toEqual({ data: { id: 'ok' } });

    expect(seen.map((r) => r.authorization)).toEqual(['Bearer stale', 'Bearer fresh']);
  });

  it('waits exactly as long as a 429 asked, not its own backoff', async () => {
    transport({ status: 429, headers: { 'retry-after': '7' }, body: {} }, ok());

    await expect(call(() => 'tok')).resolves.toEqual({ data: { id: 'ok' } });

    expect(waits).toEqual([7_000]);
  });

  // An HTML error page from Apple's edge used to reach the caller as a JSON.parse
  // SyntaxError, which hid the only fact in the response: the status.
  it('reports a 503 that is not JSON by its status, and gives up after four attempts', async () => {
    const page = { status: 503, body: '<html><body>Service Unavailable</body></html>' };
    const seen = transport(page, page, page, page);

    await expect(call(() => 'tok')).rejects.toThrow('ASC GET /v1/apps/42 → 503');

    expect(seen).toHaveLength(4);
    expect(waits).toEqual([1_000, 4_000, 10_000]);
  });

  it('never retries a refusal that a replay cannot change', async () => {
    const seen = transport({ status: 409, body: { errors: [{ code: 'ENTITY_ERROR', title: 'Conflict', detail: 'already exists' }] } });

    const err = await call(() => 'tok', 'POST', '/v1/betaGroups', { data: 1 }).catch((e) => e);

    expect(err.status).toBe(409);
    expect(err.codes).toEqual(['ENTITY_ERROR']);
    expect(err.message).toContain('Conflict: already exists');
    expect(seen).toHaveLength(1);
    expect(seen[0].body).toBe('{"data":1}');
    expect(waits).toEqual([]);
  });
});

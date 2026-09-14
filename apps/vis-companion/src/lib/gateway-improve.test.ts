/** @vitest-environment jsdom */
import { afterEach, describe, expect, it, vi } from 'vitest';
import { GatewayClient } from './gateway';
import { STORY_IMPROVE_RECORDS } from '../dev/story-data';
import { improveDescendants, improveParents } from './improve';

const conn = { url: 'http://gateway.example.com' };

afterEach(() => vi.unstubAllGlobals());

describe('Improve client contract', () => {
  it('keeps Unassigned explicit and passes pagination on the wire', async () => {
    const fetcher = vi.fn(
      async (_input: RequestInfo | URL, _init?: RequestInit) =>
        new Response(JSON.stringify({ records: [], after: 42, has_more: false }), {
          headers: { 'Content-Type': 'application/json' },
        }),
    );
    vi.stubGlobal('fetch', fetcher);
    await new GatewayClient(conn).improveRecords(null, 42);
    const url = new URL(String(fetcher.mock.calls[0][0]));
    expect(url.pathname).toBe('/v1/improve');
    expect(url.searchParams.get('project_id')).toBe('');
    expect(url.searchParams.get('after')).toBe('42');
    expect(url.searchParams.get('limit')).toBe('200');
  });

  it('sends optimistic PATCH and preserves a conflict as an error', async () => {
    const fetcher = vi.fn(
      async (_input: RequestInfo | URL, _init?: RequestInit) =>
        new Response(JSON.stringify({ error: 'conflict', message: 'Issue changed' }), {
          status: 409,
          headers: { 'Content-Type': 'application/json' },
        }),
    );
    vi.stubGlobal('fetch', fetcher);
    await expect(
      new GatewayClient(conn).updateImproveRecord(2, { content: 'My review', expected_version: 3 }),
    ).rejects.toMatchObject({ status: 409 });
    expect(fetcher.mock.calls[0][1]?.method).toBe('PATCH');
    expect(JSON.parse(String(fetcher.mock.calls[0][1]?.body))).toEqual({
      content: 'My review',
      expected_version: 3,
    });
  });

  it('keeps project boundaries and terminates even for incomplete cyclic snapshots', () => {
    expect([...improveDescendants(STORY_IMPROVE_RECORDS, 1)]).toEqual([2]);
    expect(
      improveParents(STORY_IMPROVE_RECORDS, STORY_IMPROVE_RECORDS[0]).map((item) => item.id),
    ).toEqual([3]);
    const cycle = [{ ...STORY_IMPROVE_RECORDS[0], parent_id: 2 }, STORY_IMPROVE_RECORDS[1]];
    expect([...improveDescendants(cycle, 1)]).toEqual([2]);
  });
});

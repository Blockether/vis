/** @vitest-environment jsdom */
import { afterEach, describe, expect, it, vi } from 'vitest';
import teamUpdated from '../../../../packages/vis-contract/resources/vis-contract/fixtures/team-updated.json';
import { GatewayClient } from './gateway';
import type { Subagent } from './types';

const conn = { url: 'http://gateway.example.com' };

afterEach(() => vi.unstubAllGlobals());

const stub = () => {
  const fetcher = vi.fn(
    async (_input: RequestInfo | URL, _init?: RequestInit) =>
      new Response(JSON.stringify([]), { headers: { 'Content-Type': 'application/json' } }),
  );
  vi.stubGlobal('fetch', fetcher);
  return fetcher;
};

const sent = (fetcher: ReturnType<typeof stub>) => {
  const [input, init] = fetcher.mock.calls[0];
  return {
    path: new URL(String(input)).pathname,
    method: init?.method,
    body: JSON.parse(String(init?.body)) as unknown,
  };
};

describe('Subagent control client', () => {
  it('reads the timeline of one child of the leader', async () => {
    const fetcher = vi.fn(
      async (_input: RequestInfo | URL, _init?: RequestInit) =>
        new Response(JSON.stringify({ session_id: 'child', items: [{ at: 1, kind: 'started', text: 'Go' }] }), {
          headers: { 'Content-Type': 'application/json' },
        }),
    );
    vi.stubGlobal('fetch', fetcher);
    const timeline = await new GatewayClient(conn).agentTimeline('lead/1', 'child/2');
    expect(timeline.items).toEqual([{ at: 1, kind: 'started', text: 'Go' }]);
    const [input, init] = fetcher.mock.calls[0];
    expect(new URL(String(input)).pathname).toBe('/v1/sessions/lead%2F1/agents/child%2F2/timeline');
    expect(init?.method).toBe('GET');
  });

  it('stops a child for its leader', async () => {
    const fetcher = stub();
    await new GatewayClient(conn).cancelAgent('lead/1', 'child');
    expect(sent(fetcher)).toEqual({
      path: '/v1/sessions/lead%2F1/agents/cancel',
      method: 'POST',
      body: { session_id: 'child' },
    });
  });

  it('keeps a refused stop as an error with its status', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async () =>
          new Response(JSON.stringify({ error: 'invalid', message: 'No such agent' }), {
            status: 400,
            headers: { 'Content-Type': 'application/json' },
          }),
      ),
    );
    await expect(new GatewayClient(conn).cancelAgent('lead', 'child')).rejects.toMatchObject({ status: 400 });
  });

  it('applies a live team summary to the cached session row', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => new Response(
      JSON.stringify({ id: 'team-row', title: 'Lead' }),
      { headers: { 'Content-Type': 'application/json' } },
    )));
    const client = new GatewayClient({ url: 'http://team-row.example.com' });
    await client.session('team-row');
    expect(client.noteSessionTeam('team-row', { active: 2, total: 3 })?.team).toEqual({ active: 2, total: 3 });
    expect(client.cachedSession('team-row')?.team).toEqual({ active: 2, total: 3 });
    expect(client.noteSessionTeam('team-row', { active: -1, total: 3 })?.team).toEqual({ active: 2, total: 3 });
  });
});


describe('pushed agent rows', () => {
  const answer = (rows: unknown) =>
    vi.stubGlobal('fetch', vi.fn(async () => new Response(JSON.stringify(rows), { headers: { 'Content-Type': 'application/json' } })));

  // Contract fixture `team-updated.json`: the Companion must draw the same team as the gateway pushes.
  it('merges each session.team_updated into the read team, and a repeated event changes nothing', async () => {
    answer(teamUpdated.base);
    const client = new GatewayClient({ url: 'http://team-merge.example.com' });
    await client.agents('lead');
    for (const event of teamUpdated.events) client.applyTeamUpdate('lead', event);
    expect(client.cachedAgents('lead')).toEqual(teamUpdated.merged as unknown as Subagent[]);
    for (const event of teamUpdated.events) client.applyTeamUpdate('lead', event);
    expect(client.cachedAgents('lead')).toEqual(teamUpdated.merged as unknown as Subagent[]);
  });

  // Without a read, one changed row is not the team: the dock would hide the other children.
  it('keeps no team from an event alone unless its rows count every child', () => {
    const client = new GatewayClient({ url: 'http://team-first.example.com' });
    const [partial] = teamUpdated.events;
    expect(client.applyTeamUpdate('lead', partial)).toBeNull();
    expect(client.cachedAgents('lead')).toBeNull();
    expect(client.applyTeamUpdate('lead', { team: { active: 1, total: 1 }, agents: partial.agents })).toEqual(partial.agents);
    expect(client.applyTeamUpdate('lead', { team: { active: 1, total: 1 } })).toBeNull();
  });
});

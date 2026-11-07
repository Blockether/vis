/** @vitest-environment jsdom */
import { afterEach, describe, expect, it, vi } from 'vitest';
import teamUpdated from '../../../../packages/vis-contract/resources/vis-contract/fixtures/team-updated.json';
import { GatewayClient } from './gateway';
import { SessionSubscriptionHub } from './subscriptions';
import type { SseEvent, Subagent } from './types';

afterEach(() => vi.unstubAllGlobals());

describe('Subagent team client', () => {
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

// Contract fixture `team-updated.json`: the Companion must draw the same team as the gateway.
describe('streamed agent rows', () => {
  function hubWith(remember = vi.fn()) {
    let emit: (event: SseEvent) => void = () => {};
    const client = {
      streamSessionEvents: (_cursors: Map<string, number>, next: (event: SseEvent) => void) => {
        emit = next;
        return vi.fn();
      },
      streamFleetStatus: () => vi.fn(),
      rememberAgents: remember,
    } as unknown as GatewayClient;
    const hub = new SessionSubscriptionHub(client);
    hub.subscribeSession('lead', () => {});
    return { hub, emit: (event: Record<string, unknown>) => emit({ session_id: 'lead', ...event } as SseEvent) };
  }

  it('merges each session.team_updated into the ready team, and a repeated event changes nothing', () => {
    const { hub, emit } = hubWith();
    emit({ type: 'subscription.ready', cursor: 1, agents: teamUpdated.base });
    for (const event of teamUpdated.events) emit({ type: 'session.team_updated', ...event });
    expect(hub.team('lead')).toEqual(teamUpdated.merged as unknown as Subagent[]);
    for (const event of teamUpdated.events) emit({ type: 'session.team_updated', ...event });
    expect(hub.team('lead')).toEqual(teamUpdated.merged as unknown as Subagent[]);
    hub.dispose();
  });

  // One changed row is not the team: before the ready frame, a push would hide the other children.
  it('keeps no team from a push before the ready frame', () => {
    const { hub, emit } = hubWith();
    emit({ type: 'session.team_updated', ...teamUpdated.events[0] });
    expect(hub.team('lead')).toBeUndefined();
    hub.dispose();
  });

  it('keeps the last team of a session for the next app start, and forgets it with the session', () => {
    const client = new GatewayClient({ url: 'http://team-cache.example.com' });
    client.rememberAgents('lead', teamUpdated.base as unknown as Subagent[]);
    expect(client.cachedAgents('lead')).toEqual(teamUpdated.base);
    client.rememberAgents('lead', []);
    expect(client.cachedAgents('lead')).toBeNull();
    client.rememberAgents('lead', teamUpdated.base as unknown as Subagent[]);
    client.forgetDeletedSession('lead');
    expect(client.cachedAgents('lead')).toBeNull();
  });
});

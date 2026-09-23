// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';

import { GatewayClient, SESSION_CACHE_LIMIT } from './gateway';
import { SessionSubscriptionHub } from './subscriptions';
import type { SseEvent } from './types';

const RECENT_VISITS = 24; // The persisted subscription window in storage.ts.
let hub: SessionSubscriptionHub | null = null;

afterEach(() => {
  hub?.dispose();
  hub = null;
  vi.restoreAllMocks();
});

function streamClient() {
  const streams: { cursors: Map<string, number>; emit: (event: SseEvent) => void }[] = [];
  const client = {
    streamSessionEvents(cursors: Map<string, number>, emit: (event: SseEvent) => void) {
      streams.push({ cursors, emit });
      return vi.fn();
    },
  } as unknown as GatewayClient;
  return { client, streams };
}

describe('retained session visits', () => {
  it('forgets old cursors and in-flight replay when more sessions are visited', () => {
    const { client, streams } = streamClient();
    hub = new SessionSubscriptionHub(client);
    const recentFirst = Array.from({ length: RECENT_VISITS }, (_, i) => `s${i}`);
    hub.watchSessions(recentFirst);
    const first = streams.at(-1)!;
    first.emit({ type: 'turn.started', session_id: 's0', turn_id: 'recent', seq: 2 });
    first.emit({ type: 'turn.started', session_id: 's23', turn_id: 'old', seq: 4 });
    first.cursors.set('s23', 4);

    hub.watchSessions(['s25', 's24']);
    expect([...hub.watchedSessionIds()]).toHaveLength(RECENT_VISITS);
    expect(hub.isWatching('s0')).toBe(true);
    expect(hub.isWatching('s22')).toBe(false);
    expect(hub.isWatching('s23')).toBe(false);
    expect([...streams.at(-1)!.cursors.keys()]).not.toContain('s23');

    const recent = vi.fn();
    hub.subscribeSession('s0', recent)();
    expect(recent).toHaveBeenCalledWith(expect.objectContaining({ turn_id: 'recent' }));
    const reopened = vi.fn();
    hub.subscribeSession('s23', reopened)();
    expect(reopened).not.toHaveBeenCalled();
    expect(streams.at(-1)!.cursors.get('s23')).toBe(-1);
    expect([...hub.watchedSessionIds()]).toHaveLength(RECENT_VISITS);
  });

  it('does not evict a mounted session while later visits roll through', () => {
    const { client } = streamClient();
    hub = new SessionSubscriptionHub(client);
    const onScreen = vi.fn();
    const unmount = hub.subscribeSession('mounted', onScreen);
    hub.watchSessions(Array.from({ length: RECENT_VISITS + 1 }, (_, i) => `new-${i}`));
    expect(hub.isWatching('mounted')).toBe(true);
    expect([...hub.watchedSessionIds()]).toHaveLength(RECENT_VISITS);
    unmount();
    hub.watchSessions(['newest']);
    expect(hub.isWatching('mounted')).toBe(false);
    expect([...hub.watchedSessionIds()]).toHaveLength(RECENT_VISITS);
  });
});

describe('in-memory running-turn snapshots', () => {
  it('keeps only recently painted sessions, touching a reopened one', () => {
    const client = new GatewayClient({ url: 'http://running-retention.example.com' });
    for (let index = 0; index < SESSION_CACHE_LIMIT; index += 1) {
      client.rememberRunningTurn(`s${index}`, { id: `turn-${index}` }, index);
    }
    expect(client.cachedRunningTurn<{ id: string }>('s0')?.turn.id).toBe('turn-0');
    client.rememberRunningTurn('s10', { id: 'turn-10' }, 10);
    expect(client.cachedRunningTurn('s1')).toBeNull();
    expect(client.cachedRunningTurn<{ id: string }>('s0')?.turn.id).toBe('turn-0');
    client.rememberRunningTurn('s11', { id: 'turn-11' }, 11);
    expect(client.cachedRunningTurn('s2')).toBeNull();
    expect(client.cachedRunningTurn<{ id: string }>('s11')?.turn.id).toBe('turn-11');
  });
});

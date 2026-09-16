// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';

import { GatewayClient } from './gateway';
import { SessionSubscriptionHub } from './subscriptions';
import { draftMessageKey, peekDraftMessage, writeDraftMessage } from './draft-messages';
import type { SseEvent } from './types';

let hub: SessionSubscriptionHub | null = null;
afterEach(() => {
  hub?.dispose();
  hub = null;
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});

// Regression: deleting a session in the TUI or another app left this client's
// watched session, cached transcript and composer draft alive.
describe('session deletion across app clients', () => {
  it.each(['session', 'fleet', 'reconnect'] as const)(
    'retires a deletion from the %s stream once',
    async (channel) => {
      const conn = { url: `http://delete-${channel}.example.com` };
      const client = new GatewayClient(conn);
      const rows = [
        { id: 's1', title: 'Deleted' },
        { id: 's2', title: 'Kept' },
      ];
      vi.stubGlobal(
        'fetch',
        vi.fn(
          async (input: string) =>
            new Response(
              JSON.stringify(
                new URL(input).pathname === '/v1/sessions' ? { sessions: rows } : rows[0],
              ),
            ),
        ),
      );
      await client.listSessions();
      await client.session('s1');
      client.rememberRunningTurn('s1', { id: 'turn-1' }, 4);
      const draft = draftMessageKey(client.base, 's1');
      writeDraftMessage(draft, { text: 'Unsent message' });
      const streams: { cursors: Map<string, number>; emit: (event: SseEvent) => void }[] = [];
      let fleet: (event: SseEvent) => void = () => {};
      vi.spyOn(client, 'streamSessionEvents').mockImplementation((cursors, emit) => {
        streams.push({ cursors, emit });
        return vi.fn();
      });
      vi.spyOn(client, 'streamFleetStatus').mockImplementation((emit) => {
        fleet = emit;
        return vi.fn();
      });
      hub = new SessionSubscriptionHub(client);
      const sessionSeen = vi.fn();
      const fleetSeen = vi.fn();
      hub.subscribeSession('s1', sessionSeen);
      hub.watchSessions(['s2']);
      hub.subscribeFleet(fleetSeen);
      const stream = streams.at(-1)!;
      stream.emit({ type: 'turn.started', session_id: 's1', turn_id: 'turn-1', seq: 4 });
      sessionSeen.mockClear();
      fleetSeen.mockClear();
      const deleted: SseEvent = { type: 'session.deleted', session_id: 's1', seq: 5 };
      if (channel === 'reconnect') {
        stream.cursors.set('s1', 4);
        const opened = streams.length;
        hub.resync();
        expect(streams).toHaveLength(opened + 1);
        expect(streams.at(-1)!.cursors.get('s1')).toBe(4);
        // A missing session answers the reconnect with deletion, without a ready frame.
        streams.at(-1)!.emit(deleted);
      } else {
        (channel === 'session' ? stream.emit : fleet)(deleted);
      }
      stream.emit(deleted);
      fleet(deleted);
      stream.emit({ type: 'turn.started', session_id: 's1', turn_id: 'late', seq: 6 });

      expect(sessionSeen.mock.calls).toEqual([[deleted]]);
      expect(fleetSeen.mock.calls).toEqual([[deleted]]);
      expect([...hub.watchedSessionIds()]).toEqual(['s2']);
      expect([...stream.cursors.keys()]).toEqual(['s2']);
      expect(client.cachedSession('s1')).toBeNull();
      expect(client.cachedRunningTurn('s1')).toBeNull();
      expect(client.cachedSessions()?.map((row) => row.id)).toEqual(['s2']);
      expect(peekDraftMessage(draft).text).toBe('');
      // A stale list or project-page response from another client instance cannot
      // overwrite the deletion shared with this gateway's snapshots.
      const other = new GatewayClient(conn);
      expect((await other.listSessions()).map((row) => row.id)).toEqual(['s2']);
      const page = await other.listProjectPage('/project', 10, '', new Map());
      expect(page.rows.map((row) => row.id)).toEqual(['s2']);
      expect(page.total).toBe(1);
      const reopened = vi.fn();
      const opened = streams.length;
      hub.subscribeSession('s1', reopened);
      hub.watchSessions(['s1']);
      expect(reopened).toHaveBeenCalledWith(
        expect.objectContaining({ type: 'session.deleted' }),
      );
      expect(reopened).toHaveBeenCalledTimes(1);
      expect(streams).toHaveLength(opened);
    },
  );
});

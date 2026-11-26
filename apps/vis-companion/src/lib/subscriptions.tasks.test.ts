/** @vitest-environment jsdom */
import { expect, it, vi } from 'vitest';
import taskFixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/task.json';
import type { GatewayClient } from './gateway';
import { SessionSubscriptionHub } from './subscriptions';
import type { SseEvent } from './types';

it('keeps background tasks across turns, ignores stale replay and removes only the ended task', () => {
  let receive: (event: SseEvent) => void = () => {};
  const client = {
    streamSessionEvents: (_: Map<string, number>, next: typeof receive) => {
      receive = next;
      return vi.fn();
    },
    forgetDeletedSession: vi.fn(),
  } as unknown as GatewayClient;
  const hub = new SessionSubscriptionHub(client);
  const sid = 'lead';
  const row = { ...taskFixture, id: 't1', session_id: sid, state: 'running', detached: false };
  const other = { ...row, id: 't2' };
  const push = (event: SseEvent) => receive({ session_id: sid, ...event });
  try {
    hub.subscribeSession(sid, () => {});
    push({ type: 'subscription.ready', cursor: 2, tasks_seq: 10, tasks: [row, other] });
    push({ type: 'task.started', seq: 9, task: { ...row, id: 't-old' } });
    push({ type: 'turn.started', seq: 11 });
    push({ type: 'task.updated', seq: 12, task: { ...row, cancel_reason: 'person' } });
    expect(hub.tasks(sid)?.map(({ id, cancel_reason }) => [id, cancel_reason])).toEqual([['t1', 'person'], ['t2', undefined]]);
    push({ type: 'task.ended', seq: 13, task: { ...row, state: 'cancelled' } });
    push({ type: 'task.updated', seq: 12, task: row });
    expect(hub.tasks(sid)?.map(({ id }) => id)).toEqual(['t2']);
    push({ type: 'subscription.ready', cursor: 13, tasks_seq: 14, tasks: [] });
    expect(hub.tasks(sid)).toEqual([]);
    push({ type: 'session.deleted' });
    push({ type: 'task.started', seq: 15, task: row });
    expect(hub.tasks(sid)).toBeUndefined();
  } finally {
    hub.dispose();
  }
});

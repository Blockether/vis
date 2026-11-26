/** @vitest-environment jsdom */
import { act, fireEvent, render, screen } from '@testing-library/react';
import { expect, it, vi } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/task.json';
import type { GatewayClient } from '../lib/gateway';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';
import { BackgroundTasks } from './BackgroundTasks';

it('asks to cancel just the chosen task and leaves its sibling active', async () => {
  let receive: (event: SseEvent) => void = () => {};
  let finish: (requested: boolean) => void = () => {};
  const cancelTask = vi.fn(() => new Promise<boolean>((resolve) => { finish = resolve; }));
  const client = {
    cancelTask,
    streamSessionEvents: (_: Map<string, number>, next: typeof receive) => {
      receive = next;
      return vi.fn();
    },
  } as unknown as GatewayClient;
  const subscriptions = new SessionSubscriptionHub(client);
  const sid = fixture.session_id;
  const first = { ...fixture, id: 't1', name: 'Watch files', state: 'running', detached: false };
  const other = { ...first, id: 't2', name: 'List files' };
  const push = (event: SseEvent) => act(() => receive({ session_id: sid, ...event }));
  const ui = render(<BackgroundTasks client={client} subscriptions={subscriptions} sid={sid} views={[]} onOpenView={() => {}} />);
  try {
    push({ type: 'subscription.ready', cursor: 0, tasks: [first, other] });
    const cancel = screen.getByRole('button', { name: 'Cancel Watch files' });
    fireEvent.click(cancel);
    expect(cancelTask.mock.calls).toEqual([[sid, 't1']]);
    expect(cancel).toBeDisabled();
    push({ type: 'task.updated', seq: 1, task: { ...first, cancel_reason: 'person' } });
    await act(async () => finish(true));
    expect(cancel).toBeDisabled();
    push({ type: 'task.ended', seq: 2, task: { ...first, state: 'cancelled' } });
    expect(screen.queryByRole('button', { name: 'Cancel Watch files' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Cancel List files' })).toBeEnabled();
  } finally {
    ui.unmount();
    subscriptions.dispose();
  }
});

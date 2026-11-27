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
  let finish: (outcome: { requested: true }) => void = () => {};
  const cancelTask = vi.fn(() => new Promise<{ requested: true }>((resolve) => { finish = resolve; }));
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
    await act(async () => finish({ requested: true }));
    expect(cancel).toBeDisabled();
    push({ type: 'task.ended', seq: 2, task: { ...first, state: 'cancelled' } });
    expect(screen.queryByRole('button', { name: 'Cancel Watch files' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Cancel List files' })).toBeEnabled();
  } finally {
    ui.unmount();
    subscriptions.dispose();
  }
});

it.each(['already_ended', 'already_requested', 'request failure'] as const)(
  'refreshes a benign Task refusal and reports only a failed request (%s)', async (reason) => {
    let receive: (event: SseEvent) => void = () => {};
    const sid = fixture.session_id;
    const row = { ...fixture, id: 't1', name: 'Watch files', state: 'running', detached: false };
    const readTask = vi.fn(async () => reason === 'already_ended'
      ? { ...row, state: 'completed' }
      : { ...row, cancel_reason: 'person' });
    const client = {
      cancelTask: vi.fn(async () => {
        if (reason === 'request failure') throw new Error('offline');
        return { requested: false, reason };
      }),
      readTask,
      streamSessionEvents: (_: Map<string, number>, next: typeof receive) => {
        receive = next;
        return vi.fn();
      },
    } as unknown as GatewayClient;
    const subscriptions = new SessionSubscriptionHub(client);
    const ui = render(<BackgroundTasks client={client} subscriptions={subscriptions} sid={sid} views={[]} onOpenView={() => {}} />);
    try {
      act(() => receive({ type: 'subscription.ready', session_id: sid, cursor: 0, tasks: [row] }));
      await act(async () => fireEvent.click(screen.getByRole('button', { name: 'Cancel Watch files' })));
      if (reason === 'request failure') {
        expect(screen.getByRole('alert')).toHaveTextContent('Could not cancel Watch files.');
        expect(readTask).not.toHaveBeenCalled();
      } else {
        expect(screen.queryByRole('alert')).toBeNull();
        expect(readTask).toHaveBeenCalledWith(sid, 't1');
        if (reason === 'already_ended') {
          expect(screen.queryByRole('button', { name: 'Cancel Watch files' })).toBeNull();
        } else {
          expect(screen.getByRole('button', { name: 'Cancel Watch files' })).toBeDisabled();
        }
      }
    } finally {
      ui.unmount();
      subscriptions.dispose();
    }
  },
);

it('does not bring back a Task that ends while a refusal refresh is in flight', async () => {
  let receive: (event: SseEvent) => void = () => {};
  const sid = fixture.session_id;
  const row = { ...fixture, id: 't1', name: 'Watch files', state: 'running' };
  let finish!: (task: typeof row) => void;
  const readTask = vi.fn(() => new Promise<typeof row>((resolve) => { finish = resolve; }));
  const client = {
    cancelTask: vi.fn(async () => ({ requested: false, reason: 'already_requested' })),
    readTask,
    streamSessionEvents: (_: Map<string, number>, next: typeof receive) => {
      receive = next;
      return vi.fn();
    },
  } as unknown as GatewayClient;
  const subscriptions = new SessionSubscriptionHub(client);
  const ui = render(<BackgroundTasks client={client} subscriptions={subscriptions} sid={sid} views={[]} onOpenView={() => {}} />);
  try {
    act(() => receive({ type: 'subscription.ready', session_id: sid, cursor: 0, tasks: [row] }));
    await act(async () => fireEvent.click(screen.getByRole('button', { name: 'Cancel Watch files' })));
    expect(readTask).toHaveBeenCalledWith(sid, row.id);
    act(() => receive({ type: 'task.ended', session_id: sid, seq: 1, task: { ...row, state: 'cancelled' } }));
    await act(async () => finish(row));
    expect(screen.queryByRole('button', { name: 'Cancel Watch files' })).toBeNull();
    expect(screen.queryByRole('alert')).toBeNull();
  } finally {
    ui.unmount();
    subscriptions.dispose();
  }
});

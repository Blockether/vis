// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/view.json';
import { GatewayClient } from '../lib/gateway';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';
import { HumanInputPrompt } from './HumanInputPrompt';

const leader = 'council-leader';
const inputPath = `/v1/sessions/${leader}/views/input`;

function question(n: number) {
  return {
    ...fixture.input,
    id: `ask-${n}`,
    title: `Question ${n}`,
    source: `tool-${n}`,
    session_id: `child-${n}`,
    origin: {
      session_id: `child-${n}`,
      title: `Agent ${n}`,
      parent_session_id: leader,
      parent_title: 'Leader',
    },
    created_at: n,
  };
}

const open = (view: ReturnType<typeof question>): SseEvent => ({
  type: 'view.open', kind: 'input', session_id: leader, view_id: view.id, view,
});
const close = (view: ReturnType<typeof question>): SseEvent => ({
  type: 'view.close', kind: 'input', session_id: leader, view_id: view.id,
});

function deferredResponse() {
  let resolve!: (value: unknown) => void;
  const promise = new Promise<Response>((settle) => {
    resolve = (value) => settle(Response.json(value));
  });
  return { promise, resolve };
}

const hubs = new Set<SessionSubscriptionHub>();
afterEach(() => {
  cleanup();
  for (const hub of hubs) hub.dispose();
  hubs.clear();
  vi.unstubAllGlobals();
});

function harness() {
  const client = new GatewayClient({ url: 'https://council.example.com' });
  const reads: ReturnType<typeof deferredResponse>[] = [];
  const posts: { path: string; body: unknown; reply: ReturnType<typeof deferredResponse> }[] = [];
  vi.stubGlobal('fetch', vi.fn(async (url: string, init: RequestInit) => {
    const path = new URL(url).pathname;
    const reply = deferredResponse();
    if (init.method === 'GET' && path === inputPath) reads.push(reply);
    else if (init.method === 'GET' && path.endsWith('/views/input')) return Response.json({ requests: [] });
    else if (init.method === 'POST') posts.push({ path, body: JSON.parse(String(init.body)), reply });
    else return new Response(null, { status: 404 });
    return reply.promise;
  }));
  let deliver!: (event: SseEvent) => void;
  vi.spyOn(client, 'streamSessionEvents').mockImplementation((_cursors, receive) => {
    deliver = receive;
    return () => {};
  });
  const subscriptions = new SessionSubscriptionHub(client);
  hubs.add(subscriptions);
  return {
    reads, posts,
    prompt: (sid = leader) => <HumanInputPrompt client={client} subscriptions={subscriptions} sid={sid} />,
    emit: (...events: SseEvent[]) => act(async () => { for (const event of events) deliver(event); }),
    reconnect: () => act(async () => deliver({ type: 'subscription.ready', session_id: leader, is_live: false })),
  };
}

const answer = () => screen.getByRole('textbox', { name: /Name/ });
const expectQuestion = (n: number) => expect(screen.getByRole('heading', { name: `Question ${n}` })).toBeVisible();

it('keeps three child forms in order when a list read overlaps open and close events', async () => {
  const h = harness();
  const [one, two, three, four] = [1, 2, 3, 4].map(question);
  render(h.prompt());
  expect(h.reads).toHaveLength(1);
  await h.emit(open(one), open(two), open(three), close(two), open(four));
  await act(async () => h.reads[0].resolve({ requests: [one, two] }));
  expectQuestion(1);
  expect(screen.getByText('2 more requests waiting')).toBeVisible();
  expect(screen.getByText('Agent 1')).toBeVisible();
  expect(screen.getByText('tool-1')).toBeVisible();
  expect(answer()).toHaveFocus();
  fireEvent.change(answer(), { target: { value: 'Ada' } });
  fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
  expect(h.posts.map(({ path, body }) => ({ path, body }))).toEqual([{
    path: `/v1/sessions/${leader}/views/${one.id}/actions`,
    body: { action: 'submit', values: fixture.answer.values },
  }]);
  await h.emit(close(one));
  expectQuestion(3);
  expect(answer()).toHaveValue('');
  expect(answer()).toHaveFocus();
  fireEvent.change(answer(), { target: { value: 'Grace' } });
  await act(async () => h.posts[0].reply.resolve({ is_accepted: false, errors: { name: 'Late refusal' } }));
  expect(screen.queryByText('Late refusal')).toBeNull();
  expect(answer()).toHaveValue('Grace');
  fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
  expect(h.posts[1].path).toBe(`/v1/sessions/${leader}/views/${three.id}/actions`);
  expect(h.posts[1].body).toEqual({ action: 'cancel' });
  await h.emit(close(four));
  expectQuestion(3);
  await act(async () => h.posts[1].reply.resolve({ is_accepted: true }));
  expect(screen.queryByRole('dialog')).toBeNull();
});

it('does not resurrect a form when an older list response finishes after a reconnect', async () => {
  const h = harness();
  render(h.prompt());
  await h.reconnect();
  expect(h.reads).toHaveLength(2);
  await act(async () => h.reads[1].resolve({ requests: [] }));
  await act(async () => h.reads[0].resolve({ requests: [question(1)] }));
  expect(screen.queryByRole('dialog')).toBeNull();
});

// The operator can return to an open form, but must enter its secrets again.
it('keeps a child question through terminal frames and restores it without secret drafts', async () => {
  const h = harness();
  const row = question(1);
  const request = {
    ...row,
    nodes: [
      ...row.nodes,
      { id: 'password', type: 'password', label: 'Work password' },
      { id: 'code', type: 'otp', label: 'Work code' },
    ],
  };
  const view = render(h.prompt());
  await act(async () => h.reads[0].resolve({ requests: [] }));
  await h.emit({ ...open(row), view: request });
  const password = () => screen.getByLabelText('Work password');
  const code = () => screen.getByLabelText('Work code');
  fireEvent.change(answer(), { target: { value: 'Ada' } });
  fireEvent.change(password(), { target: { value: 'example-only' } });
  fireEvent.change(code(), { target: { value: '227662' } });
  expect(fireEvent.copy(code())).toBe(false);
  expect(fireEvent.cut(code())).toBe(false);
  for (const type of ['turn.completed', 'turn.failed', 'turn.cancelled']) {
    await h.emit({ type, session_id: leader, turn_id: 'leader-turn' });
    expectQuestion(1);
    expect(password()).toHaveValue('example-only');
    expect(code()).toHaveValue('227662');
  }
  view.rerender(h.prompt('other-session'));
  expect(screen.queryByRole('dialog')).toBeNull();
  view.rerender(h.prompt());
  await act(async () => h.reads[1].resolve({ requests: [request] }));
  expectQuestion(1);
  expect(answer()).toHaveValue('Ada');
  expect(password()).toHaveValue('');
  expect(code()).toHaveValue('');
  view.unmount();
  render(h.prompt());
  await act(async () => h.reads[2].resolve({ requests: [request] }));
  expectQuestion(1);
});

it('answers the oldest question first when the stream delivers it after a newer question', async () => {
  const h = harness();
  const older = question(1);
  const newer = question(2);
  render(h.prompt());
  await h.emit(open(older));
  await act(async () => h.reads[0].resolve({ requests: [newer] }));
  expectQuestion(1);
  await h.emit(close(older));
  expectQuestion(2);
});

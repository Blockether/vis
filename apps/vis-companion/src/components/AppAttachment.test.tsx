// @vitest-environment jsdom
// Issue #421: an app attachment opens one surface that reads the host state,
// follows the session stream and saves through the view actions.
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { GatewayClient } from '../lib/gateway';
import { GatewayError } from '../lib/gateway';
import { liveViewFromWire, type LiveView } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';
import { VIEW_PATCH_EVENT, type ViewAction } from '../lib/view';
import { AppSurface, useSessionApps } from './AppAttachment';
import { StoryAppHost, TODO, type StoryApp } from '../dev/story-apps';
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT } from '../lib/view';

afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
});

function wire(seq: number, draft?: string) {
  return {
    id: 'app-1',
    title: 'Code review',
    placement: 'attachment',
    seq,
    nodes: [
      {
        id: 'review',
        type: 'diff',
        patch: '--- a/src/a.ts\n+++ b/src/a.ts\n@@ -1 +1 @@\n-const a = 1;\n+const a = 2; // a long line that scrolls sideways on a phone',
        new_revision: 'bbb',
        threads: [
          {
            id: 't1',
            anchor: { path: 'src/a.ts', side: 'new', line: 1 },
            messages: [{ id: 'm1', author: { kind: 'agent', name: 'Vis' }, body: 'Bumped it.', created_at: 1 }],
            ...(draft ? { draft } : {}),
          },
        ],
      },
    ],
  };
}

function view(seq: number, draft?: string): LiveView {
  const parsed = liveViewFromWire(wire(seq, draft));
  if (!parsed) throw new Error('fixture must parse');
  return parsed;
}

function fakeHub() {
  const listeners = new Set<(event: SseEvent) => void>();
  const connection = new Set<(connected: boolean) => void>();
  const hub = {
    subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
    subscribeConnection: (listener: (connected: boolean) => void) => {
      connection.add(listener);
      listener(true);
      return () => connection.delete(listener);
    },
  } as unknown as SessionSubscriptionHub;
  return {
    hub,
    emit: (event: SseEvent) => listeners.forEach((listener) => listener(event)),
    connect: (connected: boolean) => connection.forEach((listener) => listener(connected)),
  };
}

function fakeClient(reads: LiveView[], act: (action: ViewAction) => Promise<unknown>) {
  const liveView = vi.fn(async () => reads.shift() ?? null);
  const viewAction = vi.fn(async (_sid: string, _vid: string, action: ViewAction) => act(action));
  return {
    client: { liveView, viewAction, liveViewLog: vi.fn() } as unknown as GatewayClient,
    liveView,
    viewAction,
  };
}

function open(client: GatewayClient, hub: SessionSubscriptionHub | null) {
  return render(
    <SessionEventsContext.Provider value={hub}>
      <div data-session-surface>
        <AppSurface client={client} sid="s1" viewId="app-1" fallbackTitle="code-review" onClose={() => undefined} />
      </div>
    </SessionEventsContext.Provider>,
  );
}

describe('AppSurface', () => {
  it('reads the app, folds its patches and reads again after a seq gap', async () => {
    const { hub, emit } = fakeHub();
    const { client, liveView } = fakeClient([view(3), view(9, 'From the other client')], async () => ({}));
    open(client, hub);
    expect(await screen.findByText('Bumped it.')).toBeInTheDocument();
    expect(screen.getByText('Live')).toBeInTheDocument();
    act(() =>
      emit({
        type: VIEW_PATCH_EVENT,
        kind: 'live',
        view_id: 'app-1',
        first_seq: 4,
        patch: {
          seq: 4,
          ops: [
            {
              op: 'thread',
              node_id: 'review',
              thread: {
                id: 't1',
                anchor: { path: 'src/a.ts', side: 'new', line: 1 },
                messages: [{ id: 'm1', author: { kind: 'agent', name: 'Vis' }, body: 'Bumped it.', created_at: 1 }],
                draft: 'Patched draft',
              },
            },
          ],
        },
      }),
    );
    expect(screen.getByText('Patched draft')).toBeInTheDocument();
    act(() =>
      emit({ type: VIEW_PATCH_EVENT, kind: 'live', view_id: 'app-1', first_seq: 8, patch: { seq: 8, ops: [] } }),
    );
    expect(await screen.findByText('From the other client')).toBeInTheDocument();
    expect(liveView).toHaveBeenCalledTimes(2);
  });

  it('saves a draft with base_seq and the revision, then reads again on a conflict', async () => {
    const { hub } = fakeHub();
    const { client, viewAction, liveView } = fakeClient([view(3, 'Mine'), view(5, 'Theirs')], async () => {
      throw new GatewayError(409, 'conflict', { is_accepted: false, reason: 'conflict', seq: 5 });
    });
    open(client, hub);
    fireEvent.click(await screen.findByRole('button', { name: 'Edit draft' }));
    fireEvent.change(screen.getByRole('textbox', { name: 'Edit draft' }), { target: { value: 'Mine, edited' } });
    fireEvent.click(screen.getByRole('button', { name: 'Save' }));
    expect(await screen.findByText('Changed elsewhere')).toBeInTheDocument();
    expect(viewAction.mock.calls[0][2]).toEqual({
      action: 'draft',
      node_id: 'review',
      thread_id: 't1',
      body: 'Mine, edited',
      base_seq: 3,
      revision: 'bbb',
    });
    await waitFor(() => expect(liveView).toHaveBeenCalledTimes(2));
  });

  it('reads the app again after a reconnect', async () => {
    const { hub, connect } = fakeHub();
    const { client, liveView } = fakeClient([view(3), view(4)], async () => ({}));
    open(client, hub);
    await screen.findByText('Bumped it.');
    act(() => connect(false));
    act(() => connect(true));
    await waitFor(() => expect(liveView).toHaveBeenCalledTimes(2));
  });

  it('shows a disconnected or closed app read only', async () => {
    const disconnected = liveViewFromWire({ ...wire(3, 'Kept'), is_stale: true });
    const { client } = fakeClient(disconnected ? [disconnected] : [], async () => ({}));
    open(client, null);
    expect(await screen.findByText('Disconnected')).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: 'Edit draft' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Send review' })).toHaveProperty('disabled', true);
  });

  it('names a closed app once and never as Done', async () => {
    // Issue #430: a closed app read "Closed" in the title and "Done" over the body,
    // and "Done" looked like a successful Save that the app never reported.
    const closed = liveViewFromWire({ ...wire(3), is_closed: true });
    const { client } = fakeClient(closed ? [closed] : [], async () => ({}));
    open(client, null);
    expect(await screen.findAllByText('Closed')).toHaveLength(1);
    expect(screen.queryByText('Done')).toBeNull();
  });
});

// Live apps: an app is any app, so its buttons, cards, tables and keys reach the owner.
function game(seq: number, extra: Record<string, unknown> = {}) {
  const parsed = liveViewFromWire({
    id: 'app-1',
    title: 'Counter',
    placement: 'attachment',
    seq,
    nodes: [
      { id: 'count', type: 'stat', label: 'Count', stats: [{ id: 'n', label: 'Count', value_text: String(seq) }] },
      { id: 'inc', type: 'button', label: 'Add one' },
      { id: 'pick', type: 'card', title: 'Pick me', is_selectable: true },
      { id: 'name', type: 'plaintext', label: 'Name' },
      {
        id: 'rows',
        type: 'table',
        columns: [{ id: 'name', label: 'Name' }],
        rows: [{ id: 'r1', cells: ['One'] }],
        is_selectable: true,
      },
    ],
    shortcuts: [
      { key: 'ArrowUp', action: 'up', label: 'Up' },
      { key: '+', node_id: 'inc' },
      { key: 'q', action: 'quit' },
    ],
    ...extra,
  });
  if (!parsed) throw new Error('fixture must parse');
  return parsed;
}

function nodeButton(id: string): HTMLButtonElement {
  const found = document.querySelector<HTMLButtonElement>(`[data-node-id="${id}"]`);
  if (!found) throw new Error(`no node ${id}`);
  return found;
}

function accepted(action: ViewAction) {
  return Promise.resolve({ action: action.action, view_id: 'app-1', is_accepted: true, seq: 9 });
}

describe('AppSurface controls and keys', () => {
  it('sends a button press and a card press as activate', async () => {
    const { client, viewAction } = fakeClient([game(3)], accepted);
    open(client, null);
    await screen.findByText('Pick me');
    fireEvent.click(nodeButton('inc'));
    await waitFor(() => expect(viewAction).toHaveBeenCalledTimes(1));
    expect(viewAction.mock.calls[0][2]).toEqual({ action: 'activate', node_id: 'inc' });
    fireEvent.click(nodeButton('pick'));
    await waitFor(() => expect(viewAction).toHaveBeenCalledTimes(2));
    expect(viewAction.mock.calls[1][2]).toEqual({ action: 'activate', node_id: 'pick' });
  });

  it('sends an owner key as a shortcut action, in the order pressed', async () => {
    const order: string[] = [];
    let release: () => void = () => undefined;
    const gate = new Promise<void>((resolve) => {
      release = resolve;
    });
    const { client, viewAction } = fakeClient([game(3)], async (action) => {
      if (action.action === 'shortcut') order.push(action.key);
      if (order.length === 1) await gate;
      return accepted(action);
    });
    open(client, null);
    await screen.findByRole('toolbar', { name: 'App keys' });
    fireEvent.keyDown(document.body, { key: 'ArrowUp' });
    fireEvent.keyDown(document.body, { key: 'q' });
    // The second key waits for the first answer.
    await waitFor(() => expect(viewAction).toHaveBeenCalledTimes(1));
    release();
    await waitFor(() => expect(viewAction).toHaveBeenCalledTimes(2));
    expect(order).toEqual(['ArrowUp', 'q']);
    expect(viewAction.mock.calls[0][2]).toEqual({ action: 'shortcut', key: 'ArrowUp' });
    fireEvent.keyDown(window, { key: 'ArrowUp' });
    await waitFor(() => expect(viewAction).toHaveBeenCalledTimes(3));
  });

  it('presses the bound node for a node key', async () => {
    const { client, viewAction } = fakeClient([game(3)], accepted);
    open(client, null);
    await screen.findByText('Pick me');
    fireEvent.keyDown(document.body, { key: '+' });
    await waitFor(() => expect(viewAction).toHaveBeenCalledTimes(1));
    expect(viewAction.mock.calls[0][2]).toEqual({ action: 'activate', node_id: 'inc' });
    expect(nodeButton('inc').getAttribute('aria-keyshortcuts')).toBe('+');
  });

  it('leaves keys with a modifier, a held key and a key in a field alone', async () => {
    const { client, viewAction } = fakeClient([game(3)], accepted);
    open(client, null);
    const name = await screen.findByRole('textbox', { name: 'Name' });
    fireEvent.keyDown(document.body, { key: 'q', ctrlKey: true });
    fireEvent.keyDown(document.body, { key: 'q', metaKey: true });
    fireEvent.keyDown(document.body, { key: 'q', repeat: true });
    fireEvent.keyDown(name, { key: 'q' });
    fireEvent.keyDown(document.body, { key: 'Escape' });
    await new Promise((resolve) => setTimeout(resolve, 20));
    expect(viewAction).not.toHaveBeenCalled();
  });

  it('follows a shortcuts patch and takes no keys when the app closes', async () => {
    const { hub, emit } = fakeHub();
    const { client, viewAction } = fakeClient([game(3), game(5, { is_closed: true })], accepted);
    open(client, hub);
    await screen.findByRole('toolbar', { name: 'App keys' });
    act(() =>
      emit({
        type: VIEW_PATCH_EVENT,
        kind: 'live',
        view_id: 'app-1',
        first_seq: 4,
        patch: { seq: 4, ops: [{ op: 'shortcuts', shortcuts: [{ key: 'r', action: 'restart', label: 'Restart' }] }] },
      }),
    );
    expect(screen.getByRole('toolbar', { name: 'App keys' }).textContent).toContain('Restart');
    fireEvent.keyDown(document.body, { key: 'q' });
    act(() => emit({ type: VIEW_CLOSE_EVENT, kind: 'live', view_id: 'app-1' }));
    await waitFor(() => expect(screen.queryByRole('toolbar', { name: 'App keys' })).toBeNull());
    fireEvent.keyDown(document.body, { key: 'r' });
    await new Promise((resolve) => setTimeout(resolve, 20));
    expect(viewAction).not.toHaveBeenCalled();
  });

  it('sends a table selection as select', async () => {
    const { client, viewAction } = fakeClient([game(3)], accepted);
    open(client, null);
    fireEvent.click(await screen.findByRole('button', { name: 'Select One' }));
    await waitFor(() => expect(viewAction).toHaveBeenCalled());
    expect(viewAction.mock.calls[0][2]).toEqual({ action: 'select', node_id: 'rows', item_ids: ['r1'] });
  });
});

function AppNames({ client }: { client: GatewayClient }) {
  const apps = useSessionApps(client, 's1');
  return <p data-testid="apps">{apps.map((one) => `${one.title}:${one.is_closed ? 'closed' : 'open'}`).join(',')}</p>;
}

describe('useSessionApps', () => {
  it('adds an opened app, follows a close and reads the list again after a reconnect', async () => {
    const { hub, emit, connect } = fakeHub();
    const appViews = vi.fn(async () => [game(3)]);
    const client = { appViews } as unknown as GatewayClient;
    render(
      <SessionEventsContext.Provider value={hub}>
        <AppNames client={client} />
      </SessionEventsContext.Provider>,
    );
    await waitFor(() => expect(screen.getByTestId('apps').textContent).toBe('Counter:open'));
    act(() =>
      emit({
        type: VIEW_OPEN_EVENT,
        kind: 'live',
        view_id: 'app-2',
        view: { id: 'app-2', title: 'Board', placement: 'attachment', seq: 1, nodes: [{ id: 'p', type: 'paragraph', text: 'x' }] },
      }),
    );
    // A view that is not an app stays out of the list.
    act(() =>
      emit({
        type: VIEW_OPEN_EVENT,
        kind: 'live',
        view_id: 'run-1',
        view: { id: 'run-1', title: 'Run', seq: 1, nodes: [{ id: 'p', type: 'paragraph', text: 'x' }] },
      }),
    );
    expect(screen.getByTestId('apps').textContent).toBe('Counter:open,Board:open');
    act(() => emit({ type: VIEW_CLOSE_EVENT, kind: 'live', view_id: 'app-2' }));
    expect(screen.getByTestId('apps').textContent).toBe('Counter:open,Board:closed');
    act(() => connect(false));
    act(() => connect(true));
    await waitFor(() => expect(appViews.mock.calls.length).toBeGreaterThanOrEqual(2));
  });
});

function wizard(seq: number, extra: Record<string, unknown> = {}): LiveView {
  const parsed = liveViewFromWire({
    id: 'app-1',
    title: 'Deploy wizard',
    placement: 'attachment',
    seq,
    nodes: [
      { id: 'agree', type: 'checkbox', label: 'I read the notes' },
      {
        id: 'env',
        type: 'select',
        label: 'Environment',
        options: [
          { value: 'staging', label: 'Staging' },
          { value: 'prod', label: 'Production' },
        ],
      },
      { id: 'next', type: 'button', label: 'Next' },
    ],
    shortcuts: [
      { key: 'n', node_id: 'next' },
      { key: 'ArrowUp', action: 'up', label: 'Up' },
      { key: 'k', action: 'up', label: 'Up' },
    ],
    ...extra,
  });
  if (!parsed) throw new Error('fixture must parse');
  return parsed;
}

describe('AppSurface focus, names and owner loss', () => {
  it('takes the focus when it opens and gives it back to the opener when it closes', async () => {
    const { client } = fakeClient([wizard(3)], accepted);
    const opener = document.createElement('button');
    document.body.append(opener);
    opener.focus();
    const shown = open(client, null);
    await screen.findByRole('toolbar', { name: 'App keys' });
    const surface = document.querySelector<HTMLElement>('[data-app-surface="app-1"]');
    expect(document.activeElement).toBe(surface);
    shown.unmount();
    expect(document.activeElement).toBe(opener);
    opener.remove();
  });

  it('takes a key that lands on the opener behind the app', async () => {
    const { client, viewAction } = fakeClient([wizard(3)], accepted);
    const opener = document.createElement('button');
    document.body.append(opener);
    opener.focus();
    open(client, null);
    await screen.findByRole('toolbar', { name: 'App keys' });
    fireEvent.keyDown(opener, { key: 'ArrowUp' });
    await waitFor(() => expect(viewAction).toHaveBeenCalledTimes(1));
    expect(viewAction.mock.calls[0][2]).toEqual({ action: 'shortcut', key: 'ArrowUp' });
    opener.remove();
  });

  it('keeps app keys after a click on a checkbox or a radio option', async () => {
    const { client, viewAction } = fakeClient([wizard(3)], accepted);
    open(client, null);
    const agree = await screen.findByRole('checkbox', { name: /I read the notes/ });
    fireEvent.click(agree);
    agree.focus();
    fireEvent.keyDown(agree, { key: 'n' });
    await waitFor(() =>
      expect(viewAction.mock.calls.map((call) => call[2])).toContainEqual({ action: 'activate', node_id: 'next' }),
    );
    const radio = screen.getByRole('radio', { name: /Production/ });
    fireEvent.click(radio);
    radio.focus();
    const before = viewAction.mock.calls.length;
    fireEvent.keyDown(radio, { key: 'ArrowUp' });
    await waitFor(() => expect(viewAction.mock.calls.length).toBeGreaterThan(before));
    expect(viewAction.mock.calls.at(-1)?.[2]).toEqual({ action: 'shortcut', key: 'ArrowUp' });
  });

  it('goes read only on a disconnected refusal and comes back live on the adopting open', async () => {
    const { hub, emit } = fakeHub();
    const { client, viewAction } = fakeClient([wizard(3), wizard(3, { is_stale: true })], async () => {
      throw new GatewayError(409, 'disconnected', { is_accepted: false, reason: 'disconnected' });
    });
    open(client, hub);
    await screen.findByRole('toolbar', { name: 'App keys' });
    fireEvent.keyDown(document.body, { key: 'ArrowUp' });
    expect(await screen.findByText('Disconnected')).toBeInTheDocument();
    // The state word says it once; the refusal adds no second word and no key works.
    expect(screen.getAllByText('Disconnected')).toHaveLength(1);
    expect(screen.queryByRole('toolbar', { name: 'App keys' })).toBeNull();
    expect(nodeButton('next')).toHaveProperty('disabled', true);
    fireEvent.keyDown(document.body, { key: 'ArrowUp' });
    await new Promise((resolve) => setTimeout(resolve, 20));
    expect(viewAction).toHaveBeenCalledTimes(1);
    // The owner adopts the app again: the same picture, now without the stale flag.
    act(() =>
      emit({
        type: VIEW_OPEN_EVENT,
        kind: 'live',
        view_id: 'app-1',
        view: { ...wizardWire(), seq: 3 },
      } as SseEvent),
    );
    expect(await screen.findByText('Live')).toBeInTheDocument();
    expect(await screen.findByRole('toolbar', { name: 'App keys' })).toBeInTheDocument();
    expect(nodeButton('next')).toHaveProperty('disabled', false);
  });

  it('shows a stale open as disconnected and keeps one row for the app', async () => {
    const { hub, emit } = fakeHub();
    const { client } = fakeClient([wizard(3)], accepted);
    open(client, hub);
    await screen.findByRole('toolbar', { name: 'App keys' });
    act(() =>
      emit({ type: VIEW_OPEN_EVENT, kind: 'live', view_id: 'app-1', view: { ...wizardWire(), seq: 3, is_stale: true } } as SseEvent),
    );
    expect(await screen.findByText('Disconnected')).toBeInTheDocument();
    expect(screen.queryByRole('toolbar', { name: 'App keys' })).toBeNull();
    act(() => emit({ type: VIEW_OPEN_EVENT, kind: 'live', view_id: 'app-1', view: { ...wizardWire(), seq: 4 } } as SseEvent));
    expect(await screen.findByText('Live')).toBeInTheDocument();
  });
});

function wizardWire() {
  const held = wizard(3);
  return {
    id: held.id,
    title: held.title,
    placement: 'attachment',
    nodes: [
      { id: 'agree', type: 'checkbox', label: 'I read the notes' },
      { id: 'next', type: 'button', label: 'Next' },
    ],
    shortcuts: [{ key: 'n', node_id: 'next' }],
  };
}

// An engine-like host: it answers each accepted change and submit with set-value
// at a new seq. A submit once sent stale values within one mount (V, A4 b).
type Deploy = { done?: string };

const DEPLOY: StoryApp<Deploy> = {
  id: 'app-1',
  title: 'Deploy',
  initial: () => ({}),
  render: (state) =>
    state.done
      ? { nodes: [{ id: 'done', type: 'status', text: state.done, tone: 'ok' }] }
      : {
          nodes: [
            { id: 'name', type: 'plaintext', label: 'Service name', is_required: true },
            {
              id: 'env',
              type: 'select',
              label: 'Environment',
              is_required: true,
              options: [
                { value: 'staging', label: 'Staging' },
                { value: 'production', label: 'Production' },
              ],
            },
            { id: 'next', type: 'button', label: 'Next', action: 'submit' },
          ],
          shortcuts: [{ key: 'n', node_id: 'next', label: 'Next' }],
        },
  act: (state, action, values) => {
    if (action.action !== 'submit') return { state };
    const missing = ['name', 'env'].filter((id) => !values[id]);
    if (missing.length > 0) {
      return {
        state,
        answer: { refuse: 'invalid', errors: Object.fromEntries(missing.map((id) => [id, 'is required'])) },
      };
    }
    const next = { done: `Deploying ${String(values.name)} to ${String(values.env)}` };
    return { state: next, answer: DEPLOY.render(next) };
  },
};

function hostOpen<S>(host: StoryAppHost<S>) {
  open(host.client, host.hub);
}

/** Wait until the host holds the change of `id` and the view has folded its set-value. */
async function changed<S>(host: StoryAppHost<S>, id: string) {
  await waitFor(() =>
    expect(host.actions.some((action) => action.action === 'change' && action.node_id === id)).toBe(true),
  );
  // Let the answer resolve after its patch frame, as on a real gateway.
  await act(async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
  });
}

async function fillDeploy(host: StoryAppHost<Deploy>) {
  hostOpen(host);
  const name = await screen.findByRole('textbox', { name: /Service name/ });
  fireEvent.change(name, { target: { value: 'sdk-api' } });
  fireEvent.blur(name);
  await changed(host, 'name');
  const staging = screen.getByRole('radio', { name: /Staging/ });
  fireEvent.click(staging);
  staging.focus();
  await changed(host, 'env');
  // The typed values give way to the view's own values, which now hold them.
  expect(host.view.seq).toBeGreaterThanOrEqual(3);
  expect(screen.getByRole('textbox', { name: /Service name/ })).toHaveValue('sdk-api');
  expect(screen.getByRole('radio', { name: /Staging/ })).toBeChecked();
  return staging;
}

describe('AppSurface against an engine-like host', () => {
  it('submits the typed values by key in the same mount', async () => {
    const host = new StoryAppHost(DEPLOY);
    const staging = await fillDeploy(host);
    fireEvent.keyDown(staging, { key: 'n' });
    await screen.findByText('Deploying sdk-api to staging');
    const submit = host.actions.find((action) => action.action === 'submit');
    expect(submit).toMatchObject({ action: 'submit', node_id: 'next', values: { name: 'sdk-api', env: 'staging' } });
  });

  it('shows an owner set-value that clears a field, as a todo list does after Add', async () => {
    const host = new StoryAppHost(TODO, { id: 'app-1' });
    hostOpen(host);
    const field = await screen.findByRole('textbox', { name: /New task/ });
    fireEvent.change(field, { target: { value: 'Buy milk' } });
    fireEvent.blur(field);
    await changed(host, 'new');
    expect(screen.getByRole('textbox', { name: /New task/ })).toHaveValue('Buy milk');
    act(() => host.push([{ op: 'set-value', node_id: 'new', value: null }]));
    await waitFor(() => expect(screen.getByRole('textbox', { name: /New task/ })).toHaveValue(''));
    fireEvent.change(screen.getByRole('textbox', { name: /New task/ }), { target: { value: 'Call Ana' } });
    fireEvent.click(nodeButton('add'));
    await screen.findByText('Call Ana');
    await waitFor(() => expect(screen.getByRole('textbox', { name: /New task/ })).toHaveValue(''));
  });
});

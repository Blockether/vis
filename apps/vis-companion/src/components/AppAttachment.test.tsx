// @vitest-environment jsdom
// Issue #421: an app attachment opens one surface that reads the host state,
// follows the session stream and saves through the view actions.
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { GatewayClient } from '../lib/gateway';
import { GatewayError } from '../lib/gateway';
import { APP_ATTACHMENT_MEDIA, liveViewFromWire, type LiveView } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { IterationAttachment, SseEvent } from '../lib/types';
import { VIEW_PATCH_EVENT, type ViewAction } from '../lib/view';
import { AppAttachmentRow, AppSurface } from './AppAttachment';

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

  it('adds the Send receipt from the answer', async () => {
    const { hub } = fakeHub();
    const { client } = fakeClient([view(3, 'Ship it')], async (action) => ({
      action: action.action,
      view_id: 'app-1',
      is_accepted: true,
      seq: 4,
      submission: { id: 'sub-1', idempotency_key: 'k', node_id: 'review', status: 'accepted', created_at: 1 },
    }));
    open(client, hub);
    fireEvent.click(await screen.findByRole('button', { name: 'Send review' }));
    expect(await screen.findByRole('list', { name: 'Receipts' })).toBeInTheDocument();
    expect(screen.getByText('Accepted')).toBeInTheDocument();
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

  it.each([320, 375, 430, 1280])('keeps the code scrolling inside the surface at %ipx', async (width) => {
    vi.stubGlobal('innerWidth', width);
    const { client } = fakeClient([view(3, 'Draft')], async () => ({}));
    open(client, null);
    const thread = await screen.findByRole('region', { name: 'Thread on src/a.ts line 1' });
    const lines = document.querySelector('[data-diff-lines]');
    expect(lines?.className).toContain('overflow-x-auto');
    expect(lines?.className).toContain('min-w-0');
    expect(thread.closest('[data-diff-lines]')).toBeNull();
    expect(thread.className).toContain('min-w-0');
    expect(document.querySelector('[data-app-surface="app-1"]')?.className).toContain('min-w-0');
  });
});

describe('AppAttachmentRow', () => {
  it('opens the app from its transcript row', async () => {
    const { client, liveView } = fakeClient([view(3)], async () => ({}));
    const attachment: IterationAttachment = {
      iteration_id: 'i1',
      index: 0,
      kind: 'app',
      media_type: APP_ATTACHMENT_MEDIA,
      filename: 'code-review.app.json',
      view_id: 'app-1',
    } as IterationAttachment;
    render(
      <div data-session-surface>
        <AppAttachmentRow client={client} sid="s1" attachment={attachment} />
      </div>,
    );
    expect(liveView).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Open app code-review' }));
    expect(await screen.findByText('Code review')).toBeInTheDocument();
  });
});

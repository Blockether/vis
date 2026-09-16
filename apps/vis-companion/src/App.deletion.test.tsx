// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { renderApp } from './app-harness';
import { GatewayClient } from './lib/gateway';
import { loadOpenSession } from './lib/storage';
import { draftMessageKey, peekDraftMessage } from './lib/draft-messages';
import type { SseEvent } from './lib/types';
import { listSession } from './screens/sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
  vi.restoreAllMocks();
});

// Regression: a remote deletion must leave the affected transcript, not keep a
// composer targeting a session that no longer exists or close an unrelated one.
describe('remotely deleted app conversations', () => {
  it('returns to the list without closing an unrelated conversation', async () => {
    let emit: (event: SseEvent) => void = () => {};
    vi.spyOn(GatewayClient.prototype, 'streamSessionEvents').mockImplementation(
      (_cursors, listener) => {
        emit = listener;
        return vi.fn();
      },
    );
    vi.spyOn(GatewayClient.prototype, 'streamFleetStatus').mockReturnValue(() => {});
    const remove = vi.spyOn(GatewayClient.prototype, 'deleteSession');
    const first = listSession({ id: 's1', title: 'First conversation' });
    const second = listSession({ id: 's2', title: 'Second conversation' });
    const machine = {
      sessions: [first, second],
      routes: { '/v1/sessions/s1': first, '/v1/sessions/s2': second },
    };
    const view = renderApp({ machines: [machine] });
    restore = view.restore;
    fireEvent.click(await screen.findByText('First conversation'));
    const composer = await screen.findByLabelText('Message Vis');
    fireEvent.change(composer, { target: { value: 'Unsent message' } });
    const open = await loadOpenSession();
    expect(open?.sid).toBe('s1');
    const draft = draftMessageKey(open!.url, 's1');
    await waitFor(() => expect(peekDraftMessage(draft).text).toBe('Unsent message'));

    await act(async () => emit({ type: 'session.deleted', session_id: 's2', seq: 1 }));
    expect(screen.getByLabelText('Message Vis')).toBe(composer);
    machine.sessions = [];
    await act(async () => emit({ type: 'session.deleted', session_id: 's1', seq: 1 }));
    await waitFor(() => expect(screen.queryByLabelText('Message Vis')).toBeNull());
    await waitFor(async () => expect(await loadOpenSession()).toBeNull());
    expect(peekDraftMessage(draft).text).toBe('');
    expect(screen.queryByText('First conversation')).toBeNull();
    expect(remove).not.toHaveBeenCalled();
    view.unmount();
  });
});

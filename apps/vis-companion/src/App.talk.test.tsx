// @vitest-environment jsdom
import { act, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

const links = vi.hoisted(() => ({ receive: (_url: string) => {} }));
vi.mock('./lib/deeplink', () => ({
  onPairingLink: async (handler: (url: string) => void) => {
    links.receive = handler;
    return () => {};
  },
}));

import { renderApp } from './app-harness';
import { listSession } from './screens/sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
  vi.restoreAllMocks();
  window.history.replaceState(null, '', '/');
});

const talkLink = (machine: string, session: string, at: number) =>
  `vis://talk?machine=${encodeURIComponent(machine)}&session=${encodeURIComponent(session)}&at=${at}`;

// The `Talk to Vis` App Intent (the Action Button) opens these links.
describe('a Talk link', () => {
  it('opens the named session on the named machine', async () => {
    const view = renderApp({
      machines: [
        { label: 'laptop', sessions: [listSession({ id: 's1', title: 'Session one' })] },
        { label: 'server', sessions: [listSession({ id: 's2', title: 'Session two' })] },
      ],
    });
    restore = view.restore;
    await screen.findByText('Session one');

    await act(async () => {
      links.receive(talkLink(view.conns[1]!.url, 's2', 1));
    });

    expect(await screen.findByLabelText('Message Vis')).toBeInTheDocument();
    await waitFor(() =>
      expect(
        view.requests.some((href) => href.startsWith(`${new URL(view.conns[1]!.url).origin}/v1/sessions/s2`)),
      ).toBe(true),
    );
    view.unmount();
  });

  it('creates a new session on the machine when the link asks for one', async () => {
    const view = renderApp({
      machines: [
        {
          label: 'laptop',
          sessions: [listSession({ id: 's1', title: 'Session one' })],
          routes: { '/v1/sessions': { id: 's-new', sessions: [], has_more: false } },
        },
      ],
    });
    restore = view.restore;
    await waitFor(() => expect(view.requests.some((href) => href.includes('/v1/sessions'))).toBe(true));

    await act(async () => {
      links.receive(talkLink(view.conns[0]!.url, 'new', 2));
    });

    expect(await screen.findByLabelText('Message Vis')).toBeInTheDocument();
    await waitFor(() =>
      expect(
        view.requests.some((href) => href.startsWith(`${new URL(view.conns[0]!.url).origin}/v1/sessions/s-new`)),
      ).toBe(true),
    );
    view.unmount();
  });
});

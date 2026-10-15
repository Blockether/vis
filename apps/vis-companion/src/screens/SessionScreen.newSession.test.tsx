// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { renderSessionScreen, sessionFixture } from './session-screen-harness';

/** A session that works in a draft of the `notes` project, filed in one of its groups. */
const inNotes = () =>
  sessionFixture({
    group_id: 'g-receipts',
    workspace: {
      root: '/Users/dev/.vis/drafts/notes/wire',
      repo_root: '/Users/dev/notes',
      is_draft: true,
    },
  });

async function runNewSession(text: string) {
  const composer = await screen.findByLabelText('Message Vis');
  fireEvent.change(composer, { target: { value: text } });
  fireEvent.click(screen.getByRole('button', { name: 'Send message' }));
}

// Regression, user report, Vis session 2692dc5c-b2fd-4502-b7fa-5fe440a16c1d (paraphrased:
// with two projects added, a new session took its prompt to the first project):
// `/new-session` sent no root, so the gateway started the session in its own launch folder.
describe('/new-session', () => {
  it("starts the new session in this session's project", async () => {
    const createSession = vi.fn(() => Promise.resolve(sessionFixture({ id: 'created' })));
    const submitTurn = vi.fn(() => Promise.resolve(null));
    const onOpenSession = vi.fn();
    renderSessionScreen({ session: inNotes(), client: { createSession, submitTurn }, onOpenSession });

    await runNewSession('/new-session Fix the login');

    await waitFor(() => expect(onOpenSession).toHaveBeenCalledWith('created', true));
    // The project root, not the draft clone. Like the TUI command, it keeps no group.
    expect(createSession).toHaveBeenCalledWith({
      channel: 'web',
      root: '/Users/dev/notes',
      idempotencyKey: expect.any(String),
    });
    expect(submitTurn).toHaveBeenCalledWith('created', 'Fix the login', {
      idempotencyKey: expect.any(String),
    });
  });

  it('reads this session first when its row has not loaded yet', async () => {
    let answer = () => {};
    const loaded = new Promise<void>((resolve) => {
      answer = resolve;
    });
    const createSession = vi.fn(() => Promise.resolve(sessionFixture({ id: 'created' })));
    const onOpenSession = vi.fn();
    renderSessionScreen({
      session: inNotes(),
      client: {
        cachedSession: () => null,
        session: () => loaded.then(inNotes),
        createSession,
      },
      onOpenSession,
    });

    await runNewSession('/new-session');
    answer();

    await waitFor(() => expect(onOpenSession).toHaveBeenCalledWith('created', true));
    expect(createSession).toHaveBeenCalledWith({
      channel: 'web',
      root: '/Users/dev/notes',
      idempotencyKey: expect.any(String),
    });
  });

  // Regression, issue #384: a retry after a failed send made a second session, and the
  // message could reach another session than the one the gateway made for it.
  it('retries into the session that the gateway made, with the same keys', async () => {
    const createSession = vi.fn(() => Promise.resolve(sessionFixture({ id: 'created' })));
    const submitTurn = vi
      .fn()
      .mockRejectedValueOnce(new Error('Gateway is busy'))
      .mockResolvedValueOnce(null);
    const onOpenSession = vi.fn();
    renderSessionScreen({ session: inNotes(), client: { createSession, submitTurn }, onOpenSession });

    await runNewSession('/new-session Fix the login');
    await screen.findByText('Gateway is busy');
    expect(onOpenSession).not.toHaveBeenCalled();

    // The failed request is back in the composer; sending it again is the retry.
    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));

    await waitFor(() => expect(onOpenSession).toHaveBeenCalledWith('created', true));
    expect(createSession).toHaveBeenCalledTimes(1);
    expect(submitTurn).toHaveBeenCalledTimes(2);
    const [first, second] = submitTurn.mock.calls;
    expect(second[0]).toBe('created');
    expect(second[1]).toBe('Fix the login');
    expect(second[2]).toEqual(first[2]);
  });

  it('starts a new attempt with new keys for a different request', async () => {
    const createSession = vi.fn(() => Promise.reject(new Error('Gateway is busy')));
    renderSessionScreen({ session: inNotes(), client: { createSession } });

    await runNewSession('/new-session First idea');
    await screen.findByText('Gateway is busy');
    await runNewSession('/new-session Second idea');
    await waitFor(() => expect(createSession).toHaveBeenCalledTimes(2));

    const keys = createSession.mock.calls.map(
      (call) => (call as unknown as [{ idempotencyKey: string }])[0].idempotencyKey,
    );
    expect(keys[0]).not.toBe(keys[1]);
  });
});

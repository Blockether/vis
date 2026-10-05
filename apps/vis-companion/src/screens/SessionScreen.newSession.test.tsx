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
    expect(createSession).toHaveBeenCalledWith({ channel: 'web', root: '/Users/dev/notes' });
    expect(submitTurn).toHaveBeenCalledWith('created', 'Fix the login');
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
    expect(createSession).toHaveBeenCalledWith({ channel: 'web', root: '/Users/dev/notes' });
  });
});

// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { GatewayError } from '../lib/gateway';
import { renderSessionScreen, sessionFixture } from './session-screen-harness';

const READ_ONLY = 'this session is archived and read-only - unarchive it to keep working';

// User report: a message sent to an archived session showed for half a second, then went
// back into the composer. The gateway refused it, and the reason was out of sight.
describe('an archived session', () => {
  it('takes no message and says why next to the composer', () => {
    const submitTurn = vi.fn();
    renderSessionScreen({ session: sessionFixture({ archived_at: 1717 }), client: { submitTurn } });

    expect(screen.getByLabelText('Message Vis')).toBeDisabled();
    expect(screen.getByRole('button', { name: 'Send message' })).toBeDisabled();
    expect(screen.getByText('Archived · unarchive it to keep working')).toBeInTheDocument();
    expect(submitTurn).not.toHaveBeenCalled();
  });

  it('takes messages again once the reader unarchives it', async () => {
    const setSessionArchived = vi.fn(() => Promise.resolve(sessionFixture({ archived_at: null })));
    renderSessionScreen({
      session: sessionFixture({ archived_at: 1717 }),
      client: { setSessionArchived },
    });

    fireEvent.click(screen.getByRole('button', { name: 'Unarchive' }));

    await waitFor(() => expect(screen.getByLabelText('Message Vis')).toBeEnabled());
    expect(setSessionArchived).toHaveBeenCalledWith('s1', false);
    expect(screen.queryByText('Archived · unarchive it to keep working')).not.toBeInTheDocument();
  });

  it('shows the refusal next to the composer when only its group is archived', async () => {
    const submitTurn = vi.fn(() =>
      Promise.reject(
        new GatewayError(409, READ_ONLY, { error: { type: 'session-archived', message: READ_ONLY } }),
      ),
    );
    renderSessionScreen({ client: { submitTurn } });
    const composer = screen.getByLabelText('Message Vis') as HTMLTextAreaElement;
    fireEvent.input(composer, { target: { value: 'Keep going' } });
    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));

    const notice = await screen.findByText(READ_ONLY);
    expect(notice.closest('[role="status"]')).toBeNull();
    await waitFor(() => expect(composer.value).toBe('Keep going'));
  });
});

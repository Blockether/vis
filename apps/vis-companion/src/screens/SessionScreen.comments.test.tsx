// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import type { LiveView } from '../lib/live-view';
import { renderSessionScreen } from './session-screen-harness';

// A comment on a diff line is a draft of the app. The composer shows it, and the
// next message of the session carries it, even a message without text.
const review: LiveView = {
  id: 'app-1',
  title: 'Review',
  nodes: [],
  seq: 3,
  placement: 'attachment',
  is_summary: true,
  drafts: [
    {
      node_id: 'review',
      thread_id: 't1',
      body: 'Rename this',
      anchor: { path: 'src/a.clj', side: 'new', line: 4 },
    },
  ],
};

describe('review comments in the composer', () => {
  it('sends the saved comments with the next message and then hides them', async () => {
    const submitTurn = vi.fn(() => Promise.resolve({ turn_id: 't-comments', status: 'running' }));
    renderSessionScreen({
      client: { submitTurn, liveViews: () => Promise.resolve([review]) },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });
    // The marker of the comment stands in the message, where the human can move it.
    expect(await screen.findByText('[COMMENT a.clj:4]')).toBeInTheDocument();
    expect(screen.getByText('Rename this')).toBeInTheDocument();
    const editor = screen.getByLabelText('Message Vis') as HTMLTextAreaElement;
    await waitFor(() => expect(editor.value).toBe('[COMMENT a.clj:4]'));
    fireEvent.change(editor, { target: { value: 'Look at [COMMENT a.clj:4] first' } });

    fireEvent.click(screen.getByRole('button', { name: 'Send message' }));

    await waitFor(() =>
      expect(submitTurn).toHaveBeenCalledWith(
        expect.any(String),
        'Look at [COMMENT a.clj:4] first',
        expect.objectContaining({
          comments: [
            { view_id: 'app-1', node_id: 'review', thread_id: 't1', reference: '[COMMENT a.clj:4]' },
          ],
        }),
      ),
    );
    await waitFor(() => expect(screen.queryByText('Rename this')).toBeNull());
  });

  it('removes the marker from the message when the comment is removed', async () => {
    const viewAction = vi.fn(() => Promise.resolve({}));
    renderSessionScreen({
      client: { viewAction, liveViews: () => Promise.resolve([review]) },
    });
    const editor = screen.getByLabelText('Message Vis') as HTMLTextAreaElement;
    await waitFor(() => expect(editor.value).toBe('[COMMENT a.clj:4]'));
    fireEvent.change(editor, { target: { value: 'Before [COMMENT a.clj:4] after' } });

    fireEvent.click(screen.getByRole('button', { name: 'Remove comment on src/a.clj:4' }));

    expect(editor.value).toBe('Before after');
    expect(viewAction).toHaveBeenCalledWith(
      expect.any(String),
      'app-1',
      expect.objectContaining({ action: 'draft', node_id: 'review', thread_id: 't1', body: null }),
    );
  });
});

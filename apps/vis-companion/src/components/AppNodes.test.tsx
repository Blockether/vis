// @vitest-environment jsdom
// Issue #421: the review nodes of an app attachment. A diff line opens a thread,
// a draft saves only with the host acknowledgement, and Send reuses one
// idempotency key until the host answers.
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { AppActionResult } from '../lib/app-actions';
import type { LiveDiffNode } from '../lib/live-view';
import { DiffNodeView, type AppNodeActions } from './AppNodes';

afterEach(cleanup);

const DIFF: LiveDiffNode = {
  id: 'review',
  type: 'diff',
  label: 'Review',
  patch: '--- a/a.txt\n+++ b/a.txt\n@@ -1,2 +1,2 @@\n keep\n-old\n+new',
  old_revision: 'aaa',
  new_revision: 'bbb',
  threads: [
    {
      id: 't1',
      anchor: { path: 'a.txt', side: 'new', line: 2 },
      messages: [{ id: 'm1', author: { kind: 'agent' }, body: 'Renamed it.', created_at: 1 }],
      draft: 'Looks fine',
    },
  ],
};

function actions(overrides: Partial<AppNodeActions> = {}): AppNodeActions {
  return {
    isReadOnly: false,
    submissions: [],
    draft: vi.fn(async (): Promise<AppActionResult> => ({ status: 'saved', seq: 5 })),
    send: vi.fn(async (): Promise<AppActionResult> => ({ status: 'saved', seq: 6 })),
    ...overrides,
  };
}

describe('DiffNodeView', () => {
  it('opens an editor on the line that a keyboard activates, on that line side', async () => {
    const app = actions();
    render(<DiffNodeView node={DIFF} actions={app} />);
    const removed = screen.getByRole('button', { name: 'Comment on a.txt old line 2' });
    removed.focus();
    fireEvent.click(removed);
    const editor = screen.getByRole('textbox', { name: 'Comment on a.txt line 2' });
    fireEvent.change(editor, { target: { value: 'Why remove it?' } });
    fireEvent.click(screen.getByRole('button', { name: 'Save' }));
    await waitFor(() =>
      expect(app.draft).toHaveBeenCalledWith('review', {
        anchor: { path: 'a.txt', side: 'old', line: 2 },
        body: 'Why remove it?',
      }),
    );
    expect(await screen.findByText('Saved')).toBeInTheDocument();
  });

  it('creates nothing for a blank comment on a new line', () => {
    const app = actions();
    render(<DiffNodeView node={DIFF} actions={app} />);
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 1' }));
    fireEvent.click(screen.getByRole('button', { name: 'Save' }));
    expect(app.draft).not.toHaveBeenCalled();
    expect(screen.queryByRole('textbox')).toBeNull();
  });

  it('keeps the history and the saved draft on Cancel', () => {
    render(<DiffNodeView node={DIFF} actions={actions()} />);
    fireEvent.click(screen.getByRole('button', { name: 'Edit draft' }));
    fireEvent.change(screen.getByRole('textbox', { name: 'Edit draft' }), {
      target: { value: 'changed' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(screen.getByText('Renamed it.')).toBeInTheDocument();
    expect(screen.getByText('Looks fine')).toBeInTheDocument();
  });

  it('is read only without app actions and when the app is closed', () => {
    const { unmount } = render(<DiffNodeView node={DIFF} />);
    expect(screen.queryByRole('button')).toBeNull();
    unmount();
    render(<DiffNodeView node={DIFF} actions={actions({ isReadOnly: true })} />);
    expect(screen.queryByRole('button', { name: /Comment on/ })).toBeNull();
    expect(screen.getByRole('button', { name: 'Send Review' })).toHaveProperty('disabled', true);
  });
});

describe('Save states', () => {
  it.each([
    ['offline', 'Offline'],
    ['conflict', 'Changed elsewhere'],
    ['stale', 'New revision'],
    ['failed', 'Failed'],
  ] as const)('shows %s and never Saved', async (status, word) => {
    let finish: (result: AppActionResult) => void = () => undefined;
    const app = actions({
      draft: vi.fn(() => new Promise<AppActionResult>((resolve) => (finish = resolve))),
    });
    render(<DiffNodeView node={DIFF} actions={app} />);
    fireEvent.click(screen.getByRole('button', { name: 'Edit draft' }));
    fireEvent.click(screen.getByRole('button', { name: 'Save' }));
    expect(await screen.findByText('Saving')).toBeInTheDocument();
    await act(async () => finish({ status }));
    expect(screen.getByText(word)).toBeInTheDocument();
    expect(screen.queryByText('Saved')).toBeNull();
  });
});

describe('Send', () => {
  it('reuses one idempotency key on retry and shows the receipts', async () => {
    const results: AppActionResult[] = [{ status: 'offline' }, { status: 'saved', seq: 7 }];
    const app = actions({
      send: vi.fn(async (): Promise<AppActionResult> => results.shift() ?? { status: 'saved' }),
    });
    const { rerender } = render(<DiffNodeView node={DIFF} actions={app} />);
    fireEvent.click(screen.getByRole('button', { name: 'Send Review' }));
    expect(await screen.findByText('Offline')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Send Review' }));
    await waitFor(() => expect(app.send).toHaveBeenCalledTimes(2));
    const [first, second] = vi.mocked(app.send).mock.calls;
    expect(first[1]).toBe(second[1]);
    rerender(
      <DiffNodeView
        node={DIFF}
        actions={{
          ...app,
          submissions: [
            {
              id: 's1',
              idempotency_key: first[1],
              node_id: 'review',
              status: 'answered',
              created_at: 1,
              updated_at: 2,
              comments: [{ node_id: 'review', thread_id: 't1', message_id: 'm2', body: 'x' }],
            },
          ],
        }}
      />,
    );
    expect(screen.getByRole('list', { name: 'Receipts' }).textContent).toContain(
      'Answered · 1 comment',
    );
  });
});

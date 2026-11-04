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
      messages: [
        {
          id: 'm1',
          author: { kind: 'agent' },
          body: 'Renamed it.',
          created_at: 1,
        },
      ],
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

function openLine(button: HTMLElement) {
  fireEvent.click(button);
  fireEvent.click(screen.getByRole('button', { name: 'Add comment' }));
}

describe('DiffNodeView', () => {
  it('opens an editor on the line that a keyboard activates, on that line side', async () => {
    const app = actions();
    render(<DiffNodeView node={DIFF} actions={app} />);
    const removed = screen.getByRole('button', {
      name: 'Comment on a.txt old line 2',
    });
    removed.focus();
    openLine(removed);
    const editor = screen.getByRole('textbox', {
      name: 'Comment on a.txt line 2',
    });
    fireEvent.change(editor, { target: { value: 'Why remove it?' } });
    fireEvent.click(screen.getByRole('button', { name: 'Comment' }));
    await waitFor(() =>
      expect(app.draft).toHaveBeenCalledWith('review', {
        anchor: { path: 'a.txt', side: 'old', line: 2 },
        body: 'Why remove it?',
      }),
    );
    await waitFor(() => expect(screen.queryByRole('textbox')).toBeNull());
  });

  it('creates nothing for a blank comment on a new line', () => {
    const app = actions();
    render(<DiffNodeView node={DIFF} actions={app} />);
    openLine(screen.getByRole('button', { name: 'Comment on a.txt line 1' }));
    expect(screen.getByRole('button', { name: 'Comment' })).toBeDisabled();
    expect(app.draft).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
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

  // The review parity report requires split mode to preserve comment anchors.
  it('pairs changed lines in split mode and saves a comment on the old side', async () => {
    const app = actions();
    render(<DiffNodeView node={DIFF} actions={app} />);
    fireEvent.click(screen.getByRole('button', { name: 'Split' }));
    const old = screen.getByRole('button', {
      name: 'Comment on a.txt old line 2',
    });
    expect(old.closest('tr')).toHaveTextContent('old');
    expect(old.closest('tr')).toHaveTextContent('new');
    openLine(old);
    fireEvent.change(screen.getByRole('textbox', { name: 'Comment on a.txt line 2' }), {
      target: { value: 'Keep this?' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Comment' }));
    await waitFor(() =>
      expect(app.draft).toHaveBeenCalledWith('review', {
        anchor: { path: 'a.txt', side: 'old', line: 2 },
        body: 'Keep this?',
      }),
    );
  });

  // Split mode must not repeat old-side context threads below the diff.
  it('shows an old-side context thread once in either layout', () => {
    const node: LiveDiffNode = {
      ...DIFF,
      threads: [
        {
          id: 'old-context',
          anchor: { path: 'a.txt', side: 'old', line: 1 },
          messages: [
            {
              id: 'context-message',
              author: { kind: 'agent' },
              body: 'Keep this context.',
              created_at: 1,
            },
          ],
        },
      ],
    };
    render(<DiffNodeView node={node} />);
    expect(screen.getAllByText('Keep this context.')).toHaveLength(1);
    fireEvent.click(screen.getByRole('button', { name: 'Split' }));
    expect(screen.getAllByText('Keep this context.')).toHaveLength(1);
  });

  // T24 user report: a picture or a binary file has no lines to count.
  it('shows line counts for a text file but not for a binary file', () => {
    const patch = [
      'diff --git a/logo.png b/logo.png',
      'index 8fc1a2b..4de6e13 100644',
      'Binary files a/logo.png and b/logo.png differ',
      'diff --git a/a.txt b/a.txt',
      '--- a/a.txt',
      '+++ b/a.txt',
      '@@ -1 +1 @@',
      '-old',
      '+new',
    ].join('\n');
    render(<DiffNodeView node={{ id: 'review', type: 'diff', patch, threads: [] }} />);
    const header = (path: string) => screen.getByRole('button', { name: `Fold ${path}` }).closest('header');
    expect(header('logo.png')?.querySelector('.diff-stat')).toBeNull();
    expect(header('a.txt')?.querySelector('.diff-stat')).toHaveTextContent('-1+1');
  });

  // The review parity report requires the file tree to open folded files.
  it('unfolds a file selected from the tree after Collapse all', () => {
    const { container } = render(<DiffNodeView node={DIFF} />);
    fireEvent.click(screen.getByRole('button', { name: 'Files' }));
    fireEvent.click(screen.getByRole('button', { name: 'Collapse all' }));
    expect(screen.queryByText('old')).toBeNull();
    const scroll = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = vi.fn();
    fireEvent.click(screen.getByRole('button', { name: 'Open a.txt' }));
    Element.prototype.scrollIntoView = scroll;
    expect(container.querySelector('[data-diff-scroll]')).toHaveTextContent('old');
    expect(screen.getByRole('button', { name: 'Fold a.txt' })).toHaveAttribute('aria-expanded', 'true');
  });

  // The review app moved its scope and file pickers into the diff toolbar: a menu
  // item must press the button it names, never the group that holds it.
  it('presses the button that a toolbar menu item names', async () => {
    const onActivate = vi.fn(async () => {});
    const button = (id: string, label: string) => ({
      id,
      type: 'button' as const,
      label,
      action: 'activate' as const,
      variant: 'secondary' as const,
      is_disabled: false,
      clicks: 0,
    });
    const node: LiveDiffNode = {
      ...DIFF,
      children: [
        {
          id: 'scope',
          type: 'group',
          label: 'This session',
          direction: 'row',
          children: [button('session', 'This session'), button('checkpoint-2', 'Checkpoint #2')],
        },
        button('refresh', 'Refresh'),
      ],
    };
    render(<DiffNodeView node={node} commands={{ onActivate, isSettled: false }} />);
    fireEvent.click(screen.getByRole('button', { name: 'This session' }));
    fireEvent.click(screen.getByRole('menuitem', { name: 'Checkpoint #2' }));
    await waitFor(() => expect(onActivate).toHaveBeenCalledWith('checkpoint-2'));
    expect(screen.queryByRole('dialog', { name: 'This session' })).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Refresh' }));
    await waitFor(() => expect(onActivate).toHaveBeenLastCalledWith('refresh'));
    expect(onActivate).toHaveBeenCalledTimes(2);
  });

  // User report 2026-11-09: the review toolbar must work like the T3 Code diff panel.
  // A nested group opens a submenu, the choice that holds is checked, and a command
  // drawn as a picture keeps its label as its name.
  it('opens a toolbar submenu, checks the choice that holds, and names picture commands', async () => {
    const onActivate = vi.fn(async () => {});
    const button = (id: string, label: string, more: { is_selected?: boolean; icon?: 'refresh' } = {}) => ({
      id,
      type: 'button' as const,
      label,
      action: 'activate' as const,
      variant: 'secondary' as const,
      is_disabled: false,
      clicks: 0,
      ...more,
    });
    const node: LiveDiffNode = {
      ...DIFF,
      children: [
        {
          id: 'scope',
          type: 'group',
          label: 'Uncommitted',
          direction: 'column',
          children: [
            button('all', 'All changes', { is_selected: false }),
            button('uncommitted', 'Uncommitted', { is_selected: true }),
            {
              id: 'checkpoints',
              type: 'group',
              label: 'Checkpoint',
              direction: 'column',
              children: [button('checkpoint-2', '#2 edit', { is_selected: false })],
            },
          ],
        },
        button('refresh', 'Refresh diff', { icon: 'refresh' }),
      ],
    };
    render(<DiffNodeView node={node} commands={{ onActivate, isSettled: false }} />);
    fireEvent.click(screen.getByRole('button', { name: 'Uncommitted' }));
    expect(screen.getByRole('menuitemradio', { name: 'Uncommitted' })).toHaveAttribute('aria-checked', 'true');
    expect(screen.getByRole('menuitemradio', { name: 'All changes' })).toHaveAttribute('aria-checked', 'false');
    fireEvent.click(screen.getByRole('menuitem', { name: 'Checkpoint' }));
    fireEvent.click(await screen.findByRole('menuitemradio', { name: '#2 edit' }));
    await waitFor(() => expect(onActivate).toHaveBeenCalledWith('checkpoint-2'));
    expect(screen.queryByRole('menuitemradio')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Refresh diff' }));
    await waitFor(() => expect(onActivate).toHaveBeenLastCalledWith('refresh'));
  });

  it('is read only without app actions and when the app is closed', () => {
    const { unmount } = render(<DiffNodeView node={DIFF} />);
    expect(screen.queryByRole('button', { name: /Comment on/ })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Edit draft' })).toBeNull();
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
              comments: [
                {
                  node_id: 'review',
                  thread_id: 't1',
                  message_id: 'm2',
                  body: 'x',
                },
              ],
            },
          ],
        }}
      />,
    );
    expect(screen.getByRole('list', { name: 'Receipts' }).textContent).toContain('Answered · 1 comment');
  });
});

// Report: select review lines by click and prevent repeated keyboard submissions.
describe('selected line comments', () => {
  it('selects a line before opening its editor', () => {
    render(<DiffNodeView node={DIFF} actions={actions()} />);
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 1' }));
    expect(screen.queryByRole('textbox')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Add comment' }));
    expect(screen.getByRole('textbox', { name: 'Comment on a.txt line 1' })).toBeInTheDocument();
  });
  it('submits a nonblank comment once while a save is pending', async () => {
    let finish!: (r: AppActionResult) => void;
    const app = actions({
      draft: vi.fn(
        () =>
          new Promise<AppActionResult>((resolve) => {
            finish = resolve;
          }),
      ),
    });
    render(<DiffNodeView node={DIFF} actions={app} />);
    openLine(screen.getByRole('button', { name: 'Comment on a.txt line 1' }));
    const editor = screen.getByRole('textbox');
    fireEvent.keyDown(editor, { key: 'Enter', ctrlKey: true });
    expect(app.draft).not.toHaveBeenCalled();
    fireEvent.change(editor, { target: { value: 'Explain this' } });
    fireEvent.keyDown(editor, { key: 'Enter', metaKey: true });
    fireEvent.keyDown(editor, { key: 'Enter', ctrlKey: true });
    expect(app.draft).toHaveBeenCalledTimes(1);
    await act(async () => finish({ status: 'saved', seq: 5 }));
  });
});

// Report: searching a folded diff must reveal the result and return focus on Escape.
it('opens search results in folded files and returns focus when search closes', () => {
  const scroll = Element.prototype.scrollIntoView;
  Element.prototype.scrollIntoView = vi.fn();
  try {
    const { container } = render(<DiffNodeView node={DIFF} />);
    fireEvent.click(screen.getByRole('button', { name: 'Collapse all' }));
    const diff = container.querySelector('.app-diff')!;
    fireEvent.keyDown(diff, { key: 'f', ctrlKey: true });
    const input = screen.getByRole('textbox', { name: 'Find in diff' });
    fireEvent.change(input, { target: { value: 'e' } });
    expect(screen.getByText('1 of 3')).toBeInTheDocument();
    expect(container.querySelector('[data-search-current]')).toHaveTextContent('keep');
    fireEvent.click(screen.getByRole('button', { name: 'Next match' }));
    expect(screen.getByText('2 of 3')).toBeInTheDocument();
    expect(container.querySelector('[data-search-current]')).toHaveTextContent('keep');
    fireEvent.click(screen.getByRole('button', { name: 'Next match' }));
    expect(container.querySelector('[data-search-current]')).toHaveTextContent('new');
    fireEvent.keyDown(input, { key: 'Enter', shiftKey: true });
    expect(screen.getByText('2 of 3')).toBeInTheDocument();
    fireEvent.keyDown(input, { key: 'g', ctrlKey: true });
    expect(screen.getByText('3 of 3')).toBeInTheDocument();
    fireEvent.keyDown(input, { key: 'g', metaKey: true, shiftKey: true });
    expect(screen.getByText('2 of 3')).toBeInTheDocument();
    fireEvent.keyDown(input, { key: 'Escape' });
    expect(screen.queryByRole('search')).toBeNull();
    expect(document.activeElement).toBe(diff);
  } finally {
    Element.prototype.scrollIntoView = scroll;
  }
});

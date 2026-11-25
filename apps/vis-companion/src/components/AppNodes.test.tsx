// @vitest-environment jsdom
// Issue #421: the review nodes of an app attachment. A diff line opens a thread,
// a draft saves only with the host acknowledgement, and the next message of the
// session adds the saved drafts.
import { act, cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { AppActionResult } from '../lib/app-actions';
import type { LiveDiffNode } from '../lib/live-view';
import { DiffNodeView, DraftEditor, type AppNodeActions } from './AppNodes';

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
    draft: vi.fn(async (): Promise<AppActionResult> => ({ status: 'saved', seq: 5 })),
    ...overrides,
  };
}

function openLine(button: HTMLElement) {
  fireEvent.click(button);
  fireEvent.click(screen.getByRole('button', { name: 'Add comment' }));
}

it.each([
  ['\u00a0', '\u00a0'],
  ['\u001c', ''],
])('saves a reply of %j as %j under the gateway whitespace rules', async (body, expected) => {
  const save = vi.fn(async (): Promise<AppActionResult> => ({ status: 'saved', seq: 1 }));
  render(<DraftEditor label="Reply" saved="" memoryId="reply-whitespace" isBlankAllowed onSave={save} onClose={() => {}} />);
  fireEvent.change(screen.getByRole('textbox', { name: 'Reply' }), { target: { value: body } });
  await act(async () => fireEvent.click(screen.getByRole('button', { name: 'Save' })));
  expect(save).toHaveBeenCalledExactlyOnceWith(expected);
});

it('saves all emoji at the draft limit and refuses a longer draft', async () => {
  const save = vi.fn(async (): Promise<AppActionResult> => ({ status: 'saved', seq: 1 }));
  const body = '😀'.repeat(4000);
  render(<DraftEditor label="Reply" saved="" memoryId="emoji-limit" maxLength={4000} onSave={save} onClose={() => {}} />);
  const editor = screen.getByRole('textbox', { name: 'Reply' });
  fireEvent.change(editor, { target: { value: `${body}a` } });
  fireEvent.keyDown(editor, { key: 'Enter', ctrlKey: true });
  expect(save).not.toHaveBeenCalled();
  fireEvent.change(editor, { target: { value: body } });
  fireEvent.click(screen.getByRole('button', { name: 'Save' }));
  await waitFor(() => expect(save).toHaveBeenCalledExactlyOnceWith(body));
});

describe('DiffNodeView', () => {
  it('shows both versions under the binary file header and hides them when folded', () => {
    const sound = {
      id: 'chime', type: 'media', kind: 'audio', alt: 'Chime',
      url: 'https://example.com/after.wav', before: { url: 'https://example.com/before.wav' },
    } as const;
    const node: LiveDiffNode = {
      ...DIFF,
      id: 'review-binary',
      patch: `diff --git a/a.txt b/a.txt\n${DIFF.patch}\ndiff --git a/chime.wav b/chime.wav\nBinary files a/chime.wav and b/chime.wav differ`,
      previews: [{ path: 'chime.wav', node: sound }, { path: 'missing.wav', node: { ...sound, alt: 'Missing' } }],
    };
    render(<DiffNodeView node={node} />);
    const file = within(screen.getByRole('region', { name: 'chime.wav' }));
    const header = file.getByRole('button', { name: 'Fold chime.wav' });
    const before = file.getByRole('group', { name: 'Before: Chime' });
    expect(header.compareDocumentPosition(before) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(file.getByRole('group', { name: 'After: Chime' })).toBeInTheDocument();
    expect(screen.queryByRole('group', { name: 'After: Missing' })).not.toBeInTheDocument();
    fireEvent.click(header);
    expect(file.queryByRole('group', { name: 'Before: Chime' })).not.toBeInTheDocument();
    expect(file.queryByRole('group', { name: 'After: Chime' })).not.toBeInTheDocument();
    fireEvent.click(file.getByRole('button', { name: 'Unfold chime.wav' }));
    expect(file.getByRole('group', { name: 'After: Chime' })).toBeInTheDocument();
  });

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
    fireEvent.click(screen.getByRole('button', { name: 'Edit note' }));
    fireEvent.change(screen.getByRole('textbox', { name: 'Note' }), {
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
    const header = (path: string) => screen.getByRole('button', { name: `Copy path ${path}` }).closest('header');
    expect(header('logo.png')?.querySelector('.diff-stat')).toBeNull();
    expect(header('a.txt')?.querySelector('.diff-stat')).toHaveTextContent('-1+1');
  });

  // User report: an empty new file has no lines, so its header offers no fold.
  it('gives a file without hunks no fold button', () => {
    const patch = [
      'diff --git a/tests/__init__.py b/tests/__init__.py',
      'new file mode 100644',
      'index 0000000..e69de29',
      'diff --git a/a.txt b/a.txt',
      '--- a/a.txt',
      '+++ b/a.txt',
      '@@ -1 +1 @@',
      '-old',
      '+new',
    ].join('\n');
    render(<DiffNodeView node={{ id: 'review', type: 'diff', patch, threads: [] }} />);
    expect(screen.queryByRole('button', { name: /Fold tests\/__init__\.py/ })).toBeNull();
    expect(screen.getByRole('button', { name: 'Copy path tests/__init__.py' })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Fold a.txt' })).toHaveAttribute('aria-expanded', 'true');
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
    expect(screen.queryByRole('button', { name: 'Edit note' })).toBeNull();
    unmount();
    render(<DiffNodeView node={DIFF} actions={actions({ isReadOnly: true })} />);
    expect(screen.queryByRole('button', { name: /Comment on/ })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Remove' })).toBeNull();
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
    fireEvent.click(screen.getByRole('button', { name: 'Edit note' }));
    fireEvent.click(screen.getByRole('button', { name: 'Save' }));
    expect(await screen.findByText('Saving')).toBeInTheDocument();
    await act(async () => finish({ status }));
    expect(screen.getByText(word)).toBeInTheDocument();
    expect(screen.queryByText('Saved')).toBeNull();
  });
});

describe('comments for the next message', () => {
  const finding = { ...DIFF.threads[0], draft: undefined };

  it('adds a finding to the next message without a note', async () => {
    const app = actions();
    render(<DiffNodeView node={{ ...DIFF, threads: [finding] }} actions={app} />);
    expect(screen.queryByRole('button', { name: /^Send/ })).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Add to message' }));
    // The editor replaces the button; its own submit button has the same name.
    fireEvent.click(screen.getByRole('button', { name: 'Add to message' }));
    await waitFor(() => expect(app.draft).toHaveBeenCalledWith('review', { threadId: 't1', body: '' }));
  });

  it('removes a saved comment', async () => {
    const app = actions();
    render(<DiffNodeView node={DIFF} actions={app} />);
    expect(screen.getByText('Looks fine')).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Remove' }));
    await waitFor(() => expect(app.draft).toHaveBeenCalledWith('review', { threadId: 't1', body: null }));
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
  // User request: like T3 Code, a pointer adds a comment from the control beside the line.
  it('opens an editor from the control on a line that is not selected', () => {
    const { container } = render(<DiffNodeView node={DIFF} actions={actions()} />);
    const controls = () => container.querySelectorAll('.diff-add-comment[data-hover-only]');
    expect(controls().length).toBeGreaterThan(1);
    fireEvent.click(controls()[0]);
    expect(screen.getByRole('textbox', { name: 'Comment on a.txt line 1' })).toBeInTheDocument();
    // While the editor is open, no other line offers a second one.
    expect(controls()).toHaveLength(0);
  });

  const LINES: LiveDiffNode = {
    ...DIFF,
    patch: '--- a/a.txt\n+++ b/a.txt\n@@ -1,1 +1,4 @@\n keep\n+one\n+two\n+three',
    threads: [],
  };

  it('saves one comment on the lines from a picked line to a Shift click', async () => {
    const app = actions();
    render(<DiffNodeView node={LINES} actions={app} />);
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 4' }));
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 2' }), { shiftKey: true });
    fireEvent.click(screen.getByRole('button', { name: 'Add comment' }));
    fireEvent.change(screen.getByRole('textbox', { name: 'Comment on a.txt lines 2–4' }), {
      target: { value: 'Join these.' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Comment' }));
    await waitFor(() =>
      expect(app.draft).toHaveBeenCalledWith('review', {
        anchor: { path: 'a.txt', side: 'new', start_line: 2, line: 4 },
        body: 'Join these.',
      }),
    );
  });

  it('selects the lines that a drag over the numbers reaches', async () => {
    const { container } = render(<DiffNodeView node={LINES} actions={actions()} />);
    const number = (line: number) => screen.getByRole('button', { name: `Comment on a.txt line ${line}` });
    fireEvent.pointerDown(number(3), { button: 0, pointerType: 'mouse' });
    fireEvent.pointerMove(number(1));
    fireEvent.pointerUp(number(1));
    expect(container.querySelectorAll('tr[data-selected-line="range"]')).toHaveLength(3);
    // The click that ends the press belongs to the drag; a later click is its own.
    await act(() => new Promise((done) => setTimeout(done)));
    // The control on any line of the selection comments on all of it.
    fireEvent.click(container.querySelectorAll('.diff-add-comment[data-hover-only]')[0]);
    expect(screen.getByRole('textbox', { name: 'Comment on a.txt lines 1–3' })).toBeInTheDocument();
  });

  // User report: a range over added lines left the removed lines between them unmarked.
  it('saves one comment from one removed line to an added line, without the other removed lines', async () => {
    const app = actions();
    const node = { ...DIFF, threads: [], patch: '--- a/a.txt\n+++ b/a.txt\n@@ -1,4 +1,3 @@\n keep\n-gone\n-lost\n+one\n tail' };
    const { container } = render(<DiffNodeView node={node} actions={app} />);
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt old line 3' }));
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 2' }), { shiftKey: true });
    const marked = [...container.querySelectorAll('tr[data-selected-line]')].map((row) => row.textContent);
    expect(marked).toEqual(['3Removed: lost', '2Added: one']);
    fireEvent.click(screen.getByRole('button', { name: 'Add comment' }));
    fireEvent.change(screen.getByRole('textbox', { name: 'Comment on a.txt lines -3 to +2' }), {
      target: { value: 'Swap.' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Comment' }));
    await waitFor(() =>
      expect(app.draft).toHaveBeenCalledWith('review', {
        anchor: { path: 'a.txt', side: 'new', start_side: 'old', start_line: 3, line: 2 },
        body: 'Swap.',
      }),
    );
  });

  it('marks the removed lines that stand between the selected lines', () => {
    const node = { ...DIFF, threads: [], patch: '--- a/a.txt\n+++ b/a.txt\n@@ -1,4 +1,3 @@\n keep\n-gone\n+one\n tail\n-after' };
    const { container } = render(<DiffNodeView node={node} actions={actions()} />);
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 1' }));
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 3' }), { shiftKey: true });
    const marked = [...container.querySelectorAll('tr[data-selected-line]')].map((row) => row.textContent);
    expect(marked).toEqual(['1keep', '2Removed: gone', '2Added: one', '3tail']);
  });

  it('clears the selected lines when the editor is cancelled', () => {
    const { container } = render(<DiffNodeView node={LINES} actions={actions()} />);
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 2' }));
    fireEvent.click(screen.getByRole('button', { name: 'Comment on a.txt line 4' }), { shiftKey: true });
    fireEvent.click(screen.getByRole('button', { name: 'Add comment' }));
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(screen.queryByRole('textbox')).toBeNull();
    expect(container.querySelectorAll('tr[data-selected-line]')).toHaveLength(0);
  });

  it('opens one editor on the lines that a drag from the control reaches, without Shift', () => {
    const { container } = render(<DiffNodeView node={LINES} actions={actions()} />);
    const control = (line: number) =>
      screen.getByRole('button', { name: `Comment on a.txt line ${line}` }).parentElement!.querySelector('.diff-add-comment')!;
    fireEvent.pointerDown(control(2), { button: 0, pointerType: 'mouse' });
    fireEvent.pointerMove(control(4).closest('tr')!.querySelector('.diff-code') ?? control(4));
    fireEvent.pointerUp(control(4));
    expect(container.querySelectorAll('tr[data-selected-line="range"]')).toHaveLength(3);
    expect(screen.getByRole('textbox', { name: 'Comment on a.txt lines 2–4' })).toBeInTheDocument();
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

/** A diff whose first file fills the eager lines, so `far.txt` waits for the viewport. */
function longDiff(length = 600): LiveDiffNode {
  const lines = Array.from({ length }, (_, at) => `+line ${at}`).join('\n');
  return {
    ...DIFF,
    threads: [],
    patch: `diff --git a/near.txt b/near.txt\n--- a/near.txt\n+++ b/near.txt\n@@ -0,0 +1,${length} @@\n${lines}\ndiff --git a/far.txt b/far.txt\n--- a/far.txt\n+++ b/far.txt\n@@ -1 +1 @@\n-distant\n+remote`,
  };
}

function screenWidth(isWide: boolean) {
  vi.stubGlobal('matchMedia', (query: string) => ({
    matches: isWide && query.includes('min-width: 48rem'),
    media: query,
    addEventListener: () => {},
    removeEventListener: () => {},
  }));
}

/** The observer of the diff, driven by the test: jsdom has no layout. */
function stubViewport() {
  let notify!: IntersectionObserverCallback;
  vi.stubGlobal(
    'IntersectionObserver',
    class {
      constructor(callback: IntersectionObserverCallback) {
        notify = callback;
      }
      observe() {}
      disconnect() {}
    },
  );
  return (target: Element, isIntersecting: boolean, height = 0) =>
    act(() =>
      notify(
        [{ target, isIntersecting, boundingClientRect: { height } } as IntersectionObserverEntry],
        {} as IntersectionObserver,
      ),
    );
}

// Report: a review of 19,000 lines took seconds to open. It mounted every row at once.
describe('a long diff', () => {
  afterEach(() => vi.unstubAllGlobals());

  it('mounts the rows of a far file only near the viewport and keeps its height after', () => {
    const show = stubViewport();
    render(<DiffNodeView node={longDiff()} />);
    const far = screen.getByRole('region', { name: 'far.txt' });
    expect(screen.getByText('line 0')).toBeInTheDocument();
    expect(far.querySelector('[data-diff-scroll]')).toBeNull();
    expect(far.style.minHeight).not.toBe('');
    show(far, true);
    expect(far.querySelector('[data-diff-scroll]')).toHaveTextContent('remote');
    expect(far.style.minHeight).toBe('');
    show(far, false, 321);
    expect(far.querySelector('[data-diff-scroll]')).toBeNull();
    expect(far.style.minHeight).toBe('321px');
  });

  // Report: a review opened slowly on a phone. It mounted 600 lines, and the screen shows about 45.
  it('mounts fewer lines at once on a phone than on a wide screen', () => {
    stubViewport();
    screenWidth(false);
    const phone = render(<DiffNodeView node={longDiff(200)} />);
    expect(screen.getByRole('region', { name: 'far.txt' }).querySelector('[data-diff-scroll]')).toBeNull();
    phone.unmount();
    screenWidth(true);
    render(<DiffNodeView node={longDiff(200)} />);
    expect(screen.getByRole('region', { name: 'far.txt' }).querySelector('[data-diff-scroll]')).toHaveTextContent('remote');
  });

  it('mounts the far file of a search match', () => {
    stubViewport();
    const scroll = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = vi.fn();
    try {
      const { container } = render(<DiffNodeView node={longDiff()} />);
      fireEvent.keyDown(container.querySelector('.app-diff')!, { key: 'f', ctrlKey: true });
      fireEvent.change(screen.getByRole('textbox', { name: 'Find in diff' }), { target: { value: 'remote' } });
      expect(container.querySelector('[data-search-current]')).toHaveTextContent('remote');
    } finally {
      Element.prototype.scrollIntoView = scroll;
    }
  });
});

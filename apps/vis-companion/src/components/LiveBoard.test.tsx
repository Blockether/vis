// @vitest-environment jsdom
import { act, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { expect, it, vi } from 'vitest';
import { liveViewFromWire } from '../lib/live-view';
import { LiveViewPanel } from './LiveView';

function board(isEditable = true) {
  const view = liveViewFromWire({ id: 'v', title: 'Test view', seq: 0, nodes: [{
    id: 'board', type: 'board', label: 'Work board', is_editable: isEditable, is_selectable: true,
    columns: [{ id: 'open', title: 'Open' }, { id: 'done', title: 'Done', is_done: true }],
    issues: [{ id: 'a', key: 'APP-1', title: 'Write the guide', column: 'open', text: '**First draft**' },
      { id: 'b', key: 'APP-2', title: 'Read the guide', column: 'open', rank: 1 }],
  }] });
  if (!view) throw new Error('Expected a view');
  const onBoardAction = vi.fn(() => Promise.resolve(true));
  const onSelect = vi.fn();
  render(<LiveViewPanel view={view} onBoardAction={onBoardAction} onSelect={onSelect} inDialog />);
  return { onBoardAction, onSelect };
}

it('moves with the keyboard and shows the new place before the next patch', async () => {
  const user = userEvent.setup();
  const { onBoardAction } = board();
  screen.getByRole('button', { name: 'Move APP-1' }).focus();
  await user.keyboard('{Enter}');
  await user.click(within(screen.getByRole('dialog', { name: 'Move APP-1' })).getByRole('button', { name: 'Move down' }));
  expect(onBoardAction).toHaveBeenCalledWith({ action: 'move', node_id: 'board', item_id: 'a', column: 'open', index: 1 });
  const column = screen.getByLabelText('Open issues');
  expect(within(column).getAllByRole('button', { name: /^APP-/ }).map((button) => button.getAttribute('aria-label'))).toEqual(['APP-2 Read the guide', 'APP-1 Write the guide']);
});

it('returns the card to its former place if the change is refused', async () => {
  const { onBoardAction } = board();
  onBoardAction.mockResolvedValueOnce(false);
  fireEvent.click(screen.getByRole('button', { name: 'Move APP-1' }));
  fireEvent.click(screen.getByRole('button', { name: 'Move down' }));
  expect(await screen.findByRole('alert')).toHaveTextContent('The board did not accept the change.');
  expect(within(screen.getByLabelText('Open issues')).getAllByRole('button', { name: /^APP-/ })[0]).toHaveAccessibleName('APP-1 Write the guide');
});

it('creates a new issue in the chosen column and does not create one on Escape', async () => {
  const user = userEvent.setup();
  const { onBoardAction } = board();
  await user.click(screen.getByRole('button', { name: 'Create in Done' }));
  await user.type(screen.getByRole('textbox', { name: 'Title in Done' }), 'Drop this draft{Escape}');
  expect(onBoardAction).not.toHaveBeenCalled();
  expect(screen.getByRole('button', { name: 'Create in Done' })).toHaveFocus();
  await user.keyboard('{Enter}');
  await user.type(screen.getByRole('textbox', { name: 'Title in Done' }), 'Check the new issue{Enter}');
  expect(onBoardAction).toHaveBeenCalledWith({ action: 'edit', node_id: 'board', values: { title: 'Check the new issue', column: 'done' } });
  await waitFor(() => expect(screen.queryByRole('textbox', { name: 'Title in Done' })).not.toBeInTheDocument());
});

it('opens the issue, edits only its title, and sends a close selection', async () => {
  const user = userEvent.setup();
  const { onBoardAction, onSelect } = board();
  await user.click(screen.getByRole('button', { name: 'APP-1 Write the guide' }));
  expect(onSelect).toHaveBeenCalledWith('board', ['a']);
  const dialog = screen.getByRole('dialog', { name: 'APP-1' });
  expect(within(dialog).getByText('First draft').tagName).toBe('STRONG');
  const title = within(dialog).getByRole('textbox', { name: 'Title' });
  await user.clear(title);
  await user.type(title, 'Update the guide{Enter}');
  expect(onBoardAction).toHaveBeenCalledWith({ action: 'edit', node_id: 'board', item_id: 'a', values: { title: 'Update the guide' } });
  await user.click(within(dialog).getByRole('button', { name: 'Close APP-1' }));
  expect(onSelect).toHaveBeenLastCalledWith('board', []);
});

it('restores the saved title when an edit is refused', async () => {
  const user = userEvent.setup();
  const { onBoardAction } = board();
  onBoardAction.mockResolvedValueOnce(false);
  await user.click(screen.getByRole('button', { name: 'APP-1 Write the guide' }));
  const title = screen.getByRole('textbox', { name: 'Title' });
  await user.clear(title);
  await user.type(title, 'Unaccepted title{Enter}');
  expect(await screen.findByRole('alert')).toHaveTextContent('The board did not accept the change.');
  await waitFor(() => expect(screen.getByRole('textbox', { name: 'Title' })).toHaveValue('Write the guide'));
});

it('allows reading an issue on a read-only board but offers no edit actions', async () => {
  const user = userEvent.setup();
  const { onBoardAction } = board(false);
  expect(screen.queryByRole('button', { name: /Create|Move APP/ })).not.toBeInTheDocument();
  await user.click(screen.getByRole('button', { name: 'APP-1 Write the guide' }));
  const dialog = within(screen.getByRole('dialog', { name: 'APP-1' }));
  expect(dialog.getByText('First draft')).toBeVisible();
  expect(dialog.queryByRole('button', { name: /Edit|Delete|Move/ })).not.toBeInTheDocument();
  expect(onBoardAction).not.toHaveBeenCalled();
});

it('does not open the move menu when a held touch releases over its handle', () => {
  vi.useFakeTimers();
  try {
    board();
    const handle = screen.getByRole('button', { name: 'Move APP-1' });
    vi.spyOn(handle, 'getBoundingClientRect').mockReturnValue(new DOMRect(0, 0, 44, 44));
    const hit = vi.fn(() => handle);
    const before = Object.getOwnPropertyDescriptor(document, 'elementFromPoint');
    Object.defineProperty(document, 'elementFromPoint', { configurable: true, value: hit });
    try {
      const finger = { pointerType: 'touch', pointerId: 1, isPrimary: true, button: 0, clientX: 22, clientY: 22 };
      fireEvent.pointerDown(handle, { ...finger, buttons: 1 });
      act(() => vi.advanceTimersByTime(250));
      expect(handle.closest('.board')).toHaveAttribute('data-dragging', 'true');
      fireEvent.pointerUp(handle, { ...finger, buttons: 0 });
      fireEvent.click(handle, { detail: 1 });
      expect(screen.queryByRole('dialog', { name: 'Move APP-1' })).not.toBeInTheDocument();
    } finally {
      if (before) Object.defineProperty(document, 'elementFromPoint', before);
      else Reflect.deleteProperty(document, 'elementFromPoint');
    }
  } finally {
    vi.useRealTimers();
  }
});

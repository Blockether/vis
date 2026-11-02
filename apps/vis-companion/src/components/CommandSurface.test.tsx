// @vitest-environment jsdom
import { act, cleanup, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { CommandProvider, CommandRegistry, useCommands, type CommandHandlers } from '../lib/commands';
import { CommandSurface } from './CommandSurface';

function Commands({ handlers }: { handlers: CommandHandlers }) {
  useCommands(handlers);
  return null;
}

function mount(handlers: CommandHandlers) {
  const later = vi.fn();
  window.addEventListener('keydown', later);
  const view = render(
    <CommandProvider registry={new CommandRegistry()}>
      <Commands handlers={handlers} />
      <input aria-label="Message" />
      <CommandSurface />
    </CommandProvider>,
  );
  return { view, later, done: () => window.removeEventListener('keydown', later) };
}

afterEach(cleanup);

describe('CommandSurface', () => {
  // User request: C-x s s replaces Ctrl+/ and opens the search from a field too.
  it('runs C-x s s from a field and keeps its keys out of the field', async () => {
    const search = vi.fn();
    const { later, done } = mount({ 'show-sessions': search });
    const field = screen.getByRole('textbox', { name: 'Message' });
    await userEvent.click(field);
    await userEvent.keyboard('{Control>}x{/Control}s');
    expect(screen.getByRole('status')).toHaveTextContent('switch session');
    await userEvent.keyboard('s');
    expect(search).toHaveBeenCalledTimes(1);
    expect(field).toHaveValue('');
    expect(screen.queryByRole('status')).not.toBeInTheDocument();
    expect(later.mock.calls.map(([event]) => (event as KeyboardEvent).key)).toEqual(['Control']);
    done();
  });

  it('leaves Ctrl+X to the field when the field holds a selection', async () => {
    const { later, done } = mount({ 'show-sessions': () => {} });
    const field = screen.getByRole<HTMLInputElement>('textbox', { name: 'Message' });
    await userEvent.type(field, 'cut me');
    field.setSelectionRange(0, 3);
    await userEvent.keyboard('{Control>}x{/Control}');
    expect(screen.queryByRole('status')).not.toBeInTheDocument();
    expect(later.mock.calls.some(([event]) => (event as KeyboardEvent).key === 'x')).toBe(true);
    done();
  });

  // The Escape that ends a chord must not also stop the running turn.
  it('ends a chord on Escape without passing the Escape on', async () => {
    const search = vi.fn();
    const { later, done } = mount({ 'show-sessions': search });
    await userEvent.keyboard('{Control>}x{/Control}{Escape}s');
    expect(search).not.toHaveBeenCalled();
    expect(later.mock.calls.some(([event]) => (event as KeyboardEvent).key === 'Escape')).toBe(false);
    done();
  });

  it('lists only the commands that can run, and runs the chosen one from the palette', async () => {
    const search = vi.fn();
    const { done } = mount({ 'show-sessions': search, 'delete-session': false });
    await userEvent.keyboard('{Control>}x{/Control}p');
    const options = screen.getAllByRole('option').map((option) => option.textContent);
    expect(options.some((text) => text?.startsWith('switch session'))).toBe(true);
    expect(options.some((text) => text?.startsWith('delete session'))).toBe(false);
    await userEvent.keyboard('switch{Enter}');
    await act(() => new Promise((resolve) => setTimeout(resolve, 0)));
    expect(search).toHaveBeenCalledTimes(1);
    done();
  });
});

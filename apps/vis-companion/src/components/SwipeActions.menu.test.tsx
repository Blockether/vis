// @vitest-environment jsdom
import { fireEvent, render, screen, within } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { SwipeActions } from './SwipeActions';
import { PencilIcon, TrashIcon } from './icons';

// Regression: invisible desktop action slots consumed the session title's width.
describe('desktop row action menu', () => {
  function setup() {
    const rename = vi.fn();
    const remove = vi.fn();
    const open = vi.fn();
    const view = render(
      <SwipeActions
        label="Review gateway reconnect behavior"
        actions={[
          { key: 'rename', label: 'Rename', icon: <PencilIcon />, onSelect: rename },
          { key: 'delete', label: 'Delete', icon: <TrashIcon />, tone: 'danger', onSelect: remove },
        ]}
      >
        <button onClick={open}>Open session</button>
      </SwipeActions>,
    );
    const trigger = screen.getByRole('button', {
      name: 'Actions for Review gateway reconnect behavior',
    });
    return { ...view, trigger, rename, remove, open };
  }

  it('opens named actions without opening the row and returns focus on Escape', () => {
    const { trigger, open } = setup();
    expect(trigger).toHaveAttribute('aria-expanded', 'false');
    fireEvent.click(trigger);
    const menu = screen.getByRole('dialog', { name: 'Review gateway reconnect behavior actions' });
    expect(trigger).toHaveAttribute('aria-expanded', 'true');
    expect(within(menu).getByRole('button', { name: 'Rename' })).toHaveFocus();
    expect(open).not.toHaveBeenCalled();
    fireEvent.keyDown(document.activeElement!, { key: 'Escape' });
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
    expect(trigger).toHaveFocus();
  });

  it('selects once, closes the menu, and passes the persistent trigger to the action', () => {
    const { trigger, rename, remove } = setup();
    fireEvent.click(trigger);
    const menu = screen.getByRole('dialog');
    fireEvent.click(within(menu).getByRole('button', { name: 'Rename' }));
    expect(rename).toHaveBeenCalledExactlyOnceWith(trigger);
    expect(remove).not.toHaveBeenCalled();
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });

  it('keeps keyboard focus inside the menu and dismisses on outside click or resize', () => {
    const { trigger } = setup();
    fireEvent.click(trigger);
    const menu = screen.getByRole('dialog');
    const buttons = within(menu).getAllByRole('button');
    buttons.at(-1)!.focus();
    fireEvent.keyDown(document.activeElement!, { key: 'Tab' });
    expect(buttons[0]).toHaveFocus();
    fireEvent.keyDown(document.activeElement!, { key: 'Tab', shiftKey: true });
    expect(buttons.at(-1)).toHaveFocus();
    fireEvent.click(menu.parentElement!);
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
    fireEvent.click(trigger);
    fireEvent(window, new Event('resize'));
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });
});

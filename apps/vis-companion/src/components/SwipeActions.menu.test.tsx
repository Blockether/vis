// @vitest-environment jsdom
import { fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { SwipeActions } from './SwipeActions';
import { PencilIcon, TrashIcon } from './icons';

/** One row with a Rename and a danger-toned Delete, and a title that opens the session. */
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

// Regression: invisible desktop action slots consumed the session title's width.
describe('desktop row action menu', () => {
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

  // Regression, user report (paraphrased: every update to the session hides the
  // menu I have open): the panel dismissed itself on ANY scroll in the document,
  // and a transcript that is following its end rewrites its own `scrollTop` on
  // every update it receives — behind a settings dialog, on another screen, it
  // makes no difference. A scroller that does not carry the row cannot move the
  // panel off its anchor, so it no longer closes it.
  it('stays open while something that cannot move it scrolls', () => {
    const elsewhere = document.body.appendChild(document.createElement('div'));
    const { trigger } = setup();
    fireEvent.click(trigger);
    expect(screen.getByRole('dialog')).toBeInTheDocument();

    fireEvent.scroll(elsewhere);

    expect(screen.getByRole('dialog')).toBeInTheDocument();

    // The list the row stands in is the other case: it takes the anchor with it.
    fireEvent.scroll(trigger.closest('[data-swipe-track]')!.parentElement!);

    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
    elsewhere.remove();
  });
});

// The desktop window drops the webview's own right-click menu (`lib/desktop.ts`), so a
// right-click on a row answers with the row's own verbs — the ones behind its `⋯` — at
// the cursor that asked for them.
describe('a right-click on the row', () => {
  /** A desk with a mouse, or a bare touch screen. */
  function pointing(kind: 'fine' | 'coarse') {
    vi.stubGlobal('matchMedia', (query: string) => ({
      matches: query.includes('pointer: fine') ? kind === 'fine' : false,
      media: query,
      onchange: null,
      addListener: () => undefined,
      removeListener: () => undefined,
      addEventListener: () => undefined,
      removeEventListener: () => undefined,
      dispatchEvent: () => false,
    }));
  }

  function rightClick() {
    const track = screen
      .getByRole('button', { name: 'Open session' })
      .closest<HTMLElement>('[data-swipe-track]')!;
    return fireEvent.contextMenu(track, { clientX: 220, clientY: 140 });
  }

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it('opens the row menu at the cursor instead of the system one', () => {
    pointing('fine');
    const { open } = setup();

    const systemMenu = rightClick();

    expect(systemMenu).toBe(false);
    const menu = screen.getByRole('dialog', { name: 'Review gateway reconnect behavior actions' });
    expect(within(menu).getByRole('button', { name: 'Rename' })).toHaveFocus();
    expect(menu.style.getPropertyValue('--menu-left')).toBe('220px');
    expect(menu.style.getPropertyValue('--menu-top')).toBe('146px');
    expect(open).not.toHaveBeenCalled();
  });

  it('runs a verb picked there against the persistent row trigger', () => {
    pointing('fine');
    const { trigger, rename } = setup();
    rightClick();

    fireEvent.click(within(screen.getByRole('dialog')).getByRole('button', { name: 'Rename' }));

    expect(rename).toHaveBeenCalledExactlyOnceWith(trigger);
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });

  it('leaves a long press to the platform under a finger', () => {
    pointing('coarse');
    setup();

    expect(rightClick()).toBe(true);
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument();
  });
});

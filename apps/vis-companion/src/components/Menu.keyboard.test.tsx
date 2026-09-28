// @vitest-environment jsdom
import { render, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { Menu } from './Menu';

const keyboard = vi.hoisted(() => ({ inset: 0 }));
vi.mock('../lib/viewport', () => ({ useKeyboardInset: () => keyboard.inset }));

const height = window.innerHeight;
beforeEach(() => {
  window.innerHeight = 844;
  keyboard.inset = 0;
});
afterEach(() => {
  window.innerHeight = height;
});

function panel(at: { left: number; top?: number; bottom?: number }) {
  const content = () => <Menu label="Groups in vis" at={at} onDismiss={() => {}}>Group name</Menu>;
  const view = render(content());
  const dialog = screen.getByRole('dialog', { name: 'Groups in vis' });
  return { dialog, update: () => view.rerender(content()) };
}

describe('a menu above the phone keyboard', () => {
  // Regression, user report: the new-group form jumped from the Groups heading
  // to the keyboard even though there was ample room beneath the heading.
  it('stays beside its heading when the form fits between it and the keyboard', () => {
    const { dialog, update } = panel({ top: 224, left: 12 });
    keyboard.inset = 300;
    update();
    expect(dialog).toHaveStyle({ '--menu-top': '224px' });
    expect(dialog.style.getPropertyValue('--menu-bottom')).toBe('');
    expect(dialog.className).toContain('var(--menu-top,0px)');
    keyboard.inset = 0;
    update();
    expect(dialog).toHaveStyle({ '--menu-top': '224px' });
  });

  it('lifts a panel whose heading is covered by the keyboard', () => {
    const { dialog, update } = panel({ top: 600, left: 12 });
    keyboard.inset = 300;
    update();
    expect(dialog).toHaveStyle({ '--menu-bottom': '312px' });
    expect(dialog.style.getPropertyValue('--menu-top')).toBe('');
  });

  it('lifts a panel when too little room remains to use the form below its heading', () => {
    const { dialog, update } = panel({ top: 490, left: 12 });
    keyboard.inset = 300;
    update();
    expect(dialog).toHaveStyle({ '--menu-bottom': '312px' });
    expect(dialog.style.getPropertyValue('--menu-top')).toBe('');
  });

  it('keeps a panel flipped above its heading there if the keyboard is lower', () => {
    const { dialog, update } = panel({ bottom: 550, left: 12 });
    keyboard.inset = 300;
    update();
    expect(dialog).toHaveStyle({ '--menu-bottom': '550px' });
  });
});

// @vitest-environment jsdom
// Live apps: an app binds its own keys, and the surface reads only the keys that it may.
import { describe, expect, it } from 'vitest';

import { applyLivePatch, liveViewFromWire, LIVE_SHORTCUTS_MAX, type LiveView } from './live-view';
import {
  ariaShortcut,
  isAppKeyTarget,
  isEditableTarget,
  matchShortcut,
  nodeShortcuts,
  shortcutArrow,
  shortcutGlyph,
  shortcutKey,
  shortcutName,
  shortcutSpoken,
} from './app-shortcuts';

function app(shortcuts: unknown): LiveView {
  const view = liveViewFromWire({
    id: 'game',
    title: 'Game',
    placement: 'attachment',
    seq: 1,
    nodes: [{ id: 'go', type: 'button', label: 'Go' }],
    shortcuts,
  });
  if (!view) throw new Error('fixture must parse');
  return view;
}

function press(key: string, extra: Partial<KeyboardEvent> = {}) {
  return {
    key,
    ctrlKey: false,
    metaKey: false,
    altKey: false,
    repeat: false,
    isComposing: false,
    target: document.body,
    ...extra,
  } as KeyboardEvent;
}

describe('shortcuts on the wire', () => {
  it('keeps valid keys with exactly one target, the first of a repeated key', () => {
    const view = app([
      { key: 'ArrowUp', action: 'up', label: 'Up' },
      { key: 'g', node_id: 'go' },
      { key: 'ArrowUp', action: 'again' },
      { key: 'Escape', action: 'quit' },
      { key: 'Tab', action: 'next' },
      { key: 'ab', action: 'two' },
      { key: 'x' },
      { key: 'y', action: 'both', node_id: 'go' },
      { key: ' ', action: 'space-char' },
      { key: 'Space', action: 'jump', label: '   ' },
    ]);
    expect(view.shortcuts).toEqual([
      { key: 'ArrowUp', action: 'up', label: 'Up' },
      { key: 'g', node_id: 'go' },
      { key: 'Space', action: 'jump' },
    ]);
  });

  it('keeps at most the schema bound', () => {
    const many = Array.from({ length: 40 }, (_, at) => ({ key: String.fromCharCode(0x21 + at), action: `a${at}` }));
    expect(app(many).shortcuts).toHaveLength(LIVE_SHORTCUTS_MAX);
  });

  it('a view without shortcuts has none', () => {
    expect(app(undefined).shortcuts).toBeUndefined();
  });

  it('the shortcuts op replaces every binding, and an empty list clears them', () => {
    const view = app([{ key: 'a', action: 'left' }]);
    const swapped = applyLivePatch(view, {
      first_seq: 2,
      patch: { seq: 2, ops: [{ op: 'shortcuts', shortcuts: [{ key: 'd', action: 'right' }] }] },
    });
    expect(swapped.shortcuts).toEqual([{ key: 'd', action: 'right' }]);
    expect(swapped.seq).toBe(2);
    const cleared = applyLivePatch(swapped, {
      first_seq: 3,
      patch: { seq: 3, ops: [{ op: 'shortcuts', shortcuts: [] }] },
    });
    expect(cleared.shortcuts).toEqual([]);
  });
});

describe('matchShortcut', () => {
  const shortcuts = [
    { key: 'a', action: 'lower' },
    { key: 'A', action: 'upper' },
    { key: 'Space', action: 'jump' },
    { key: '?', action: 'help' },
  ];

  it('is case-sensitive and allows Shift', () => {
    expect(matchShortcut(shortcuts, press('a'))?.action).toBe('lower');
    expect(matchShortcut(shortcuts, press('A', { shiftKey: true }))?.action).toBe('upper');
    expect(matchShortcut(shortcuts, press('?', { shiftKey: true }))?.action).toBe('help');
  });

  it('reads the space bar as Space', () => {
    expect(shortcutKey(press(' '))).toBe('Space');
    expect(matchShortcut(shortcuts, press(' '))?.action).toBe('jump');
  });

  it('ignores modifiers, held keys, composition and unbound keys', () => {
    expect(matchShortcut(shortcuts, press('a', { ctrlKey: true }))).toBeNull();
    expect(matchShortcut(shortcuts, press('a', { metaKey: true }))).toBeNull();
    expect(matchShortcut(shortcuts, press('a', { altKey: true }))).toBeNull();
    expect(matchShortcut(shortcuts, press('a', { repeat: true }))).toBeNull();
    expect(matchShortcut(shortcuts, press('a', { isComposing: true }))).toBeNull();
    expect(matchShortcut(shortcuts, press('b'))).toBeNull();
    expect(matchShortcut(shortcuts, press('Escape'))).toBeNull();
    expect(matchShortcut([], press('a'))).toBeNull();
  });

  it('leaves a key typed into a field to the field', () => {
    const input = document.createElement('input');
    const checkbox = Object.assign(document.createElement('input'), { type: 'checkbox' });
    const area = document.createElement('textarea');
    const editable = document.createElement('div');
    editable.setAttribute('contenteditable', 'true');
    const inner = document.createElement('span');
    editable.append(inner);
    document.body.append(input, checkbox, area, editable);
    expect(isEditableTarget(input)).toBe(true);
    expect(isEditableTarget(area)).toBe(true);
    expect(isEditableTarget(inner)).toBe(true);
    expect(isEditableTarget(checkbox)).toBe(false);
    expect(isEditableTarget(document.body)).toBe(false);
    expect(matchShortcut(shortcuts, press('a', { target: input }))).toBeNull();
    expect(matchShortcut(shortcuts, press('a', { target: checkbox }))?.action).toBe('lower');
    document.body.replaceChildren();
  });
});

describe('key caps', () => {
  it('draws arrow keys as icons, shortens named keys and keeps characters', () => {
    expect(shortcutArrow('ArrowLeft')).toBe('left');
    expect(shortcutArrow('ArrowDown')).toBe('down');
    expect(shortcutArrow('q')).toBeNull();
    expect(shortcutGlyph('Enter')).toBe('↵');
    expect(shortcutGlyph('Escape')).toBe('Esc');
    expect(shortcutGlyph('q')).toBe('q');
    expect(ariaShortcut('Q')).toBe('Shift+Q');
    expect(ariaShortcut('q')).toBe('q');
  });

  it('indexes node shortcuts by node id', () => {
    const byNode = nodeShortcuts([
      { key: 'g', node_id: 'go' },
      { key: 'h', node_id: 'go' },
      { key: 'x', action: 'x' },
    ]);
    expect([...byNode.keys()]).toEqual(['go']);
    expect(byNode.get('go')?.key).toBe('g');
  });
});

describe('app key names and owners', () => {
  it('names a legend entry by its label and its key, so equal labels stay apart', () => {
    expect(shortcutName({ key: 'n', node_id: 'next' }, 'Next')).toBe('Next, key n');
    expect(shortcutName({ key: 'ArrowUp', action: 'up', label: 'Up' })).toBe('Up, key Up arrow');
    expect(shortcutName({ key: 'k', action: 'up', label: 'Up' })).toBe('Up, key k');
    expect(shortcutSpoken('PageDown')).toBe('Page down');
    expect(shortcutSpoken('Enter')).toBe('Enter');
  });

  it('lets a checkbox, a radio, a button and an option keep app keys, but not a text field', () => {
    const make = (html: string) => {
      const host = document.createElement('div');
      host.innerHTML = html;
      return host.firstElementChild as HTMLElement;
    };
    expect(isEditableTarget(make('<input type="checkbox">'))).toBe(false);
    expect(isEditableTarget(make('<input type="radio">'))).toBe(false);
    expect(isEditableTarget(make('<button role="checkbox"></button>'))).toBe(false);
    expect(isEditableTarget(make('<button role="radio"></button>'))).toBe(false);
    expect(isEditableTarget(make('<div role="option"></div>'))).toBe(false);
    expect(isEditableTarget(make('<input type="text">'))).toBe(true);
    expect(isEditableTarget(make('<input type="range">'))).toBe(true);
    expect(isEditableTarget(make('<div role="combobox"></div>'))).toBe(true);
    expect(isEditableTarget(make('<div role="spinbutton"></div>'))).toBe(true);
    expect(isEditableTarget(make('<textarea></textarea>'))).toBe(true);
  });

  it('gives the app a key on the page, on its opener and inside its dialog, never in a dialog over it', () => {
    const app = document.createElement('div');
    const inside = document.createElement('button');
    app.append(inside);
    const opener = document.createElement('button');
    const over = document.createElement('div');
    over.setAttribute('role', 'dialog');
    const overButton = document.createElement('button');
    over.append(overButton);
    document.body.append(app, opener, over);
    expect(isAppKeyTarget(document.body, app, opener)).toBe(true);
    expect(isAppKeyTarget(window, app, opener)).toBe(true);
    expect(isAppKeyTarget(inside, app, opener)).toBe(true);
    expect(isAppKeyTarget(opener, app, opener)).toBe(true);
    expect(isAppKeyTarget(overButton, app, opener)).toBe(false);
    expect(isAppKeyTarget(opener, app, null)).toBe(false);
    expect(isAppKeyTarget(null, app, opener)).toBe(false);
    app.remove();
    opener.remove();
    over.remove();
  });
});

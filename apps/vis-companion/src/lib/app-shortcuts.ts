/**
 * The keyboard shortcuts of an app attachment.
 *
 * An app binds keys in its view (`shortcuts`). The surface reads each key press,
 * finds its binding and presses the bound node or sends the owner action. A key
 * with a modifier, a held key and a key typed into a field never reach the app.
 */

import { isLiveShortcutKey, type LiveShortcut } from './live-view';

/** The shortcut name of one key press, or null when no app can bind it. */
export function shortcutKey(event: Pick<KeyboardEvent, 'key'>): string | null {
  const key = event.key === ' ' ? 'Space' : event.key;
  return isLiveShortcutKey(key) ? key : null;
}

/**
 * True when `target` takes typed text, so its keys belong to it. A checkbox, a
 * radio, a button or an option takes no text: an app key still works there.
 */
export function isEditableTarget(target: EventTarget | null): boolean {
  if (!target || typeof (target as Element).closest !== 'function') return false;
  const element = target as HTMLElement;
  if (element.isContentEditable) return true;
  if (element.closest('[contenteditable=""], [contenteditable="true"]')) return true;
  if (element instanceof HTMLTextAreaElement || element instanceof HTMLSelectElement) return true;
  if (element instanceof HTMLInputElement) {
    // A range slider is editable here: its arrow keys move it.
    return !['button', 'checkbox', 'radio', 'reset', 'submit', 'color', 'file', 'image'].includes(element.type);
  }
  const role = element.getAttribute('role');
  return role === 'textbox' || role === 'combobox' || role === 'slider' || role === 'spinbutton';
}

/**
 * True when a key aimed at `target` belongs to the app in `dialog`: the page
 * itself, the element that opened the app, or anything inside the app's dialog.
 * A field still keeps its typed keys; `matchShortcut` checks that.
 */
export function isAppKeyTarget(target: EventTarget | null, dialog: Element | null, opener: Element | null): boolean {
  if (typeof document === 'undefined' || !target) return false;
  if (target === window || target === document || target === document.body || target === document.documentElement)
    return true;
  if (!(target instanceof Node)) return false;
  if (opener !== null && target === opener) return true;
  return dialog?.contains(target) === true;
}

/**
 * The binding of one key press, or null when the app must not see it: a key with
 * Ctrl, Meta or Alt, a held key, a press in a field or a key that is not bound.
 * Shift stays allowed, because `A` and `?` are typed with it.
 */
export function matchShortcut(
  shortcuts: readonly LiveShortcut[],
  event: Pick<KeyboardEvent, 'key' | 'ctrlKey' | 'metaKey' | 'altKey' | 'repeat' | 'target' | 'isComposing'>,
): LiveShortcut | null {
  if (shortcuts.length === 0) return null;
  if (event.ctrlKey || event.metaKey || event.altKey || event.repeat || event.isComposing) return null;
  if (isEditableTarget(event.target)) return null;
  const key = shortcutKey(event);
  if (!key) return null;
  return shortcuts.find((one) => one.key === key) ?? null;
}

const GLYPHS: Record<string, string> = {
  Enter: '↵',
  Space: 'Space',
  Backspace: '⌫',
  Delete: 'Del',
  Escape: 'Esc',
  Home: 'Home',
  End: 'End',
  PageUp: 'PgUp',
  PageDown: 'PgDn',
};

const ARROWS = { ArrowUp: 'up', ArrowDown: 'down', ArrowLeft: 'left', ArrowRight: 'right' } as const;

/** The direction of an arrow key, which a key cap draws as an icon, or null for any other key. */
export function shortcutArrow(key: string): (typeof ARROWS)[keyof typeof ARROWS] | null {
  return key in ARROWS ? ARROWS[key as keyof typeof ARROWS] : null;
}

/** The short text of a key cap: a name for a named key, the character for a character. */
export function shortcutGlyph(key: string): string {
  return GLYPHS[key] ?? key;
}

/** The words a screen reader says for a key, for `aria-keyshortcuts`. */
export function ariaShortcut(key: string): string {
  if (key === 'Space') return 'Space';
  if (key.length === 1 && key >= 'A' && key <= 'Z') return `Shift+${key}`;
  return key;
}

const SPOKEN: Record<string, string> = {
  ArrowUp: 'Up arrow',
  ArrowDown: 'Down arrow',
  ArrowLeft: 'Left arrow',
  ArrowRight: 'Right arrow',
  PageUp: 'Page up',
  PageDown: 'Page down',
  ' ': 'Space',
};

/** The name of a key in a sentence, such as `Up arrow`, `Enter` or `n`. */
export function shortcutSpoken(key: string): string {
  return SPOKEN[key] ?? key;
}

/**
 * The accessible name of a legend entry: its label and its key, as `Next, key n`.
 * The key keeps two entries with one label apart, such as two keys named `Up`.
 */
export function shortcutName(shortcut: LiveShortcut, nodeLabel?: string): string {
  return `${shortcutLabel(shortcut, nodeLabel)}, key ${shortcutSpoken(shortcut.key)}`;
}

/** The legend text of a shortcut: its own label, or the name of the node it presses. */
export function shortcutLabel(shortcut: LiveShortcut, nodeLabel?: string): string {
  return shortcut.label ?? nodeLabel ?? shortcut.action ?? shortcut.node_id ?? shortcut.key;
}

/** The binding of each node that a shortcut presses, by node id. */
export function nodeShortcuts(shortcuts: readonly LiveShortcut[]): Map<string, LiveShortcut> {
  const byNode = new Map<string, LiveShortcut>();
  for (const one of shortcuts) {
    if (one.node_id && !byNode.has(one.node_id)) byNode.set(one.node_id, one);
  }
  return byNode;
}

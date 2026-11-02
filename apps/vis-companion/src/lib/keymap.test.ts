import { describe, expect, it } from 'vitest';

import { KEYMAP_GROUPS, KEYMAP_VERBS, chordLabel, chordStep, matchVerbs, type ChordKey } from './keymap';

const key = (name: string, over: Partial<ChordKey> = {}): ChordKey => ({
  key: name,
  ctrlKey: false,
  metaKey: false,
  altKey: false,
  ...over,
});
const verb = (id: string) => KEYMAP_VERBS.find((item) => item.id === id)!;
const ctrlX = key('x', { ctrlKey: true });

describe('chordStep', () => {
  it('starts only on a plain Ctrl+X, not on an AltGr character or Cmd+X', () => {
    expect(chordStep(null, ctrlX)).toEqual({ kind: 'start' });
    expect(chordStep(null, key('x', { ctrlKey: true, altKey: true })).kind).toBe('ignore');
    expect(chordStep(null, key('x', { metaKey: true })).kind).toBe('ignore');
    expect(chordStep(null, key('s')).kind).toBe('ignore');
  });

  // User request: C-x s s opens the session search, as in the TUI.
  it('runs C-x s s, also with Ctrl still held', () => {
    expect(chordStep({ group: null }, key('s'))).toEqual({ kind: 'group', group: 'session' });
    expect(chordStep({ group: 'session' }, key('s'))).toEqual({ kind: 'verb', verb: verb('show-sessions') });
    expect(chordStep({ group: 'session' }, key('s', { ctrlKey: true }))).toEqual({
      kind: 'verb',
      verb: verb('show-sessions'),
    });
  });

  it('opens the palette with C-x p and runs a top-level verb after C-x', () => {
    expect(chordStep({ group: null }, key('p'))).toEqual({ kind: 'palette' });
    expect(chordStep({ group: null }, key('h'))).toEqual({ kind: 'verb', verb: verb('toggle-help') });
  });

  it('keeps an exact capital apart from its lower case, and falls back for a held Shift', () => {
    expect(chordStep({ group: 'view' }, key('Z'))).toEqual({ kind: 'verb', verb: verb('toggle-zen-mode') });
    expect(chordStep({ group: 'view' }, key('z'))).toEqual({ kind: 'verb', verb: verb('toggle-detail-labels') });
    expect(chordStep({ group: 'session' }, key('S'))).toEqual({ kind: 'verb', verb: verb('show-sessions') });
  });

  it('waits on a modifier alone', () => {
    expect(chordStep({ group: null }, key('Control', { ctrlKey: true })).kind).toBe('wait');
    expect(chordStep({ group: 'session' }, key('Shift')).kind).toBe('wait');
  });

  it('ends on Escape, C-g, a named key or a key that it does not know', () => {
    for (const press of [key('Escape'), key('g', { ctrlKey: true }), key('Enter'), key('k', { metaKey: true })]) {
      expect(chordStep({ group: 'session' }, press).kind).toBe('abort');
    }
    expect(chordStep({ group: null }, key('9')).kind).toBe('abort');
  });

  it('ends on a group or a verb that cannot run now', () => {
    const live = KEYMAP_VERBS.filter((item) => item.id === 'show-sessions');
    const groups = KEYMAP_GROUPS.filter((group) => group.id === 'session');
    expect(chordStep({ group: null }, key('m'), groups, live).kind).toBe('abort');
    expect(chordStep({ group: 'session' }, key('d'), groups, live).kind).toBe('abort');
  });
});

describe('matchVerbs', () => {
  it('needs every word, in the title, the group or the chord', () => {
    expect(matchVerbs(KEYMAP_VERBS, 'fast model').map((item) => item.id)).toEqual(['toggle-codex-fast']);
    expect(matchVerbs(KEYMAP_VERBS, 'c-x m c').map((item) => item.id)).toContain('pick-model');
    expect(matchVerbs(KEYMAP_VERBS, '  ')).toHaveLength(KEYMAP_VERBS.length);
  });
});

describe('chordLabel', () => {
  it('writes the prefix, the group key and the verb key', () => {
    expect(chordLabel(verb('show-sessions'))).toBe('C-x s s');
    expect(chordLabel(verb('toggle-help'))).toBe('C-x h');
  });
});

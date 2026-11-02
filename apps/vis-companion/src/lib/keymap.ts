import keymapSchema from '../../../../packages/vis-contract/resources/vis-contract/schema/keymap.json';

/**
 * THE C-x KEY MAP, SHARED WITH THE TUI.
 *
 * `vis-contract/schema/keymap.json` owns the groups, the verbs and their keys, so the
 * Companion and the TUI never drift. Press C-x, then a group key, then a verb key:
 * C-x s s opens the session search. C-x p opens the command palette.
 *
 * Every function here is pure. `commands.tsx` binds the verbs that a screen can run.
 */

type SchemaEntry = {
  const: string;
  title: string;
  description: string;
  'x-vis-key': string;
  'x-vis-group'?: string;
  'x-vis-show-when'?: string;
};

export type KeymapGroup = Readonly<{ id: string; key: string; title: string; description: string }>;

export type KeymapVerb = Readonly<{
  id: string;
  /** The plain key inside `group`, or after C-x when `group` is null. Null for a palette-only command. */
  key: string | null;
  group: string | null;
  title: string;
  description: string;
}>;

export const KEYMAP_GROUPS: readonly KeymapGroup[] = (keymapSchema.$defs.group.oneOf as SchemaEntry[]).map(
  (entry) => ({ id: entry.const, key: entry['x-vis-key'], title: entry.title, description: entry.description }),
);

export const KEYMAP_VERBS: readonly KeymapVerb[] = (keymapSchema.$defs.verb.oneOf as SchemaEntry[]).map((entry) => ({
  id: entry.const,
  key: entry['x-vis-key'],
  group: entry['x-vis-group'] ?? null,
  title: entry.title,
  description: entry.description,
}));

/** The key after C-x that opens the command palette. */
export const PALETTE_KEY: string = keymapSchema['x-vis-palette-key'];

/** The prefix, as the chord labels write it. */
export const PREFIX_LABEL: string = keymapSchema['x-vis-prefix'];

/**
 * Companion commands that have no C-x key. Their ids follow the TUI palette ids, so a
 * person finds the same name in both palettes.
 */
export const PALETTE_ONLY_VERBS: readonly KeymapVerb[] = [
  { id: 'settings', key: null, group: null, title: 'settings', description: 'Open the settings of this device and its machines' },
  { id: 'session-settings', key: null, group: null, title: 'session settings', description: 'Settings for this session only' },
  { id: 'automations', key: null, group: null, title: 'automations', description: 'Run prompts on a schedule or a webhook' },
];

/** Where a chord stands: null before C-x, `{ group: null }` after C-x, or inside a group. */
export type ChordState = Readonly<{ group: string | null }> | null;

/** What one key press does to the chord. */
export type ChordStep =
  /** The key is not part of a chord: leave it to its owner. */
  | { kind: 'ignore' }
  /** A modifier alone: the chord waits for the real key. */
  | { kind: 'wait' }
  | { kind: 'start' }
  | { kind: 'group'; group: string }
  | { kind: 'palette' }
  | { kind: 'verb'; verb: KeymapVerb }
  /** Escape, C-g or a key that the chord does not know ends the chord. */
  | { kind: 'abort' };

export type ChordKey = Readonly<{
  key: string;
  ctrlKey: boolean;
  metaKey: boolean;
  altKey: boolean;
}>;

const MODIFIERS = new Set(['Control', 'Shift', 'Alt', 'Meta', 'AltGraph', 'CapsLock', 'Fn', 'OS']);

/** The key of an exact match, else of the lower-case key: C-x b Z is not C-x b z. */
function byKey<T extends { key: string | null }>(items: readonly T[], key: string): T | undefined {
  return items.find((item) => item.key === key) ?? items.find((item) => item.key === key.toLowerCase());
}

/** True for the C-x press that starts a chord. AltGr layouts type characters as Ctrl+Alt. */
export function isPrefixKey(event: ChordKey): boolean {
  return event.ctrlKey && !event.metaKey && !event.altKey && event.key.toLowerCase() === 'x';
}

/**
 * The step that `event` takes from `state`. `groups` are the groups that open now, and
 * `verbs` the verbs that can run now. A second key may keep Ctrl down (C-x C-s C-s), as
 * in Emacs, because a held Ctrl is the common slip.
 */
export function chordStep(
  state: ChordState,
  event: ChordKey,
  groups: readonly KeymapGroup[] = KEYMAP_GROUPS,
  verbs: readonly KeymapVerb[] = KEYMAP_VERBS,
): ChordStep {
  if (!state) return isPrefixKey(event) ? { kind: 'start' } : { kind: 'ignore' };
  if (MODIFIERS.has(event.key)) return { kind: 'wait' };
  if (event.key === 'Escape' || (event.ctrlKey && event.key.toLowerCase() === 'g')) return { kind: 'abort' };
  if (event.metaKey || event.altKey || event.key.length !== 1) return { kind: 'abort' };
  if (state.group === null) {
    if (event.key.toLowerCase() === PALETTE_KEY) return { kind: 'palette' };
    const group = byKey(groups, event.key);
    if (group) return { kind: 'group', group: group.id };
  }
  const verb = byKey(
    verbs.filter((item) => item.key !== null && item.group === state.group),
    event.key,
  );
  return verb ? { kind: 'verb', verb } : { kind: 'abort' };
}

/** The chord label of `verb`, such as `C-x s s`, or null for a palette-only command. */
export function chordLabel(verb: KeymapVerb): string | null {
  if (verb.key === null) return null;
  const group = verb.group && KEYMAP_GROUPS.find((item) => item.id === verb.group);
  if (verb.group && !group) return null;
  return [PREFIX_LABEL, group ? group.key : null, verb.key].filter(Boolean).join(' ');
}

/** The keys of the chord label of `verb`, one key cap each: `['C-x', 's', 's']`. */
export function chordKeys(verb: KeymapVerb): string[] {
  return chordLabel(verb)?.split(' ') ?? [];
}

/**
 * The palette rows that match `query`: every word of the query is in the title, the
 * description, the group title or the chord. Rows keep their order.
 */
export function matchVerbs(verbs: readonly KeymapVerb[], query: string): KeymapVerb[] {
  const words = query.toLowerCase().split(/\s+/).filter(Boolean);
  if (!words.length) return [...verbs];
  return verbs.filter((verb) => {
    const group = KEYMAP_GROUPS.find((item) => item.id === verb.group);
    const text = [verb.title, verb.description, group?.title, chordLabel(verb)].join(' ').toLowerCase();
    return words.every((word) => text.includes(word));
  });
}

/** The section title of `verb` in the palette and the band. */
export function sectionTitle(verb: KeymapVerb): string {
  return KEYMAP_GROUPS.find((item) => item.id === verb.group)?.title ?? 'General';
}

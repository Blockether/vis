import { describe, expect, it } from 'vitest';

import { filterSettings, flattenSettings, replaceSetting } from './setting-tree';
import type { Toggle } from './types';

const tree: Toggle[] = [
  {
    id: 'council',
    label: 'Council',
    type: 'boolean',
    enabled: true,
    children: [
      {
        id: 'council_room',
        label: 'Council room',
        type: 'enum',
        value: 'local',
        children: [{ id: 'council_room_a_access', label: 'Allow A', type: 'boolean', enabled: true }],
      },
    ],
  },
  { id: 'notifier_enabled', label: 'Desktop alerts', type: 'boolean', enabled: false },
];

describe('setting tree', () => {
  it('lists every row at any depth in display order', () => {
    expect(flattenSettings(tree).map((row) => row.id)).toEqual([
      'council',
      'council_room',
      'council_room_a_access',
      'notifier_enabled',
    ]);
  });

  it('replaces a nested row and keeps the rows under it', () => {
    const next = replaceSetting(tree, { id: 'council_room', label: 'Council room', type: 'enum', value: 'a' });
    const room = next[0].children?.[0];
    expect(room?.value).toBe('a');
    expect(room?.children?.map((row) => row.id)).toEqual(['council_room_a_access']);
    const off = replaceSetting(tree, { id: 'council', label: 'Council', type: 'boolean', enabled: false });
    expect(off[0].enabled).toBe(false);
    expect(off[0].children).toHaveLength(1);
    expect(off[1]).toBe(tree[1]);
  });

  it('keeps a match with the rows above it', () => {
    const found = filterSettings(tree, (row) => row.id === 'council_room_a_access');
    expect(flattenSettings(found).map((row) => row.id)).toEqual(['council', 'council_room', 'council_room_a_access']);
    expect(flattenSettings(filterSettings(tree, (row) => row.id === 'council'))).toHaveLength(3);
    expect(filterSettings(tree, () => false)).toEqual([]);
  });
});

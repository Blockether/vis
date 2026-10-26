import { describe, expect, it } from 'vitest';

import { filterSettings, flattenSettings, replaceSetting } from './setting-tree';
import type { Toggle } from './types';

const tree: Toggle[] = [
  {
    id: 'feature',
    label: 'Feature',
    type: 'boolean',
    enabled: true,
    children: [
      {
        id: 'feature_mode',
        label: 'Feature mode',
        type: 'enum',
        value: 'local',
        children: [{ id: 'feature_mode_a', label: 'Allow A', type: 'boolean', enabled: true }],
      },
    ],
  },
  { id: 'notifier_enabled', label: 'Desktop alerts', type: 'boolean', enabled: false },
];

describe('setting tree', () => {
  it('lists every row at any depth in display order', () => {
    expect(flattenSettings(tree).map((row) => row.id)).toEqual([
      'feature',
      'feature_mode',
      'feature_mode_a',
      'notifier_enabled',
    ]);
  });

  it('replaces a nested row and keeps the rows under it', () => {
    const next = replaceSetting(tree, { id: 'feature_mode', label: 'Feature mode', type: 'enum', value: 'a' });
    const mode = next[0].children?.[0];
    expect(mode?.value).toBe('a');
    expect(mode?.children?.map((row) => row.id)).toEqual(['feature_mode_a']);
    const off = replaceSetting(tree, { id: 'feature', label: 'Feature', type: 'boolean', enabled: false });
    expect(off[0].enabled).toBe(false);
    expect(off[0].children).toHaveLength(1);
    expect(off[1]).toBe(tree[1]);
  });

  it('keeps a match with the rows above it', () => {
    const found = filterSettings(tree, (row) => row.id === 'feature_mode_a');
    expect(flattenSettings(found).map((row) => row.id)).toEqual(['feature', 'feature_mode', 'feature_mode_a']);
    expect(flattenSettings(filterSettings(tree, (row) => row.id === 'feature'))).toHaveLength(3);
    expect(filterSettings(tree, () => false)).toEqual([]);
  });
});

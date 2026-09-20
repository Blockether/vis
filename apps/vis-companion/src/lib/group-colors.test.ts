import { describe, expect, it } from 'vitest';

import {
  DEFAULT_GROUP_COLOR,
  GROUP_COLORS,
  groupColor,
  groupSwatch,
  isGroupColor,
} from './group-colors';

describe('the closed group palette', () => {
  it('offers the eight tokens the gateway validates, in its own order', () => {
    expect([...GROUP_COLORS]).toEqual([
      'slate',
      'blue',
      'green',
      'amber',
      'red',
      'violet',
      'cyan',
      'pink',
    ]);
  });

  it('fills every token with a class of its own', () => {
    const swatches = GROUP_COLORS.map((color) => groupSwatch(color));
    expect(swatches).toEqual(GROUP_COLORS.map((color) => `bg-group-${color}`));
    expect(new Set(swatches).size).toBe(GROUP_COLORS.length);
  });

  // A band with no swatch reads as a band with no group, which is the one thing it
  // is not: a colour this app has never heard of still has to paint something.
  it('reads an unknown, empty or absent colour as the default token', () => {
    expect(groupColor('chartreuse')).toBe(DEFAULT_GROUP_COLOR);
    expect(groupColor('')).toBe(DEFAULT_GROUP_COLOR);
    expect(groupColor(null)).toBe(DEFAULT_GROUP_COLOR);
    expect(groupColor(undefined)).toBe(DEFAULT_GROUP_COLOR);
    expect(groupSwatch('#ff0000')).toBe('bg-group-slate');
  });

  it('tells a palette token from anything else', () => {
    expect(isGroupColor('violet')).toBe(true);
    expect(isGroupColor('VIOLET')).toBe(false);
    expect(isGroupColor(7)).toBe(false);
    expect(isGroupColor(null)).toBe(false);
  });
});

import { describe, expect, it } from 'vitest';

import { swipePosition, swipeRatio } from './app-image';

const png = (width?: number, height?: number) => ({
  media_type: 'image/png' as const,
  data: 'x',
  ...(width ? { width } : {}),
  ...(height ? { height } : {}),
});

describe('an image comparison', () => {
  it('sizes the swipe box by the new picture, else by the old one', () => {
    expect(swipeRatio(png(4, 3), png(1, 1))).toBe('4 / 3');
    expect(swipeRatio(png(), png(16, 9))).toBe('16 / 9');
    expect(swipeRatio(png(), png())).toBeUndefined();
  });

  it('keeps the swipe line inside the box', () => {
    expect(swipePosition(-4)).toBe(0);
    expect(swipePosition(140)).toBe(100);
    expect(swipePosition(Number.NaN)).toBe(50);
  });
});

import { afterEach, beforeEach, expect, it, vi } from 'vitest';

beforeEach(() => {
  vi.resetModules();
});

afterEach(() => {
  vi.unstubAllGlobals();
});

it('bundles Justice where the runtime segments text', async () => {
  const segmenter = Intl.Segmenter;
  const { engine } = await import('./justice');
  expect(Object.keys(engine ?? {})).toEqual(expect.arrayContaining(['prepare', 'solve']));
  expect(Intl.Segmenter).toBe(segmenter);
});

it.each<[string, object]>([
  ['missing', {}],
  ['undefined', { Segmenter: undefined }],
])('lends Justice a segmenter only while it loads when Intl.Segmenter is %s', async (_, intl) => {
  const original = Object.getOwnPropertyDescriptor(intl, 'Segmenter');
  vi.stubGlobal('Intl', intl);
  await import('./justice-segmenter');
  expect(new Intl.Segmenter(undefined, { granularity: 'grapheme' })).toBeInstanceOf(Intl.Segmenter);
  const { engine } = await import('./justice');
  expect(engine).toBeNull();
  expect(Object.getOwnPropertyDescriptor(Intl, 'Segmenter')).toEqual(original);
});

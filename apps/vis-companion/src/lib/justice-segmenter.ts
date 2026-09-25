/**
 * Justice creates its grapheme segmenter as soon as its module loads. Where the
 * runtime has no `Intl.Segmenter`, that would stop the bundle that contains Justice
 * from loading at all. `justice.ts` imports this module just before Justice: it lends
 * Justice a placeholder constructor for that one evaluation, and `restoreSegmenter`
 * puts `Intl` back as it was, so later feature checks still see the real runtime.
 */

const original = Object.getOwnPropertyDescriptor(Intl, 'Segmenter');

/** Whether the runtime can segment text; decided before Justice loads. */
export const segmenterAvailable = typeof Intl.Segmenter === 'function';

if (!segmenterAvailable) {
  Object.defineProperty(Intl, 'Segmenter', {
    configurable: true,
    writable: true,
    value: class PlaceholderSegmenter {},
  });
}

/** Removes the placeholder once Justice has loaded; a capable runtime is never touched. */
export function restoreSegmenter(): void {
  if (segmenterAvailable) return;
  if (original) Object.defineProperty(Intl, 'Segmenter', original);
  else Reflect.deleteProperty(Intl, 'Segmenter');
}

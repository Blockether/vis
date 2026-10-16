import { useLayoutEffect, useRef, type RefObject } from 'react';

/** How long a moved or new entry takes to settle, in milliseconds. */
export const LIST_MOTION_MS = 200;

/** The attribute that names one entry of a moving list. */
export const MOTION_KEY = 'data-motion-key';

type Tops = Map<string, number>;

/** Does this reader want the motion at all? */
function wantsMotion(view: Window | null): boolean {
  return !view?.matchMedia?.('(prefers-reduced-motion: reduce)').matches;
}

/** The vertical shift an animation adds to `node` right now, in pixels. */
function shiftOf(node: HTMLElement): number {
  const view = node.ownerDocument.defaultView;
  const transform = view?.getComputedStyle(node).transform;
  const values = /^matrix(3d)?\((.+)\)$/.exec(transform ?? '');
  if (!values) return 0;
  // `matrix(a, b, c, d, x, y)` keeps the shift 6th; `matrix3d(...)` keeps it 14th.
  const shift = Number(values[2].split(',')[values[1] ? 13 : 5]);
  return Number.isFinite(shift) ? shift : 0;
}

/**
 * Slide the entries of one list to their new places when the SET of entries changes.
 *
 * Without it, deleting, archiving or adding a group made every entry below it jump at
 * once. Each entry carries `data-motion-key`. After a commit that changes `keys`, an
 * entry that moved starts at its old place and slides to the new one, and a new entry
 * fades in. Other commits only record where the entries stand, so opening a band or a
 * poll never animates. Reduced motion turns the effect off.
 */
export function useListMotion(containerRef: RefObject<HTMLElement | null>, keys: readonly string[]): void {
  const signature = keys.join('\n');
  const last = useRef<{ signature: string; tops: Tops } | null>(null);
  const moving = useRef(new Map<HTMLElement, Animation>());
  useLayoutEffect(() => {
    const container = containerRef.current;
    if (!container) {
      last.current = null;
      return;
    }
    const nodes = Array.from(container.querySelectorAll<HTMLElement>(`[${MOTION_KEY}]`));
    const origin = container.getBoundingClientRect().top;
    // Layout places, without the shift a running slide adds; that shift is kept apart so a
    // slide that a second change interrupts continues from where the entry is on screen.
    const shifts = new Map<HTMLElement, number>();
    const tops: Tops = new Map();
    for (const node of nodes) {
      const key = node.getAttribute(MOTION_KEY);
      if (!key) continue;
      const shift = moving.current.has(node) ? shiftOf(node) : 0;
      shifts.set(node, shift);
      tops.set(key, node.getBoundingClientRect().top - origin - shift);
    }
    const before = last.current;
    last.current = { signature, tops };
    if (!before || before.signature === signature || !wantsMotion(container.ownerDocument.defaultView)) return;
    for (const node of nodes) {
      const key = node.getAttribute(MOTION_KEY);
      if (!key || typeof node.animate !== 'function') continue;
      const was = before.tops.get(key);
      const now = tops.get(key) ?? 0;
      const from = was === undefined ? null : was + (shifts.get(node) ?? 0) - now;
      // A slide still running from the last change is replaced, or stopped when the entry
      // is already where it belongs.
      moving.current.get(node)?.cancel();
      if (from !== null && Math.abs(from) < 1) continue;
      const animation =
        from === null
          ? node.animate([{ opacity: 0 }, { opacity: 1 }], { duration: LIST_MOTION_MS, easing: 'ease-out' })
          : node.animate([{ transform: `translateY(${from}px)` }, { transform: 'none' }], {
              duration: LIST_MOTION_MS,
              easing: 'ease-out',
            });
      moving.current.set(node, animation);
      const settle = () => {
        if (moving.current.get(node) === animation) moving.current.delete(node);
      };
      animation.addEventListener?.('finish', settle);
      animation.addEventListener?.('cancel', settle);
    }
  });
}

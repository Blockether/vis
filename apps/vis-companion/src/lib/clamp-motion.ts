import { useLayoutEffect, useRef, useState, type RefObject } from 'react';
import { flushSync } from 'react-dom';

/** How long a body takes to open or close, in milliseconds. */
export const CLAMP_MOTION_MS = 200;

/** The class that cuts a closed body to its first four lines. */
export const CLAMP_CLASS = 'line-clamp-4';

// The ease of the transcript's own entry motion (`--animate-transcript-*` in index.css).
const EASE = 'cubic-bezier(0.22, 0.61, 0.36, 1)';

/** Does this reader want the motion at all? */
function wantsMotion(view: Window | null): boolean {
  return !view?.matchMedia?.('(prefers-reduced-motion: reduce)').matches;
}

/**
 * Open and close a body that shows its first four lines, with its height in motion.
 *
 * A bare class swap changed the height in one frame, so "Show more" and "Show less"
 * made everything under the body jump. Now the body slides from its old height to the
 * new one. A closing body keeps all its text until the slide ends, then it takes the
 * clamp: a clamp at the start would cut the text above empty paper. A second press
 * during a slide continues from the height on screen. Reduced motion swaps at once.
 */
export function useClampMotion(body: RefObject<HTMLElement | null>): {
  isOpen: boolean;
  clampClass: string;
  toggle: () => void;
} {
  const [isOpen, setOpen] = useState(false);
  const [isClamped, setClamped] = useState(true);
  const from = useRef<number | null>(null);
  const motion = useRef<Animation | null>(null);

  useLayoutEffect(() => {
    const element = body.current;
    const start = from.current;
    from.current = null;
    if (!element || start === null) return;
    motion.current?.cancel();
    motion.current = null;
    // A closing body has no clamp yet. Measure the clamped height without a paint.
    if (!isOpen) element.classList.add(CLAMP_CLASS);
    const end = element.getBoundingClientRect().height;
    if (!isOpen) element.classList.remove(CLAMP_CLASS);
    if (
      Math.abs(end - start) < 1 ||
      typeof element.animate !== 'function' ||
      !wantsMotion(element.ownerDocument.defaultView)
    ) {
      if (!isOpen) setClamped(true);
      return;
    }
    const animation = element.animate(
      [
        { height: `${start}px`, overflow: 'hidden' },
        { height: `${end}px`, overflow: 'hidden' },
      ],
      // The end height holds until the clamp lands; without it the full text showed for a frame.
      { duration: CLAMP_MOTION_MS, easing: EASE, fill: 'forwards' },
    );
    motion.current = animation;
    animation.addEventListener('finish', () => {
      if (motion.current !== animation) return;
      motion.current = null;
      if (!isOpen) flushSync(() => setClamped(true));
      animation.cancel();
    });
  }, [body, isOpen]);

  useLayoutEffect(() => () => motion.current?.cancel(), []);

  return {
    isOpen,
    clampClass: isClamped ? CLAMP_CLASS : '',
    toggle: () => {
      const element = body.current;
      if (!element) return;
      // The height on screen, a running slide included.
      from.current = element.getBoundingClientRect().height;
      setOpen((open) => !open);
      setClamped(false);
    },
  };
}

/**
 * The element whose place a scroller keeps while a disclosure opens or closes.
 *
 * It is the body that the toggle controls while the top of that body is in view, so
 * the body opens and closes downward and nothing above it moves. A body that starts
 * above the view keeps the toggle in place instead: "Show less" at the end of a long
 * body then stays under the finger.
 */
export function disclosureAnchor(viewport: Element, toggle: Element): Element {
  const id = toggle.getAttribute('aria-controls');
  const controlled = id ? toggle.ownerDocument.getElementById(id) : null;
  if (!controlled || !viewport.contains(controlled)) return toggle;
  const top = controlled.getBoundingClientRect().top;
  return top >= viewport.getBoundingClientRect().top ? controlled : toggle;
}

/** Is the body that this toggle controls still in motion? */
export function isDisclosureMoving(toggle: Element): boolean {
  const id = toggle.getAttribute('aria-controls');
  const controlled = id ? toggle.ownerDocument.getElementById(id) : null;
  return (controlled?.getAnimations?.() ?? []).some((animation) => animation.playState === 'running');
}

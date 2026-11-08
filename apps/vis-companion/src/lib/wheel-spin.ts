/**
 * A spin of the mouse wheel moves the scroller it started in, also when the pointer
 * drifts over another scroller before the spin stops.
 *
 * WebKit and Chromium latch a scroll gesture to one scroller only when the wheel
 * events carry phases: a trackpad or a Magic Mouse. A plain mouse wheel sends no
 * phases, so the browser hit-tests each notch again. Measured in WebKit: a spin over
 * the transcript that drifted onto the session list scrolled the list 600 px, and the
 * transcript stopped. A guarded scroller gives such a notch back to the scroller of
 * the spin. The rules are the wheel transaction of Firefox.
 */

import { wantsScrollMotion } from './scroll-glide';

/** Wheel events nearer together than this are one spin. */
const SPIN_GAP_MS = 300;

/**
 * Pointer movement this long after the last notch ends the spin. A hand that spins
 * and drifts at the same time stays in the spin; a hand that stops to move goes to
 * the scroller it moved to.
 */
const MOVE_ENDS_SPIN_MS = 150;

/** Pixels in one line of a line-mode wheel event (Chromium's line height). */
const LINE_PX = 40;

/**
 * Where the guarded notches send the scroller of the spin. A smooth `scrollBy` starts
 * from where an earlier glide is now, so a fast spin lost most of its notches (measured:
 * 147 of 500 px in Chromium, 100 of 500 px in WebKit). Each notch moves this target.
 */
let glide: { scroller: Element; top: number } | null = null;

let lastWheelAt = Number.NEGATIVE_INFINITY;

/** The scroller that the current spin started in, or null. */
let spinScroller: Element | null = null;

const SCROLLING = /^(auto|scroll|overlay)$/;

function scrollsVertically(target: EventTarget): target is Element {
  return (
    target instanceof Element &&
    SCROLLING.test(getComputedStyle(target).overflowY) &&
    target.scrollHeight > target.clientHeight
  );
}

function onWheel(event: Event): void {
  const now = Date.now();
  if (now - lastWheelAt > SPIN_GAP_MS) {
    spinScroller = event.composedPath().find(scrollsVertically) ?? null;
    glide = null;
  }
  // The browser moves the scroller of the spin for a notch over it, so a held target is old.
  else if (spinScroller && event.composedPath().includes(spinScroller)) glide = null;
  lastWheelAt = now;
}

function onPointerMove(): void {
  if (Date.now() - lastWheelAt <= MOVE_ENDS_SPIN_MS) return;
  lastWheelAt = Number.NEGATIVE_INFINITY;
  spinScroller = null;
  glide = null;
}

// Captured on `window`, so each spin is known before a guard on its target hears it.
// Passive, because the observer never delays a scroll.
if (typeof window !== 'undefined') {
  const watch = { capture: true, passive: true };
  window.addEventListener('wheel', onWheel, watch);
  window.addEventListener('pointermove', onPointerMove, watch);
}

/**
 * Make `element` give each notch of a spin that started in another scroller back to
 * that scroller. Returns the function that removes the guard. The listener is not
 * passive, so WebKit sends each wheel over `element` to the main thread first: guard
 * only a scroller whose own scroll can wait for that, never the transcript.
 */
export function keepWheelSpin(element: HTMLElement): () => void {
  const onElementWheel = (event: WheelEvent) => {
    const spin = spinScroller;
    if (!spin?.isConnected || event.deltaY === 0) return;
    if (element.contains(spin) || spin.contains(element)) return;
    event.preventDefault();
    const unit =
      event.deltaMode === WheelEvent.DOM_DELTA_LINE
        ? LINE_PX
        : event.deltaMode === WheelEvent.DOM_DELTA_PAGE
          ? spin.clientHeight
          : 1;
    // A notch glides like a notch over the scroller itself. Reduced motion jumps.
    const from = glide?.scroller === spin ? glide.top : spin.scrollTop;
    const top = Math.max(0, Math.min(spin.scrollHeight - spin.clientHeight, from + event.deltaY * unit));
    glide = { scroller: spin, top };
    const view = spin.ownerDocument.defaultView;
    spin.scrollTo({ top, behavior: wantsScrollMotion(view) ? 'smooth' : 'auto' });
  };
  element.addEventListener('wheel', onElementWheel, { capture: true, passive: false });
  return () => element.removeEventListener('wheel', onElementWheel, { capture: true });
}

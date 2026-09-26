/**
 * While the reader manipulates the transcript, automatic pinning and anchoring stand
 * down through a short post-gesture grace period so the resulting position can be
 * measured without a correction loop.
 */

/** Grace long enough to outlive the next animation-frame measurement. */
const GESTURE_GRACE_MS = 300;

let lastGestureAt = Number.NEGATIVE_INFINITY;

/** Record a real reader gesture. Exported for the screens' own handlers. */
export function noteReaderGesture(): void {
  lastGestureAt = Date.now();
}

/** True while the reader's own gesture owns the scroll position. */
export function readerOwnsScroll(): boolean {
  if (dragging) return true;
  return Date.now() - lastGestureAt <= GESTURE_GRACE_MS;
}

/**
 * How long after the reader's last input a moving scroller can still be their doing.
 * Momentum keeps a flick going once the finger has lifted, the native scroller keeps
 * following a finger JS was told was cancelled, and a main thread busy committing a
 * running turn can deliver the first scroll event of either after the grace above.
 */
const MOVEMENT_REACH_MS = 1_000;

/**
 * Whether a scroll that moves the transcript now may still come from the reader.
 * Outside this, a move the layout explains — the browser clamping `scrollTop` to an
 * end that dipped for one forced layout — is nobody's gesture.
 */
export function readerMayBeScrolling(): boolean {
  if (dragging) return true;
  return Date.now() - lastGestureAt <= MOVEMENT_REACH_MS;
}

/**
 * Only MOVEMENT counts, never a press. A tap on send, on a disclosure, on the
 * “↓ Latest” button is not a scroll, and treating it as one would stand the
 * catch-ups down exactly when they were asked for — the pin that a send sets up
 * would be cancelled by the tap that requested it.
 */
function onReaderMove(event: Event): void {
  if (event.type === 'pointermove' && (event as PointerEvent).buttons === 0) {
    // No button is down now, whatever `pointerup` said or never said.
    onPointerUp(event);
    return;
  }
  noteReaderGesture();
}

/**
 * The keyboard scrolls as well: the arrows, Page Up/Down, Home, End and Space move
 * the scroller that holds the focus, and Tab brings the control it focuses into
 * view. In a text field the same keys move the caret, and Space presses a button.
 */
const SCROLL_KEYS = new Set(['ArrowDown', 'ArrowUp', 'End', 'Home', 'PageDown', 'PageUp', ' ']);

function keyScrolls(key: string, target: EventTarget | null): boolean {
  if (key === 'Tab') return true;
  if (!SCROLL_KEYS.has(key)) return false;
  if (!(target instanceof HTMLElement)) return true;
  if (target.isContentEditable || /^(INPUT|SELECT|TEXTAREA)$/.test(target.tagName)) return false;
  return key !== ' ' || !target.matches('button, summary, [role="button"]');
}

function onKeyDown(event: Event): void {
  const { key, target } = event as KeyboardEvent;
  if (keyScrolls(key, target)) noteReaderGesture();
}

/**
 * A finger on the glass is not yet a gesture — a tap is not a scroll — but a
 * finger that HAS moved the scroller owns it until it lifts, and that has to be
 * tracked separately from `touchmove`: WebKit may hand the drag to the native
 * scroller and stop sending `touchmove` (it sends `touchcancel` instead), so a
 * long slow drag can go quiet in JS while the scroller is still following the
 * finger. “It scrolled while a finger was down” survives that; `touchmove` is
 * only the earliest notice of it. A mouse or pen button held down counts the same
 * way: it drags a scrollbar, or selects text past the edge the scroller follows,
 * and the browser need not report that movement to the page.
 */
let touchesDown = 0;
let pointerHeld = false;
let dragging = false;

/** A new scroll surface cannot inherit the gesture that owned the previous one. */
export function releaseReaderScroll(): void {
  lastGestureAt = Number.NEGATIVE_INFINITY;
  touchesDown = 0;
  pointerHeld = false;
  dragging = false;
}

/** When the last finger or button lets go, a drag it made keeps the grace. */
function letGo(): void {
  if (touchesDown > 0 || pointerHeld) return;
  if (dragging) noteReaderGesture();
  dragging = false;
}

function onTouchStart(event: Event): void {
  touchesDown = (event as TouchEvent).touches?.length ?? touchesDown + 1;
}

// `touchcancel` counts as a lift so the count can never leak into a permanent
// veto. It is also the native scroller taking the drag over, so it restarts the
// reader's reach: the scroller may go on following that finger in silence.
function onTouchEnd(event: Event): void {
  touchesDown = (event as TouchEvent).touches?.length ?? 0;
  if (event.type === 'touchcancel') noteReaderGesture();
  letGo();
}

// Touch has its own count above, which survives WebKit taking the drag over.
function onPointerDown(event: Event): void {
  if ((event as PointerEvent).pointerType !== 'touch') pointerHeld = true;
}

function onPointerUp(event: Event): void {
  if ((event as PointerEvent).pointerType === 'touch' || !pointerHeld) return;
  pointerHeld = false;
  letGo();
}

function onScroll(): void {
  if (touchesDown === 0 && !pointerHeld) return;
  dragging = true;
  noteReaderGesture();
}

// Captured on `window` so this holds wherever the gesture starts — a code block,
// an image, a nested scroller — and so nothing downstream can hide it by
// stopping propagation: window capture runs first. Passive, because an observer
// must never be able to delay the scroll it is only watching. `scroll` does not
// bubble at all, which is the other reason it is captured here.
if (typeof window !== 'undefined') {
  const watch = { capture: true, passive: true };
  for (const type of ['touchmove', 'wheel', 'pointermove']) {
    window.addEventListener(type, onReaderMove, watch);
  }
  window.addEventListener('touchstart', onTouchStart, watch);
  for (const type of ['touchend', 'touchcancel']) {
    window.addEventListener(type, onTouchEnd, watch);
  }
  window.addEventListener('pointerdown', onPointerDown, watch);
  for (const type of ['pointerup', 'pointercancel']) {
    window.addEventListener(type, onPointerUp, watch);
  }
  window.addEventListener('keydown', onKeyDown, watch);
  window.addEventListener('scroll', onScroll, watch);
}

// Keeping the reader's line while content lands above them is NOT here, and no
// component may do it locally. Rows arrive from three directions — history
// prepends, the backfill that refills the render window, and traces ramping
// their segments — and every corrector that compensates "its own" growth bills
// the same frame again: measured, a 39 730 px "↑ Load earlier" walked the
// scroller 59 910 px and left the reader 20 000 px past their line. The screen
// that owns the scroller anchors it once, in one ResizeObserver, with
// `scrollAnchorFor`/`applyScrollAnchor` from `lib/viewport`.

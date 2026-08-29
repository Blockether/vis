/**
 * Who owns the scroll position right now: the reader's hand, or the code that
 * keeps a live transcript pinned to its end?
 *
 * The transcript scroller is moved by several independent catch-ups, each
 * written for a scroller nobody is touching:
 *
 *   - opening a session pins to the end on a settle schedule (`SessionScreen`),
 *   - the turn backfill re-pins after every chunk it hydrates — one per frame,
 *   - a resizing composer or keyboard re-pins a reader who was at the end,
 *   - `ChatContent`'s segment ramp holds the reader's line while segments mount
 *     above them, and pins to the end when they were already there.
 *
 * Under a finger they compound, and they do not merely cost a frame of the
 * drag. `scrollToEnd` re-asserts "this reader is following the end", and the
 * scroll event its correction emits measures distance-to-bottom 0 — so the next
 * chunk is just as certain it is helping, and there is a chunk every frame.
 * That loop is why dragging up during the first second of a large session did
 * nothing at all: the drag moved the scroller, the next frame put it back. The
 * ramp's own hold had the mirror-image bug for a slow drag, which moves less
 * than the "still at the end" tolerance within a single frame.
 *
 * So the fact lives in one place. While the reader is working the scroller —
 * and for a short grace afterwards, because whether they are still at the end
 * is only measured a frame later — every automatic scroll stands down and lets
 * the position that the gesture produced be the one that gets measured.
 */

/**
 * How long after the last movement the reader keeps the scroller. It only has
 * to outlive the `requestAnimationFrame` in which the screen measures where the
 * gesture left them; momentum past that is judged on distance-to-bottom, like
 * any other scroll.
 */
const GESTURE_GRACE_MS = 300;

let lastGestureAt = Number.NEGATIVE_INFINITY;

/** Record a real reader gesture. Exported for the screens' own handlers. */
export function noteReaderGesture(): void {
  lastGestureAt = Date.now();
}

/** True while the reader's own gesture owns the scroll position. */
export function readerOwnsScroll(): boolean {
  if (draggingUnderTouch) return true;
  return Date.now() - lastGestureAt <= GESTURE_GRACE_MS;
}

/**
 * Only MOVEMENT counts, never a press. A tap on send, on a disclosure, on the
 * “↓ Latest” button is not a scroll, and treating it as one would stand the
 * catch-ups down exactly when they were asked for — the pin that a send sets up
 * would be cancelled by the tap that requested it.
 */
function onReaderMove(event: Event): void {
  if (event.type === "pointermove" && (event as PointerEvent).buttons === 0)
    return;
  noteReaderGesture();
}

/**
 * A finger on the glass is not yet a gesture — a tap is not a scroll — but a
 * finger that HAS moved the scroller owns it until it lifts, and that has to be
 * tracked separately from `touchmove`: WebKit may hand the drag to the native
 * scroller and stop sending `touchmove` (it sends `touchcancel` instead), so a
 * long slow drag can go quiet in JS while the scroller is still following the
 * finger. “It scrolled while a finger was down” survives that; `touchmove` is
 * only the earliest notice of it.
 */
let touchesDown = 0;
let draggingUnderTouch = false;

/** A new scroll surface cannot inherit the gesture that owned the previous one. */
export function releaseReaderScroll(): void {
  lastGestureAt = Number.NEGATIVE_INFINITY;
  touchesDown = 0;
  draggingUnderTouch = false;
}

function onTouchStart(event: Event): void {
  touchesDown = (event as TouchEvent).touches?.length ?? touchesDown + 1;
}

// `touchcancel` counts as a lift so the count can never leak into a permanent
// veto; the drag it usually announces keeps the ordinary grace window instead.
function onTouchEnd(event: Event): void {
  touchesDown = (event as TouchEvent).touches?.length ?? 0;
  if (touchesDown > 0) return;
  if (draggingUnderTouch) noteReaderGesture();
  draggingUnderTouch = false;
}

function onScroll(): void {
  if (touchesDown === 0) return;
  draggingUnderTouch = true;
  noteReaderGesture();
}

// Captured on `window` so this holds wherever the gesture starts — a code block,
// an image, a nested scroller — and so nothing downstream can hide it by
// stopping propagation: window capture runs first. Passive, because an observer
// must never be able to delay the scroll it is only watching. `scroll` does not
// bubble at all, which is the other reason it is captured here.
if (typeof window !== "undefined") {
  for (const type of ["touchmove", "wheel", "pointermove"]) {
    window.addEventListener(type, onReaderMove, {
      capture: true,
      passive: true,
    });
  }
  window.addEventListener("touchstart", onTouchStart, {
    capture: true,
    passive: true,
  });
  for (const type of ["touchend", "touchcancel"]) {
    window.addEventListener(type, onTouchEnd, { capture: true, passive: true });
  }
  window.addEventListener("scroll", onScroll, { capture: true, passive: true });
}

// Keeping the reader's line while content lands above them is NOT here, and no
// component may do it locally. Rows arrive from three directions — history
// prepends, the backfill that refills the render window, and traces ramping
// their segments — and every corrector that compensates "its own" growth bills
// the same frame again: measured, a 39 730 px "↑ Load earlier" walked the
// scroller 59 910 px and left the reader 20 000 px past their line. The screen
// that owns the scroller anchors it once, in one ResizeObserver, with
// `scrollAnchorFor`/`applyScrollAnchor` from `lib/viewport`.

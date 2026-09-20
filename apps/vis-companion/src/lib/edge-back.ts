/**
 * COMING BACK IS A STROKE TOO. On a phone the transcript IS the screen, so the
 * way out of a session is one arrow at the top of the glass — and the hand that
 * is already holding the phone answers "back" with a finger drawn in from the
 * left edge, because that is what every other app on that phone answers to.
 *
 * Nothing native offers it here: a Capacitor shell is ONE web page with no
 * navigation stack under it, so a swipe means exactly what this page decides it
 * means. It is read from touches and it commits on the LIFT — the same shape as
 * `lib/pull-to-search` — so a stroke begun by accident can always be taken back
 * by carrying the finger home again.
 */

import { useEffect, useRef, useState } from 'react';

/** A finger, in client coordinates. */
export interface EdgePoint {
  readonly x: number;
  readonly y: number;
}

/** The pane being left, seen from here: only where its leading edge stands decides anything. */
export interface EdgeBox {
  readonly left: number;
}

/** Where a back swipe may begin — a thumb's width in from the pane's leading edge. */
export const EDGE_ZONE_PX = 24;

/**
 * How far across the finger must carry before the lift leaves the session. It is
 * one unhurried thumb sweep on the narrowest phone this app runs on, and long
 * enough that a tap which slid a little is never a navigation.
 */
export const EDGE_BACK_PX = 64;

/** Carried back toward the edge past this, and the reader has taken the stroke back. */
const HOMEWARD_PX = 8;

/** Up or down past this, and further than it has come across: the transcript is being scrolled. */
const VERTICAL_PX = 16;

/** Whether a lift right now would leave the session. */
export type EdgePhase = 'none' | 'armed';

export interface EdgeGesture {
  /** Where the finger landed, so every reading is measured from one origin. */
  readonly from: EdgePoint;
  readonly phase: EdgePhase;
  /** How far the finger has come in from `from`. */
  readonly across: number;
}

/**
 * Begin watching a finger, or refuse it. Only the pane's own edge strip asks a
 * question about navigation: a finger further in is reading, and a finger that
 * landed before the pane begins belongs to whatever stands there. A second
 * finger is a pinch, which is never a way out.
 */
export function edgeStart(box: EdgeBox, touches: number, at: EdgePoint): EdgeGesture | null {
  if (touches !== 1) return null;
  const into = at.x - box.left;
  if (into < 0 || into > EDGE_ZONE_PX) return null;
  return { from: at, phase: 'none', across: 0 };
}

/**
 * Where the stroke stands after a move, or `null` once it can no longer be a way
 * back. The phase falls back as well as forward, which is the whole point of
 * committing on the lift.
 */
export function edgeMove(gesture: EdgeGesture, touches: number, at: EdgePoint): EdgeGesture | null {
  if (touches !== 1) return null;
  const across = at.x - gesture.from.x;
  const vertical = Math.abs(at.y - gesture.from.y);
  if (across < -HOMEWARD_PX) return null;
  if (vertical > VERTICAL_PX && vertical > across) return null;
  return { from: gesture.from, phase: across >= EDGE_BACK_PX ? 'armed' : 'none', across };
}

/**
 * Is the edge strip FREE to mean "back" where this touch landed?
 *
 * Two things under the finger already own a sideways drag, and both would rather
 * be scrolled than navigated away from:
 *
 *  - anything the reader can scroll sideways — a code block, a wide table, the
 *    row of actions a session card slides open;
 *  - anything that took the touch away from the browser (`touch-action: none`)
 *    because it drives the picture itself, like the zoomable image viewer.
 *
 * A dialog standing over the session answers for the whole screen while it is
 * up, so the session underneath is not the thing being left.
 */
export function edgeIsFree(from: EventTarget | null, root: Element): boolean {
  if (root.ownerDocument.querySelector('[aria-modal="true"]')) return false;
  let node = from instanceof Element ? from : null;
  while (node && node !== root) {
    const style = getComputedStyle(node);
    if (style.touchAction === 'none' || style.touchAction === 'pan-y') return false;
    const scrolls = style.overflowX === 'auto' || style.overflowX === 'scroll';
    if (scrolls && node.scrollWidth > node.clientWidth + 1) return false;
    node = node.parentElement;
  }
  return true;
}

/**
 * Watch the pane for a swipe in from its leading edge and leave the session when
 * one is released. Answers the ref to PUT ON THAT PANE.
 *
 * `onBack` is `null` wherever the gesture has nothing to say: on a desk the list
 * is already standing beside the transcript, so there is nowhere to come back
 * to, and with no session open there is nothing to leave. Nothing is watched
 * then.
 *
 * The pane is held as STATE rather than read from a `RefObject`, because it
 * arrives LATER than this hook's first call — a session opens, its stream comes
 * up, and only then is there a pane — and a ref filled silently would leave the
 * listeners on nothing.
 *
 * The listeners are passive and captured: passive because a gesture that only
 * WATCHES must never be able to delay the scroll it is watching, captured so a
 * component inside the transcript cannot hide the stroke by stopping it on its
 * way up.
 */
export function useEdgeBack(onBack: (() => void) | null): (pane: HTMLElement | null) => void {
  // Read only from an event, so it is kept in a ref instead of resubscribing the
  // listeners every time the shell re-renders.
  const latest = useRef(onBack);
  useEffect(() => {
    latest.current = onBack;
  });

  const hasDoor = onBack !== null;
  const [pane, setPane] = useState<HTMLElement | null>(null);

  useEffect(() => {
    if (!pane || !hasDoor) return;
    let gesture: EdgeGesture | null = null;

    const pointOf = (event: TouchEvent): EdgePoint | null => {
      const touch = event.touches[0];
      return touch ? { x: touch.clientX, y: touch.clientY } : null;
    };

    const onStart = (event: Event) => {
      const touchEvent = event as TouchEvent;
      const at = pointOf(touchEvent);
      const started = at
        ? edgeStart(pane.getBoundingClientRect(), touchEvent.touches.length, at)
        : null;
      // The strip is asked about FIRST, so the walk up the tree only happens for
      // the few touches that landed where the gesture could begin at all.
      gesture = started && edgeIsFree(touchEvent.target, pane) ? started : null;
    };

    const onMove = (event: Event) => {
      if (!gesture) return;
      const touchEvent = event as TouchEvent;
      const at = pointOf(touchEvent);
      gesture = at ? edgeMove(gesture, touchEvent.touches.length, at) : null;
    };

    // The lift is the commitment.
    const onEnd = () => {
      const armed = gesture?.phase === 'armed';
      gesture = null;
      if (armed) latest.current?.();
    };

    // A cancel is not a lift. WebKit sends one when it hands the drag to a native
    // scroller, and a stroke the browser took away was never released here.
    const onCancel = () => {
      gesture = null;
    };

    pane.addEventListener('touchstart', onStart, { capture: true, passive: true });
    pane.addEventListener('touchmove', onMove, { capture: true, passive: true });
    pane.addEventListener('touchend', onEnd, { capture: true, passive: true });
    pane.addEventListener('touchcancel', onCancel, { capture: true, passive: true });
    return () => {
      pane.removeEventListener('touchstart', onStart, true);
      pane.removeEventListener('touchmove', onMove, true);
      pane.removeEventListener('touchend', onEnd, true);
      pane.removeEventListener('touchcancel', onCancel, true);
    };
  }, [hasDoor, pane]);

  return setPane;
}

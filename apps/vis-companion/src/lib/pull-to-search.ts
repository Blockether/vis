/**
 * PULL THE LIST DOWN AND THE SEARCH ARRIVES.
 *
 * The fleet-wide search is a PAGE and the app bar is its door (`Header` in
 * `App.tsx`): one magnifying glass in the far top corner of a 390px phone, which
 * is the corner a thumb reading the list cannot reach. Every native list that
 * reader uses answers the same question with the same gesture — pull a list that
 * is already at its top and the search comes down over it. This module is that
 * gesture, and it opens the very door the glass opens; nothing else about the
 * screen changes.
 *
 * Three facts decide whether a finger is asking for it:
 *
 *   - the list must ALREADY be at the top. A pull anywhere else is scrolling,
 *     and a screen that answered it with a search page would be unreadable.
 *   - the drag must be DOWNWARD and roughly straight. A session row is a
 *     horizontal swipe of its own (`components/SwipeActions`), so a gesture that
 *     commits sideways belongs to the row it started on and is dropped here.
 *   - it commits on the LIFT, never on the pixel that crosses the threshold.
 *     That is what makes it cancellable — drag back up and the hint stands down
 *     with nothing opened — and it is the ordering that raises a phone keyboard:
 *     the caret is put in the field from `touchend`, the discrete event React
 *     flushes inside the gesture itself, exactly as a press on the glass does it.
 *
 * The gesture REPORTS ITSELF WHILE IT HAPPENS (`PullToSearchHint`), and it
 * reports by FOLLOWING THE FINGER: the band comes down the exact fraction of
 * the way the pull has come, so the hand is moving the band rather than
 * tripping a switch that then plays a canned slide of its own. Past the
 * threshold the band gives only a third of what the finger asks for, and that
 * resistance is the detent this app has no haptics to tick. The screen is the
 * only confirmation there is, and a pull that showed nothing until the finger
 * lifted would be a gesture nobody could learn and nobody could trust.
 */

import { useEffect, useRef, type RefObject } from 'react';

/** The list, seen from here: only where it is parked decides anything. */
export interface PullBox {
  readonly scrollTop: number;
}

/** A finger, in client coordinates. */
export interface PullPoint {
  readonly x: number;
  readonly y: number;
}

/**
 * What the pull is doing right now, and therefore what the screen says:
 * nothing, `Pull to search`, or `Release to search`.
 */
export type PullPhase = 'none' | 'pulling' | 'armed';

export interface PullGesture {
  /** Where the finger landed, so every reading is measured from one origin. */
  readonly from: PullPoint;
  readonly phase: PullPhase;
  /** How far the finger has come down from `from` — the band's whole position. */
  readonly down: number;
}

/**
 * Under this the list is at the top for every human purpose — the same tolerance
 * `lib/list-scroll` uses to decide a reader parked nowhere worth remembering.
 */
export const AT_TOP_PX = 2;

/** Past this the pull is deliberate, and the hint comes down to say so. */
export const PULL_HINT_PX = 16;

/**
 * Past this the lift opens the search. It is longer than the band it slides over
 * (52px) so the hint is fully read before it can be committed to, and it is
 * still one comfortable thumb travel on the shortest phone this app runs on.
 */
export const PULL_OPEN_PX = 72;

/** Sideways past this, and further sideways than down: the row's swipe, not ours. */
const SIDEWAYS_PX = 12;

/** Upward past this, the finger is scrolling into the list and has left us. */
const UPWARD_PX = 4;

/**
 * Begin watching a finger, or refuse it. A second finger is a pinch and a list
 * that is not at its top is being scrolled; neither is a question about search.
 */
export function pullStart(box: PullBox, touches: number, at: PullPoint): PullGesture | null {
  if (touches !== 1) return null;
  if (box.scrollTop > AT_TOP_PX) return null;
  return { from: at, phase: 'none', down: 0 };
}

/**
 * Where the gesture stands after a move, or `null` once it can no longer be a
 * pull. The phase falls back as well as forward: a finger dragged back up
 * disarms, which is the whole point of committing on the lift.
 */
export function pullMove(gesture: PullGesture, touches: number, at: PullPoint): PullGesture | null {
  if (touches !== 1) return null;
  const down = at.y - gesture.from.y;
  const sideways = Math.abs(at.x - gesture.from.x);
  if (down < -UPWARD_PX) return null;
  if (sideways > SIDEWAYS_PX && sideways > down) return null;
  const phase: PullPhase =
    down >= PULL_OPEN_PX ? 'armed' : down >= PULL_HINT_PX ? 'pulling' : 'none';
  return { from: gesture.from, phase, down };
}

/**
 * Past the open threshold the band GIVES rather than keeps up: about a third of
 * every further pixel, and never more than this much of its own height out. A
 * pull that tracked one-to-one forever would run the band down over the list
 * with nothing left to say, where resistance is how a hand feels an end stop on
 * a screen with no haptics to tick one.
 */
const OVERPULL_GIVE = 0.35;
const MAX_REVEAL = 1.12;

/**
 * How much of the band is out, as a fraction of its own height: 0 where the
 * finger landed, 1 exactly where a lift would open the search. It is the single
 * number the screen is painted from, so what the band SHOWS can never disagree
 * with what the phase SAYS.
 */
export function pullReveal(down: number): number {
  if (down <= 0) return 0;
  const reached = down / PULL_OPEN_PX;
  if (reached <= 1) return reached;
  return Math.min(MAX_REVEAL, 1 + (reached - 1) * OVERPULL_GIVE);
}

/**
 * Put the band where the finger has dragged it, or hand it back to its class.
 *
 * While a finger is down the element carries its own position and the
 * transition is switched OFF: a 150ms ease between the frames of a live drag is
 * exactly the lag that reads as a gesture the phone is only pretending to
 * follow. `null` clears both properties, and the class the band declares takes
 * over WITH its transition — which is the same thing as gliding home, whether
 * the pull was abandoned or the search page is on its way in.
 *
 * POSITION IS THE WHOLE REVEAL: the list card that owns the band is
 * `overflow-hidden`, so a band parked one height up is not dimmed, it is simply
 * behind the edge. Fading it in as well only laid a ghost of the header under
 * the paper of the band coming down to cover it.
 */
export function paintPull(element: HTMLElement | null, reveal: number | null): void {
  if (!element) return;
  if (reveal === null) {
    element.style.transitionDuration = '';
    element.style.translate = '';
    return;
  }
  element.style.transitionDuration = '0ms';
  element.style.translate = `0px ${Math.round((reveal - 1) * 10000) / 100}%`;
}

/**
 * Wire the gesture to `viewport`, reporting every phase change through
 * `onPhase`, painting `hint` wherever the finger has dragged it, and opening
 * the search on the lift that ends an armed pull.
 *
 * `onSearch` is `null` when the search page is ALREADY the screen: there is no
 * door left to open, so nothing is watched and nothing is hinted — a hint that
 * promised a page the reader is standing on would be the screen telling a lie.
 *
 * The listeners are passive. They never fight the scroller and they never need
 * to: at the top of the list a downward drag moves nothing (`overscroll-contain`
 * on the scroller, `overscroll-behavior: none` on the shell), which is exactly
 * why this gesture is free to mean something.
 *
 * The band is painted STRAIGHT ONTO THE ELEMENT rather than through React state
 * — `components/ImageViewer` carries a pinched picture the same way — because a
 * re-render per `touchmove` stutters on exactly the phones this gesture exists
 * for. The class on the band still owns where it RESTS, so a lift only has to
 * hand it back and it glides home under the transition it declares itself.
 */
export function usePullToSearch(
  viewport: RefObject<HTMLElement | null>,
  hint: RefObject<HTMLElement | null>,
  onPhase: (phase: PullPhase) => void,
  onSearch: (() => void) | null,
): void {
  // Read only from an event, so they are kept in a ref rather than in the
  // wiring effect's dependencies: a list this long must not resubscribe four
  // listeners because a parent re-rendered.
  const latest = useRef({ onPhase, onSearch });
  useEffect(() => {
    latest.current = { onPhase, onSearch };
  });

  const hasDoor = onSearch !== null;
  useEffect(() => {
    const element = viewport.current;
    if (!element || !hasDoor) return;
    let gesture: PullGesture | null = null;

    // One place decides what the screen is told AND what it is painted with, so
    // the hint can never be left standing by a path that forgot to clear it,
    // nor left behind the finger by a path that only reported a phase.
    const paint = (next: PullGesture | null) =>
      paintPull(hint.current, next && pullReveal(next.down));
    const report = (next: PullGesture | null) => {
      const was = gesture?.phase ?? 'none';
      gesture = next;
      const now = next?.phase ?? 'none';
      paint(next);
      if (now !== was) latest.current.onPhase(now);
    };

    const pointOf = (event: TouchEvent): PullPoint | null => {
      const touch = event.touches[0];
      return touch ? { x: touch.clientX, y: touch.clientY } : null;
    };

    const onStart = (event: Event) => {
      const touchEvent = event as TouchEvent;
      const at = pointOf(touchEvent);
      report(at ? pullStart(element, touchEvent.touches.length, at) : null);
    };

    const onMove = (event: Event) => {
      if (!gesture) return;
      const touchEvent = event as TouchEvent;
      const at = pointOf(touchEvent);
      report(at ? pullMove(gesture, touchEvent.touches.length, at) : null);
    };

    // The lift is the commitment. The hint is taken down first so the search
    // page does not open underneath a band still saying how to open it.
    const onEnd = () => {
      const armed = gesture?.phase === 'armed';
      report(null);
      if (armed) latest.current.onSearch?.();
    };

    // A cancel is not a lift. WebKit sends one when it hands a drag to a native
    // scroller, and a gesture the browser took away was never released here.
    const onCancel = () => report(null);

    element.addEventListener('touchstart', onStart, { passive: true });
    element.addEventListener('touchmove', onMove, { passive: true });
    element.addEventListener('touchend', onEnd, { passive: true });
    element.addEventListener('touchcancel', onCancel, { passive: true });
    return () => {
      element.removeEventListener('touchstart', onStart);
      element.removeEventListener('touchmove', onMove);
      element.removeEventListener('touchend', onEnd);
      element.removeEventListener('touchcancel', onCancel);
      report(null);
    };
  }, [hasDoor, hint, viewport]);
}

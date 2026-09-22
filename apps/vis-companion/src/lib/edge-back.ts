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
 *
 * AND IT HAS TO LOOK LIKE THE PHONE'S OWN. A way back that only jumps reads as a
 * bug on iOS, so the stroke DRAGS: the transcript rides the finger with the list
 * already standing behind it — a third of the width back and dimmed, the way
 * UIKit parks the page underneath — and the lift finishes the pop in whichever
 * direction the finger was going, at the speed it was going.
 *
 * The numbers are UIKit's, read off the implementation Apple's own shape is
 * measured against (`ios.transition.ts` and `swipe-back.ts` in Ionic): a 0.33
 * parallax, a 0.8 dim under the page being left, `cubic-bezier(0.32, 0.72, 0, 1)`
 * and at most 540ms for the rest of the way. A reader who asked for less motion
 * keeps the plain step, with no slide at all.
 */

import { useCallback, useEffect, useRef, useState } from 'react';

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
 * up, so the session underneath is not the thing being left. The dialog is the
 * one exception, and it is its own: a stroke dragging the DIALOG is leaving the
 * dialog, which is what `isLayer` says.
 */
export function edgeIsFree(from: EventTarget | null, root: Element, isLayer = false): boolean {
  if (!isLayer && root.ownerDocument.querySelector('[aria-modal="true"]')) return false;
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

/** The curve a page rides on iOS, in and out of the stack. */
export const EDGE_EASING = 'cubic-bezier(0.32, 0.72, 0, 1)';

/** The longest the rest of the way back ever takes, in ms. */
export const EDGE_SETTLE_MS = 540;

/** How far behind the page being left the one underneath waits: a third of the width. */
export const EDGE_PARALLAX = 0.33;

/** How dim that page is while it waits, and 1 once it is back in front. */
export const EDGE_UNDER_DIM = 0.8;

/** A throw, in px/ms: past this speed the lift completes however short the stroke was. */
export const EDGE_FLICK_PX_MS = 0.2;

/**
 * No finger travels faster than this, in px/ms. A touch stream whose timestamps
 * land in the same millisecond would otherwise read as an infinite throw and
 * finish the stroke inside a single frame.
 */
const EDGE_TOP_PX_MS = 4;

/** The shadow the leading edge of the page being left casts on the list behind it. */
const EDGE_SHADOW = '-12px 0 24px rgba(0, 0, 0, 0.28)';

/** How far across the stroke has to come before the panes are set up to be dragged. */
const EDGE_OPEN_PX = 4;

/**
 * Would a lift right here leave the session? iOS asks two questions of a back
 * swipe: has the page come far enough to stay gone, or was it THROWN — a flick
 * completes from anywhere, and a finger travelling back toward the edge never
 * completes, however far out it had already carried the page.
 */
export function edgeCompletes(across: number, velocity: number): boolean {
  if (velocity < 0) return false;
  if (across >= EDGE_BACK_PX) return true;
  return velocity > EDGE_FLICK_PX_MS && across > HOMEWARD_PX;
}

/**
 * How long the rest of the stroke takes. It carries on at the speed the finger
 * was already travelling, never longer than a full-length pop — and a page that
 * is as good as home is simply put there.
 */
export function edgeSettleMs(remainingPx: number, velocity: number): number {
  if (remainingPx <= 5) return 0;
  const speed = Math.abs(velocity);
  if (speed < 0.01) return EDGE_SETTLE_MS;
  return Math.min(Math.round(remainingPx / speed), EDGE_SETTLE_MS);
}

/**
 * The two panes for the length of one stroke: the transcript being dragged out
 * and the list waiting under it, each with the inline styles it had before, so
 * the end of the stroke can hand them back untouched.
 */
interface EdgeStage {
  readonly pane: HTMLElement;
  readonly under: HTMLElement | null;
  /** How far the transcript has to travel to be gone. */
  readonly width: number;
  readonly paneStyle: string;
  readonly underStyle: string;
}

/**
 * Lift the transcript out of the layout and onto the shell, pinned to the box it
 * is standing in right now.
 *
 * It has to leave the flow before anything else moves: the list coming back
 * brings the app bar with it, and a pane still in that column would be pushed
 * down by the bar's height the moment the shell paints it. Pinned to the shell —
 * which is the element the app's own geometry is kept on — it covers the bar as
 * a page does on iOS, and nothing underneath can shift it.
 *
 * A LAYER — a dialog, an opened artifact — is already out of the flow and
 * already pinned to the host it was portalled into, so it is dragged where it
 * stands: putting it on the shell as well would move it by the distance between
 * the two.
 */
function edgeOpen(pane: HTMLElement, under: HTMLElement | null, isLayer: boolean): EdgeStage {
  const view = pane.ownerDocument.defaultView;
  const box = pane.getBoundingClientRect();
  const shell = pane.closest('[data-viewport-shell]');
  const frame = shell?.getBoundingClientRect();
  const width = box.width || frame?.width || view?.innerWidth || 0;
  const height = box.height || frame?.height || view?.innerHeight || 0;
  const stage: EdgeStage = {
    pane,
    under,
    width,
    paneStyle: pane.style.cssText,
    underStyle: under?.style.cssText ?? '',
  };

  if (frame && !isLayer) {
    pane.style.position = 'absolute';
    pane.style.left = `${box.left - frame.left}px`;
    pane.style.top = `${box.top - frame.top}px`;
    pane.style.width = `${width}px`;
    pane.style.height = `${height}px`;
    pane.style.margin = '0';
    pane.style.zIndex = '30';
  }
  pane.style.transition = '';
  pane.style.willChange = 'transform';
  pane.style.boxShadow = EDGE_SHADOW;
  if (under) {
    under.style.transition = '';
    under.style.willChange = 'transform, opacity';
  }
  return stage;
}

/** Stand both panes where a stroke this far across puts them. */
function edgePaint(stage: EdgeStage, across: number): void {
  const travelled = Math.max(0, Math.min(across, stage.width));
  const step = stage.width > 0 ? travelled / stage.width : 0;
  stage.pane.style.transform = `translateX(${travelled}px)`;
  if (!stage.under) return;
  stage.under.style.transform = `translateX(${-(1 - step) * stage.width * EDGE_PARALLAX}px)`;
  stage.under.style.opacity = `${EDGE_UNDER_DIM + (1 - EDGE_UNDER_DIM) * step}`;
}

/** Carry both panes the rest of the way, out of the stack or home again. */
function edgeSettle(stage: EdgeStage, completes: boolean, ms: number): void {
  const ride = ms > 0 ? `transform ${ms}ms ${EDGE_EASING}, opacity ${ms}ms ${EDGE_EASING}` : '';
  stage.pane.style.transition = ride;
  if (stage.under) stage.under.style.transition = ride;
  edgePaint(stage, completes ? stage.width : 0);
}

/** Give both panes back exactly the inline styles they arrived with. */
function edgeClose(stage: EdgeStage): void {
  stage.pane.style.cssText = stage.paneStyle;
  if (stage.under) stage.under.style.cssText = stage.underStyle;
}

/** Does this reader want the motion at all? */
function wantsMotion(view: Window | null): boolean {
  return !view?.matchMedia?.('(prefers-reduced-motion: reduce)').matches;
}

export interface EdgeBackPanes {
  /** Put this on the pane the stroke LEAVES — the transcript. */
  readonly pane: (element: HTMLElement | null) => void;
  /** Put this on the pane it comes back TO — the list, mounted behind it all along. */
  readonly under: (element: HTMLElement | null) => void;
  /** A stroke is in flight: the list and the app bar it returns to must be on the glass. */
  readonly isSwiping: boolean;
}

export interface EdgeBackOptions {
  /**
   * The pane is a layer standing OVER the application — a dialog, an opened
   * artifact — rather than a page in the flow under it. What such a pane
   * uncovers is already on screen behind it, so `under` goes unused.
   */
  readonly isLayer?: boolean;
}

/**
 * Watch the pane for a swipe in from its leading edge, drag it with the finger,
 * and leave the session when the stroke completes. Answers the refs to PUT ON
 * THOSE PANES.
 *
 * `onBack` is `null` wherever the gesture has nothing to say: on a desk the list
 * is already standing beside the transcript, so there is nowhere to come back
 * to, and with no session open there is nothing to leave. Nothing is watched
 * then.
 *
 * The transcript pane is held as STATE rather than read from a `RefObject`,
 * because it arrives LATER than this hook's first call — a session opens, its
 * stream comes up, and only then is there a pane — and a ref filled silently
 * would leave the listeners on nothing. The list behind it is only ever read
 * inside a stroke, so a ref is enough for that one.
 *
 * `isSwiping` turns on at the FIRST MOVE, not at the touch: a tap that lands in
 * the edge strip must not make the shell re-render, and by the time the first
 * move has been drawn the pane still covers the whole glass, so the list and the
 * bar arriving behind it are invisible.
 *
 * The listeners are passive and captured: passive because a gesture that only
 * WATCHES must never be able to delay the scroll it is watching, captured so a
 * component inside the transcript cannot hide the stroke by stopping it on its
 * way up.
 *
 * A DIALOG takes the same stroke with `isLayer`. It stands over the application
 * rather than in the flow under it, so it is dragged where it is, and its own
 * being up stops being a reason to refuse: what the stroke leaves is the dialog.
 */
export function useEdgeBack(
  onBack: (() => void) | null,
  { isLayer = false }: EdgeBackOptions = {},
): EdgeBackPanes {
  // Read only from an event, so it is kept in a ref instead of resubscribing the
  // listeners every time the shell re-renders.
  const latest = useRef(onBack);
  useEffect(() => {
    latest.current = onBack;
  });

  const hasDoor = onBack !== null;
  const [pane, setPane] = useState<HTMLElement | null>(null);
  const under = useRef<HTMLElement | null>(null);
  const setUnder = useCallback((element: HTMLElement | null) => {
    under.current = element;
  }, []);
  const [swiping, setSwiping] = useState(false);

  useEffect(() => {
    if (!pane || !hasDoor) return;
    const view = pane.ownerDocument.defaultView;
    let gesture: EdgeGesture | null = null;
    let stage: EdgeStage | null = null;
    let velocity = 0;
    let lastX = 0;
    let lastAt = 0;
    let settling: number | null = null;

    const pointOf = (event: TouchEvent): EdgePoint | null => {
      const touch = event.touches[0];
      return touch ? { x: touch.clientX, y: touch.clientY } : null;
    };

    // The finger's own speed, smoothed, because the last pair of points alone is
    // as likely to describe a stumble as a throw.
    const track = (at: EdgePoint) => {
      const now = performance.now();
      const since = now - lastAt;
      if (since <= 0) return;
      const instant = (at.x - lastX) / since;
      velocity = Math.max(
        -EDGE_TOP_PX_MS,
        Math.min(instant * 0.7 + velocity * 0.3, EDGE_TOP_PX_MS),
      );
      lastX = at.x;
      lastAt = now;
    };

    /**
     * End the stroke: carry the panes the rest of the way, and only once they are
     * there take the step. Leaving is announced AFTER the slide so the transcript
     * is gone from the glass before it is gone from the shell; the inline styles
     * go one frame later still, by which time the list is back in the layout and
     * nothing would flash.
     */
    const finish = (completes: boolean, across: number) => {
      gesture = null;
      const closing = stage;
      stage = null;
      if (!closing) {
        if (completes) latest.current?.();
        return;
      }
      const travelled = Math.max(0, Math.min(across, closing.width));
      const ms = edgeSettleMs(completes ? closing.width - travelled : travelled, velocity);
      edgeSettle(closing, completes, ms);
      const land = () => {
        settling = null;
        if (completes) latest.current?.();
        setSwiping(false);
        (view?.requestAnimationFrame ?? ((run: FrameRequestCallback) => run(0)))(() =>
          edgeClose(closing),
        );
      };
      if (ms <= 0) land();
      else settling = view?.setTimeout(land, ms) ?? null;
    };

    const onStart = (event: Event) => {
      if (settling !== null) return;
      const touchEvent = event as TouchEvent;
      const at = pointOf(touchEvent);
      const started = at
        ? edgeStart(pane.getBoundingClientRect(), touchEvent.touches.length, at)
        : null;
      // The strip is asked about FIRST, so the walk up the tree only happens for
      // the few touches that landed where the gesture could begin at all.
      gesture = started && edgeIsFree(touchEvent.target, pane, isLayer) ? started : null;
      velocity = 0;
      lastAt = performance.now();
      lastX = at?.x ?? 0;
    };

    const onMove = (event: Event) => {
      if (!gesture) return;
      const touchEvent = event as TouchEvent;
      const at = pointOf(touchEvent);
      const next = at ? edgeMove(gesture, touchEvent.touches.length, at) : null;
      if (at) track(at);
      if (!next) {
        // The stroke stopped being a way back — the finger turned into a scroll,
        // or carried the page home. Whatever it had already dragged goes back.
        if (stage) finish(false, gesture.across);
        gesture = null;
        return;
      }
      gesture = next;
      if (!stage && next.across >= EDGE_OPEN_PX && wantsMotion(view)) {
        stage = edgeOpen(pane, under.current, isLayer);
        setSwiping(true);
      }
      if (stage) edgePaint(stage, next.across);
    };

    // The lift is the commitment.
    const onEnd = () => {
      if (!gesture) return;
      const completes = edgeCompletes(gesture.across, velocity);
      finish(completes, gesture.across);
    };

    // A cancel is not a lift. WebKit sends one when it hands the drag to a native
    // scroller, and a stroke the browser took away was never released here.
    const onCancel = () => {
      if (gesture || stage) finish(false, gesture?.across ?? 0);
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
      if (settling !== null) view?.clearTimeout(settling);
      if (stage) edgeClose(stage);
    };
  }, [hasDoor, isLayer, pane]);

  return { pane: setPane, under: setUnder, isSwiping: swiping };
}

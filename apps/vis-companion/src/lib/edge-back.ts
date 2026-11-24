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
 * already standing behind it — a third of the width back and darkened, the way
 * UIKit parks the page underneath — and the lift finishes the pop in whichever
 * direction the finger was going, at the speed it was going.
 *
 * The numbers are UIKit's, read off the implementation Apple's own shape is
 * measured against (`ios.transition.ts` and `swipe-back.ts` in Ionic): a 0.33
 * parallax, a 10% black veil over the page underneath, `cubic-bezier(0.32, 0.72,
 * 0, 1)` and at most 540ms for the rest of the way. A page let go before the middle
 * goes home unless it was thrown, and it does not scroll while it is dragged. A
 * reader who asked for less motion keeps the plain step, with no slide at all.
 *
 * THE ARROW AND THE PHONE'S OWN BACK TAKE THE SAME SLIDE, and a session that opens
 * from the list comes in the way it goes out. A page that slides under the finger and
 * jumps under the arrow reads as two different apps. Android reads its back gesture
 * itself and reports each step (`lib/back-gesture`), so that stroke drags this page too.
 */

import { useCallback, useEffect, useLayoutEffect, useRef, useState } from 'react';
import { useSessionActive } from './session-active';
import { isAppForeground, isKeyboardInputElement } from './viewport';

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
 * How far across the finger must carry before the lift leaves the session, when the
 * stroke drags no page. It is one unhurried thumb sweep on the narrowest phone this
 * app runs on, and long enough that a tap which slid a little is never a navigation.
 * A dragged page shows its own middle, and that decides instead ({@link edgeCompletes}).
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

/** The control points of the curve a page rides on iOS: x1, y1, x2, y2. */
const EDGE_CURVE = [0.32, 0.72, 0, 1] as const;

/** The curve a page rides on iOS, in and out of the stack. */
export const EDGE_EASING = `cubic-bezier(${EDGE_CURVE.join(', ')})`;

/** The longest the rest of the way back ever takes, in ms. */
export const EDGE_SETTLE_MS = 540;

/**
 * The longest a finished stroke waits for its step, in ms. The shell can take the
 * step later than the call: to leave a session that the list opened, it pops
 * history, and the pop arrives as a later event. If the step never comes, the
 * transcript comes back.
 */
export const EDGE_STEP_WAIT_MS = 1000;

/** How far behind the page being left the one underneath waits: a third of the width. */
export const EDGE_PARALLAX = 0.33;

/**
 * How dark the veil over that page is while it waits, and 0 once it is back in front.
 *
 * It is a black veil, never the page's own opacity: a page made translucent shows
 * the shell through it, and on a light theme that WASHES the list out instead of
 * putting it in the shade the way UIKit's dimming view does.
 */
export const EDGE_DIM = 0.1;

/**
 * How much faster than its average speed the curve starts: the slope of
 * {@link EDGE_EASING} at 0, `0.72 / 0.32`. The rest of the stroke is timed so that
 * the page leaves the finger at the finger's own speed instead of lurching ahead.
 */
const EDGE_LAUNCH = EDGE_CURVE[1] / EDGE_CURVE[0];

/** Nearer to its end than this, in px, the page has arrived: the eye sees no more motion. */
const EDGE_ARRIVED_PX = 0.5;

/** A finger held still this long before the lift was not thrown, in ms. */
const EDGE_STILL_MS = 80;

/** A throw, in px/ms: past this speed the lift completes however short the stroke was. */
export const EDGE_FLICK_PX_MS = 0.2;

/**
 * No finger travels faster than this, in px/ms. A touch stream whose timestamps
 * land in the same millisecond would otherwise read as an infinite throw and
 * finish the stroke inside a single frame.
 */
const EDGE_TOP_PX_MS = 4;

/**
 * The shadow the leading edge of the page being left casts on the list behind it: a
 * thin line of shade at the edge, as UIKit draws it, not a glow across the list.
 */
const EDGE_SHADOW = '-2px 0 8px rgba(0, 0, 0, 0.16)';

/**
 * Sideways past this, and further than it has gone up or down, the stroke holds the
 * page still. It is the distance at which the phone itself decides that a drag began.
 */
const EDGE_HOLD_PX = 10;

/** How far across the stroke has to come before the panes are set up to be dragged. */
const EDGE_OPEN_PX = 4;

/**
 * Would a lift right here leave the session? iOS asks two questions of a back
 * swipe: has the page come far enough to stay gone, or was it THROWN — a flick
 * completes from anywhere, and a finger travelling back toward the edge never
 * completes, however far out it had already carried the page.
 *
 * Far enough is the middle of the page that the finger drags, `width` px wide: UIKit
 * sends a page home when it is let go before its middle. A stroke that drags no page
 * (`width` 0, for a reader who asked for less motion) has no middle to show, so
 * {@link EDGE_BACK_PX} decides.
 */
export function edgeCompletes(across: number, velocity: number, width = 0): boolean {
  if (velocity < 0) return false;
  if (across >= (width > 0 ? width / 2 : EDGE_BACK_PX)) return true;
  return velocity > EDGE_FLICK_PX_MS && across > HOMEWARD_PX;
}

/**
 * How long the rest of the stroke takes. The curve starts at {@link EDGE_LAUNCH}
 * times its average speed, so a duration of that many times `remaining / speed`
 * makes the page carry on at the speed the finger was already travelling. It is
 * never longer than a full-length pop, and a page that is as good as home is
 * simply put there.
 */
export function edgeSettleMs(remainingPx: number, velocity: number): number {
  if (remainingPx <= 5) return 0;
  const speed = Math.abs(velocity);
  if (speed < 0.01) return EDGE_SETTLE_MS;
  return Math.min(Math.round((EDGE_LAUNCH * remainingPx) / speed), EDGE_SETTLE_MS);
}

/** One coordinate of a cubic Bézier curve from 0 to 1, with control values `a` and `b`. */
const bezier = (a: number, b: number, s: number): number =>
  3 * (1 - s) ** 2 * s * a + 3 * (1 - s) * s ** 2 * b + s ** 3;

/**
 * When the page ARRIVES, in ms: the time at which a settle of `ms` over `remainingPx`
 * on {@link EDGE_EASING} is within {@link EDGE_ARRIVED_PX} of its end. The curve has a
 * long tail, and the last fifth or more of it moves the page less than a pixel. The
 * step is taken on arrival, so the list is live again while the tail runs out unseen.
 */
export function edgeArrivalMs(remainingPx: number, ms: number): number {
  if (ms <= 0 || remainingPx <= EDGE_ARRIVED_PX) return 0;
  const done = 1 - EDGE_ARRIVED_PX / remainingPx;
  let low = 0;
  let high = 1;
  for (let i = 0; i < 20; i += 1) {
    const mid = (low + high) / 2;
    if (bezier(EDGE_CURVE[1], EDGE_CURVE[3], mid) < done) low = mid;
    else high = mid;
  }
  return Math.round(ms * bezier(EDGE_CURVE[0], EDGE_CURVE[2], high));
}

/**
 * The two panes for the length of one stroke: the transcript being dragged out
 * and the list waiting under it, each with the inline styles it had before, so
 * the end of the stroke can hand them back untouched.
 */
interface EdgeStage {
  readonly pane: HTMLElement;
  readonly under: HTMLElement | null;
  /** The black veil that puts `under` in the shade, between it and the pane. */
  readonly veil: HTMLElement | null;
  /** How far the transcript has to travel to be gone. */
  readonly width: number;
  readonly paneStyle: string;
  readonly underStyle: string;
  /** The scrollers that the stroke holds still, each with the inline `overflow-y` it had. */
  readonly held: Array<readonly [HTMLElement, string]>;
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
  const veil = under && frame && !isLayer && pane.parentElement ? edgeVeil(pane) : null;
  const stage: EdgeStage = {
    pane,
    under,
    veil,
    width,
    paneStyle: pane.style.cssText,
    underStyle: under?.style.cssText ?? '',
    held: [],
  };

  if (frame && !isLayer) {
    pane.style.position = 'absolute';
    pane.style.left = `${box.left - frame.left}px`;
    pane.style.top = `${box.top - frame.top}px`;
    pane.style.width = `${width}px`;
    // The bottom is pinned to the shell, not a height: the stroke drops the keyboard,
    // the shell grows back, and the transcript grows with it instead of leaving a gap.
    pane.style.bottom = `${frame.height ? frame.bottom - box.bottom : 0}px`;
    if (!frame.height) pane.style.height = `${height}px`;
    pane.style.margin = '0';
    pane.style.zIndex = '30';
  }
  pane.style.transition = '';
  pane.style.willChange = 'transform';
  if (under) {
    under.style.transition = '';
    under.style.willChange = 'transform';
  }
  return stage;
}

/**
 * The veil stands just before the pane, in the same parent: later in the tree than
 * the list and the app bar that come back, so above them at the same z-index, and
 * below the transcript that leaves. It is pinned to the shell as the pane is, and it
 * never takes a touch.
 */
function edgeVeil(pane: HTMLElement): HTMLElement {
  const veil = pane.ownerDocument.createElement('div');
  veil.setAttribute('aria-hidden', 'true');
  veil.dataset.edgeVeil = '';
  veil.style.cssText =
    'position:absolute;inset:0;z-index:30;pointer-events:none;background:#000;will-change:opacity;';
  veil.style.opacity = `${EDGE_DIM}`;
  pane.before(veil);
  return veil;
}

/**
 * What a finger that landed on `from` can scroll up and down: each ancestor of the
 * touch, up to the pane, that scrolls and has content taller than its box.
 */
export function edgeScrollers(from: EventTarget | null, root: Element): HTMLElement[] {
  const found: HTMLElement[] = [];
  let node = from instanceof Element ? from : null;
  while (node) {
    if (node instanceof HTMLElement && node.scrollHeight > node.clientHeight + 1) {
      const overflow = getComputedStyle(node).overflowY;
      if (overflow === 'auto' || overflow === 'scroll') found.push(node);
    }
    if (node === root) break;
    node = node.parentElement;
  }
  return found;
}

/**
 * Hold still what the finger could scroll up and down. The listeners only watch, so
 * they cannot refuse a scroll. A scroller with `overflow-y: hidden` keeps its place
 * and does not follow the finger. {@link edgeClose} gives each one its own value back.
 */
function edgeHoldScroll(stage: EdgeStage, from: EventTarget | null, root: Element): void {
  for (const scroller of edgeScrollers(from, root)) {
    stage.held.push([scroller, scroller.style.overflowY]);
    scroller.style.overflowY = 'hidden';
  }
}

/** Stand both panes where a stroke this far across puts them. */
function edgePaint(stage: EdgeStage, across: number): void {
  const travelled = Math.max(0, Math.min(across, stage.width));
  const step = stage.width > 0 ? travelled / stage.width : 0;
  stage.pane.style.transform = `translateX(${travelled}px)`;
  // The shadow only once the page has moved: a stage set up under a finger that has not
  // moved yet must not show anything at all.
  if (travelled > 0 && !stage.pane.style.boxShadow) stage.pane.style.boxShadow = EDGE_SHADOW;
  if (stage.veil) stage.veil.style.opacity = `${EDGE_DIM * (1 - step)}`;
  if (!stage.under) return;
  stage.under.style.transform = `translateX(${-(1 - step) * stage.width * EDGE_PARALLAX}px)`;
}

/** Carry both panes the rest of the way, out of the stack or home again. */
function edgeSettle(stage: EdgeStage, completes: boolean, ms: number): void {
  const ride = ms > 0 ? `transform ${ms}ms ${EDGE_EASING}, opacity ${ms}ms ${EDGE_EASING}` : '';
  stage.pane.style.transition = ride;
  if (stage.under) stage.under.style.transition = ride;
  if (stage.veil) stage.veil.style.transition = ride;
  edgePaint(stage, completes ? stage.width : 0);
}

/** Stop a slide where it is on the glass, and say how far across the pane stands. */
function edgeHalt(stage: EdgeStage): number {
  // A browser gives the place of a moving pane as a matrix: `matrix(1, 0, 0, 1, x, 0)`.
  const matrix = /^matrix\(([^)]+)\)$/.exec(getComputedStyle(stage.pane).transform);
  const across = Number(matrix?.[1].split(',')[4]) || 0;
  stage.pane.style.transition = '';
  if (stage.under) stage.under.style.transition = '';
  if (stage.veil) stage.veil.style.transition = '';
  edgePaint(stage, across);
  return Math.max(0, Math.min(across, stage.width));
}

/** Give both panes back exactly the inline styles they arrived with. */
function edgeClose(stage: EdgeStage): void {
  for (const [scroller, overflowY] of stage.held) scroller.style.overflowY = overflowY;
  stage.held.length = 0;
  stage.pane.style.cssText = stage.paneStyle;
  if (stage.under) stage.under.style.cssText = stage.underStyle;
  stage.veil?.remove();
}

/**
 * Take the keyboard down when the field that holds it is in the pane being left.
 * Only while the app is on screen: a blur from the background is what the keyboard
 * watchdog kills for (`isAppForeground`).
 */
function edgeDropKeyboard(pane: HTMLElement): void {
  const active = pane.ownerDocument.activeElement;
  if (isAppForeground() && isKeyboardInputElement(active) && pane.contains(active)) active.blur();
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
  /**
   * A stroke is in flight or waits for its step: the list and the app bar it returns to
   * must be on the glass.
   */
  readonly isSwiping: boolean;
  /**
   * Take the way back with no finger on the pane: the header's arrow and the phone's
   * own back. The page slides out, then the step is taken. A stroke in flight is
   * carried the rest of the way from where it stands.
   */
  readonly leave: () => void;
  /**
   * A stroke that the phone reads itself, such as Android's back gesture. `step` is how
   * much of the way the page has come, from 0 to 1. `null` takes the stroke back, and
   * {@link leave} completes it.
   */
  readonly drag: (step: number | null) => void;
}

export interface EdgeBackOptions {
  /**
   * The pane is a layer standing OVER the application — a dialog, an opened
   * artifact — rather than a page in the flow under it. What such a pane
   * uncovers is already on screen behind it, so `under` goes unused.
   */
  readonly isLayer?: boolean;
  /**
   * Which of the two the phone shows now: the page in this hook's pane, or the one
   * under it. When `under` gives way to `pane`, the page slides in over it. Each other
   * change is a plain step: a cold start into a session has nothing to slide over.
   */
  readonly page?: 'pane' | 'under' | null;
  /**
   * What the pane holds, such as the open session. A step back can stay in the pane: a
   * subagent session goes back to its parent. A new key is that step, and it gives the
   * pane back at once rather than after {@link EDGE_STEP_WAIT_MS}.
   */
  readonly holds?: string | null;
}

/**
 * THE OTHER WAY BACK, and the only one that never reaches this file as touches.
 * Android answers "back" with its own button or its own system gesture, and what
 * the page is handed is ONE event for the whole shell (`App.tsx`) — so the layer a
 * finger could have dragged out has to be FOUND rather than touched. Layers stand
 * here while they are up, in the order they opened, and the shell asks for the top
 * one before it walks its own history: a reader inside an opened run means to leave
 * the run, not the session it belongs to.
 */
const layers: Array<() => void> = [];

/**
 * Stand a layer over the application for as long as the caller is mounted, so the
 * phone's back takes it down first. `null` is a layer that is not up — the same
 * "nothing to leave" a null door says to `useEdgeBack`.
 */
export function useBackLayer(onDismiss: (() => void) | null): void {
  // Read only when back arrives, so a re-render never re-announces the layer.
  const latest = useRef(onDismiss);
  useEffect(() => {
    latest.current = onDismiss;
  });
  // A screen in the background holds no layer: back belongs to the screen in front.
  const isActive = useSessionActive();
  const isUp = onDismiss !== null && isActive;
  useEffect(() => {
    if (!isUp) return;
    const dismiss = () => latest.current?.();
    layers.push(dismiss);
    return () => {
      const at = layers.lastIndexOf(dismiss);
      if (at >= 0) layers.splice(at, 1);
    };
  }, [isUp]);
}

/**
 * Take down the layer standing on top, and say whether there was one. False leaves
 * back unspent, and the shell underneath is what answers it.
 */
export function dismissTopLayer(): boolean {
  const top = layers.at(-1);
  if (!top) return false;
  top();
  return true;
}

/**
 * Whether a layer stands over the application right now: one standing here, or a
 * modal surface that takes the whole screen (`aria-modal`) without being one. A key
 * the screen underneath would answer, such as Escape stopping the running turn,
 * belongs to that layer while it is up.
 */
export function isLayerUp(doc: Document = document): boolean {
  return layers.length > 0 || doc.querySelector('[aria-modal="true"]') !== null;
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
 * bar arriving behind it are invisible. After a lift that leaves, it stays on until
 * the step has been taken, so the shell does not show the session again on the way
 * out.
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
  { isLayer = false, page = null, holds = null }: EdgeBackOptions = {},
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
  // A stroke that the finger let go of, whose panes get their styles back on the next
  // frame. It outlives one run of the effect below, so it is kept here.
  const unwinding = useRef<{ stage: EdgeStage; frame: number | null } | null>(null);
  // The same layer answers the phone's own back button, which arrives as one event
  // for the shell rather than as a finger on this pane.
  useBackLayer(isLayer ? onBack : null);
  // The ways back that no finger on the pane draws, while the pane is watched.
  const hands = useRef<{ leave: () => void; drag: (step: number | null) => void } | null>(null);

  // THE PAGE COMES IN THE WAY IT GOES OUT. The render that shows it also keeps the list
  // and the app bar on the glass, so the shell never drops them and builds them again.
  const [shown, setShown] = useState(page);
  const [isEntering, setEntering] = useState(false);
  if (shown !== page) {
    setShown(page);
    setEntering(
      !isLayer &&
        shown === 'under' &&
        page === 'pane' &&
        wantsMotion(typeof window === 'undefined' ? null : window),
    );
  }
  // A page on its way in takes no stroke: it is not there yet.
  const entering = useRef(false);
  useLayoutEffect(() => {
    // The pane arrives one render after the page: wait for it.
    if (!isEntering || !pane) return;
    const view = pane.ownerDocument.defaultView;
    const stage = edgeOpen(pane, under.current, false);
    entering.current = true;
    // Off the glass first, with no transition: the frame after starts the slide.
    edgePaint(stage, stage.width);
    let timer: number | null = null;
    let frame: number | null = null;
    const arrive = () => setEntering(false);
    const slide = () => {
      frame = null;
      // The veil is new and the pane has just moved: the slide starts from where the
      // browser has seen them, so it must see them first.
      void pane.offsetWidth;
      const ms = edgeSettleMs(stage.width, 0);
      edgeSettle(stage, false, ms);
      timer = view?.setTimeout(arrive, edgeArrivalMs(stage.width, ms)) ?? null;
    };
    if (view?.requestAnimationFrame) frame = view.requestAnimationFrame(slide);
    else slide();
    return () => {
      if (frame !== null) view?.cancelAnimationFrame(frame);
      if (timer !== null) view?.clearTimeout(timer);
      edgeClose(stage);
      entering.current = false;
    };
  }, [isEntering, pane]);

  useEffect(() => {
    if (!pane || !hasDoor) return;
    const view = pane.ownerDocument.defaultView;
    let gesture: EdgeGesture | null = null;
    let stage: EdgeStage | null = null;
    let velocity = 0;
    let lastX = 0;
    let lastAt = 0;
    // The settle, and then the wait for the step.
    let timer: number | null = null;
    // A stroke that the finger let go of, until its panes get their styles back.
    let landing: EdgeStage | null = null;
    // This stroke has already taken the keyboard down.
    let isKeyboardDropped = false;
    // What the finger landed on, and whether this stroke already holds its scrollers.
    let from: EventTarget | null = null;
    let isScrollHeld = false;
    // The lift, the arrow or the phone's back has sent the page out, and the step waits.
    let isLeaving = false;
    // How far the phone's own stroke has carried the page, while it holds the stage.
    let driven: number | null = null;
    // Where a touch had put the page when the phone took its stroke.
    let floor = 0;
    // The frame in which a way back with no finger starts its slide.
    let frame: number | null = null;

    const pointOf = (event: TouchEvent): EdgePoint | null => {
      const touch = event.touches[0];
      return touch ? { x: touch.clientX, y: touch.clientY } : null;
    };

    // The finger's own speed, smoothed, because the last pair of points alone is
    // as likely to describe a stumble as a throw.
    const track = (at: EdgePoint, now: number) => {
      const since = now - lastAt;
      // Moves that arrive together, under a millisecond apart, describe no speed.
      if (since < 1) return;
      const instant = (at.x - lastX) / since;
      velocity = Math.max(
        -EDGE_TOP_PX_MS,
        Math.min(instant * 0.7 + velocity * 0.3, EDGE_TOP_PX_MS),
      );
      lastX = at.x;
      lastAt = now;
    };

    /** Give a stroke that let go its panes' styles now, if its frame has not come yet. */
    const unwind = () => {
      const closing = unwinding.current;
      unwinding.current = null;
      if (!closing) return;
      if (closing.frame !== null) view?.cancelAnimationFrame(closing.frame);
      edgeClose(closing.stage);
    };

    /**
     * Give both panes back the inline styles they arrived with, and the shell its own
     * layout. The styles go one frame after `isSwiping`, by which time the shell has
     * drawn the screen that the stroke ends on, and nothing would flash.
     */
    const release = () => {
      const closing = landing;
      landing = null;
      isLeaving = false;
      if (timer !== null) view?.clearTimeout(timer);
      timer = null;
      setSwiping(false);
      if (!closing) return;
      unwind();
      const pending = { stage: closing, frame: null as number | null };
      unwinding.current = pending;
      if (view?.requestAnimationFrame) pending.frame = view.requestAnimationFrame(unwind);
      else unwind();
    };

    /**
     * Set a new stage. A stroke that went home in the last frame still holds the panes:
     * they get their own styles first, or the new stage keeps the list's shifted
     * transform as its original style and gives it back at its own end.
     */
    const open = (): EdgeStage => {
      unwind();
      const opened = edgeOpen(pane, under.current, isLayer);
      setSwiping(true);
      return opened;
    };

    /**
     * End the stroke: carry the panes the rest of the way, and only once they are
     * there take the step. Leaving is announced AFTER the slide so the transcript
     * is gone from the glass before it is gone from the shell. There is where the
     * page arrives ({@link edgeArrivalMs}), not where the tail of the curve ends.
     *
     * The step can come later than the call. A session that the list opened is a
     * history entry, so the shell leaves it by a pop, and the pop arrives as a later
     * event. Until then the session is still open: the panes stay where the slide
     * left them, and the step releases them when it closes this hook's door.
     */
    const finish = (completes: boolean, across: number) => {
      gesture = null;
      driven = null;
      const closing = stage;
      stage = null;
      if (!closing) {
        if (completes) latest.current?.();
        return;
      }
      landing = closing;
      isLeaving = completes;
      const travelled = Math.max(0, Math.min(across, closing.width));
      const remaining = completes ? closing.width - travelled : travelled;
      const ms = edgeSettleMs(remaining, velocity);
      edgeSettle(closing, completes, ms);
      const land = () => {
        timer = null;
        if (!completes) {
          release();
          return;
        }
        latest.current?.();
        if (landing) timer = view?.setTimeout(release, EDGE_STEP_WAIT_MS) ?? null;
      };
      if (ms <= 0) land();
      else timer = view?.setTimeout(land, edgeArrivalMs(remaining, ms)) ?? null;
    };

    /**
     * Set the stage for a way back that no finger on this pane draws. Null for a pane
     * that takes the plain step: a layer, or a reader who asked for less motion.
     */
    const stand = (): EdgeStage | null => {
      if (isLayer || !wantsMotion(view)) return null;
      edgeDropKeyboard(pane);
      velocity = 0;
      const opened = open();
      edgePaint(opened, 0);
      return opened;
    };

    const leave = () => {
      if (isLeaving) return;
      // A page that is still on its way, in or home, takes the plain step.
      if (entering.current || landing) {
        latest.current?.();
        return;
      }
      if (stage) {
        finish(true, driven ?? gesture?.across ?? 0);
        return;
      }
      const opened = stand();
      if (!opened) {
        latest.current?.();
        return;
      }
      stage = opened;
      // The slide starts one frame later: by then the shell has put the list and the
      // app bar on the glass, and the browser has seen where the panes start.
      const go = () => {
        frame = null;
        if (stage !== opened) return;
        void pane.offsetWidth;
        finish(true, 0);
      };
      if (view?.requestAnimationFrame) frame = view.requestAnimationFrame(go);
      else go();
    };

    const drag = (step: number | null) => {
      if (step === null) {
        if (driven !== null && stage) finish(false, driven);
        return;
      }
      if (driven === null) {
        if (isLeaving || entering.current) return;
        // THE PHONE TOOK THE TOUCH THAT BEGAN THIS STROKE. The page saw the start of the
        // touch and moved with it. The cancel of that touch comes before this first step
        // or after it. The page stays where the touch put it and goes on from there.
        // Sent home first, it jumped back at the start of each stroke.
        if (stage && gesture) {
          floor = Math.min(gesture.across, stage.width);
          edgeDropKeyboard(pane);
        } else if (stage) {
          // The arrow set this stage, and its slide starts in the next frame.
          return;
        } else if (landing) {
          if (timer !== null) view?.clearTimeout(timer);
          timer = null;
          stage = landing;
          landing = null;
          floor = edgeHalt(stage);
        } else {
          const opened = stand();
          if (!opened) return;
          stage = opened;
          floor = 0;
        }
        gesture = null;
        velocity = 0;
      }
      if (!stage) return;
      driven = floor + Math.max(0, Math.min(step, 1)) * (stage.width - floor);
      edgePaint(stage, driven);
    };
    hands.current = { leave, drag };

    const onStart = (event: Event) => {
      if (landing || entering.current) return;
      // A STROKE STILL ON THE STAGE NEVER ENDED HERE. A second finger refused its
      // gesture, or its lift went to a node that left the tree. Send it home first.
      // A new stage over it would keep its shifted list as the list's own style.
      if (stage) {
        finish(false, driven ?? gesture?.across ?? 0);
        return;
      }
      const touchEvent = event as TouchEvent;
      const at = pointOf(touchEvent);
      const started = at
        ? edgeStart(pane.getBoundingClientRect(), touchEvent.touches.length, at)
        : null;
      // The strip is asked about FIRST, so the walk up the tree only happens for
      // the few touches that landed where the gesture could begin at all.
      gesture = started && edgeIsFree(touchEvent.target, pane, isLayer) ? started : null;
      velocity = 0;
      isKeyboardDropped = false;
      from = touchEvent.target;
      isScrollHeld = false;
      lastAt = event.timeStamp;
      lastX = at?.x ?? 0;
      // THE STAGE IS SET UNDER THE FINGER, BEFORE IT MOVES. Bringing the app bar and the
      // list back is a React commit; made at the first move, it took that move's frame
      // and the page stuck, then jumped. Behind a page that has not moved, it is unseen.
      if (gesture && !isLayer && wantsMotion(view)) {
        stage = open();
        edgePaint(stage, 0);
      }
    };

    const onMove = (event: Event) => {
      if (!gesture) return;
      const touchEvent = event as TouchEvent;
      const at = pointOf(touchEvent);
      const next = at ? edgeMove(gesture, touchEvent.touches.length, at) : null;
      if (at) track(at, event.timeStamp);
      if (!next) {
        // The stroke stopped being a way back — the finger turned into a scroll,
        // or carried the page home. Whatever it had already dragged goes back.
        if (stage) finish(false, gesture.across);
        gesture = null;
        return;
      }
      gesture = next;
      // THE KEYBOARD GOES WITH THE FIRST SIDEWAYS MOVE. Left up, it stood over the
      // transcript for the whole slide and fell only once the session had closed.
      // A tap or a scroll that began in the strip keeps it.
      const isSideways = next.across >= VERTICAL_PX && next.across > Math.abs((at?.y ?? 0) - next.from.y);
      if (!isKeyboardDropped && at && isSideways) {
        isKeyboardDropped = true;
        edgeDropKeyboard(pane);
      }
      if (!stage && next.across >= EDGE_OPEN_PX && wantsMotion(view)) {
        stage = open();
      }
      // THE PAGE DOES NOT SCROLL WHILE IT IS DRAGGED OUT. The browser scrolled the
      // transcript by the up and down part of a slanted stroke, under the finger that
      // carried it away. UIKit holds a page still for the whole pop.
      const vertical = Math.abs((at?.y ?? next.from.y) - next.from.y);
      if (stage && !isScrollHeld && next.across >= EDGE_HOLD_PX && next.across > vertical) {
        isScrollHeld = true;
        edgeHoldScroll(stage, from, pane);
      }
      if (stage) edgePaint(stage, next.across);
    };

    // The lift is the commitment.
    const onEnd = (event: Event) => {
      if (!gesture) return;
      // A finger that stopped before it let go throws nothing, whatever it did before.
      if (event.timeStamp - lastAt > EDGE_STILL_MS) velocity = 0;
      const completes = edgeCompletes(gesture.across, velocity, stage?.width ?? 0);
      finish(completes, gesture.across);
    };

    // A cancel is not a lift. WebKit sends one when it hands the drag to a native
    // scroller, and a stroke the browser took away was never released here.
    const onCancel = () => {
      // The phone's own stroke holds the stage: its cancel is not this touch's.
      if (driven !== null) return;
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
      hands.current = null;
      if (frame !== null) view?.cancelAnimationFrame(frame);
      if (stage) {
        edgeClose(stage);
        setSwiping(false);
      }
      // The step was taken, or the pane went: give back what the stroke still holds.
      if (landing) release();
    };
    // A new `holds` is a step that stayed in the pane: the cleanup gives the stroke back.
  }, [hasDoor, isLayer, pane, holds]);

  const leave = useCallback(() => {
    if (hands.current) hands.current.leave();
    else latest.current?.();
  }, []);
  const drag = useCallback((step: number | null) => hands.current?.drag(step), []);

  return { pane: setPane, under: setUnder, isSwiping: swiping || isEntering, leave, drag };
}

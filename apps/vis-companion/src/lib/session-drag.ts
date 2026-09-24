/**
 * CARRYING A SESSION WITH A FINGER, so a phone can file one into a group.
 *
 * The rows and the places that take them speak HTML5 drag-and-drop, which a touch
 * screen never sends: `dragstart`, `dragover` and `drop` arrive from a mouse, and
 * neither WKWebView nor Android WebView raises one for a finger. Without this module
 * the shelves a project draws cannot be dragged into at all on the device the app is
 * mostly read on, and a session can only change groups through the row's own menu.
 *
 * A finger says "pick this up" by RESTING: a press that stays within `LIFT_SLOP_PX`
 * for `LIFT_DELAY_MS` lifts the row, and anything quicker belongs to the list's
 * scroller or to the row's swipe, both of which are ordinary reading. Once a row is
 * up the gesture is ours — every move is prevented, so neither scroller can take the
 * finger back — the row travels as a floating copy of itself, the place under it
 * lights, and letting go files it there.
 *
 * The carry lives in ONE module-scope store rather than in React state: the row that
 * is moving and the band that lights are in different trees, and a re-render per
 * `touchmove` stutters on exactly the phones this gesture exists for.
 */

import { useEffect, useId, useRef, useState, type RefObject } from 'react';

/** A finger, in client coordinates. */
export interface LiftPoint {
  readonly x: number;
  readonly y: number;
}

/**
 * What a place that takes a session marks itself with. The hit test reads this
 * attribute and nothing else; `DropTargetProps` spells it a second time because only
 * a literal key types as a `data-` attribute in JSX.
 */
export const DROP_TARGET_ATTRIBUTE = 'data-session-drop';

/** HTML drag payload for the sessions selected with Shift; touch carries one id separately. */
export const SESSION_DRAG_MIME = 'application/vnd.vis.sessions+json';

/** The mark a place wears while it is offering to take a carried session. */
export interface DropTargetProps {
  readonly 'data-session-drop'?: string;
}

/** How long a still finger rests on a row before the row comes up. */
export const LIFT_DELAY_MS = 400;

/**
 * Further than this before the row is up, and the finger was scrolling the list or
 * swiping the row open. Neither may be interrupted by a pick-up.
 */
export const LIFT_SLOP_PX = 8;

/** Inside this much of the scroller's edge, a carried row drags the list along. */
export const EDGE_PULL_PX = 72;

/** How far that pull travels per frame, right at the edge. */
export const EDGE_PULL_SPEED = 12;

/** Has the finger left the place it landed? */
export function movedBeyond(from: LiftPoint, to: LiftPoint, slop = LIFT_SLOP_PX): boolean {
  return Math.abs(to.x - from.x) > slop || Math.abs(to.y - from.y) > slop;
}

/**
 * WHICH PLACE IS UNDER THE FINGER. `document.elementFromPoint` cannot answer it: the
 * row travels under the finger as a floating copy, and a band is painted over by the
 * rows standing inside it. The places are measured instead, and the smallest box
 * holding the point wins, so a band nested in a set is answered before the set.
 */
export function dropTargetAt(at: LiftPoint, places: Iterable<Element>): string | null {
  let nearest: { key: string; area: number } | null = null;
  for (const place of places) {
    const key = place.getAttribute(DROP_TARGET_ATTRIBUTE);
    if (!key) continue;
    const box = place.getBoundingClientRect();
    if (at.x < box.left || at.x > box.right) continue;
    if (at.y < box.top || at.y > box.bottom) continue;
    const area = box.width * box.height;
    if (!nearest || area < nearest.area) nearest = { key, area };
  }
  return nearest?.key ?? null;
}

/**
 * How far the list travels this frame with the finger parked here: negative toward
 * the top, positive toward the bottom, and zero anywhere but the last `EDGE_PULL_PX`
 * of the view. A group further down the page is reachable no other way while one
 * hand is holding a row.
 */
export function edgePull(
  y: number,
  view: { readonly top: number; readonly bottom: number },
): number {
  const fromTop = y - view.top;
  if (fromTop < EDGE_PULL_PX) {
    return -Math.round(EDGE_PULL_SPEED * (1 - Math.max(0, fromTop) / EDGE_PULL_PX));
  }
  const fromBottom = view.bottom - y;
  if (fromBottom < EDGE_PULL_PX) {
    return Math.round(EDGE_PULL_SPEED * (1 - Math.max(0, fromBottom) / EDGE_PULL_PX));
  }
  return 0;
}

/** A session in the air: which one it is, and the place it is over right now. */
export interface SessionLift {
  readonly sessionId: string;
  readonly over: string | null;
}

let carried: SessionLift | null = null;
const watchers = new Set<() => void>();
const takers = new Map<string, (sid: string) => void>();

function announce(): void {
  for (const watcher of [...watchers]) watcher();
}

/** The session being carried right now, if a finger is holding one. */
export function liftedSession(): SessionLift | null {
  return carried;
}

/** Hear every change to the carry. Answers how to stop listening. */
export function watchLift(watcher: () => void): () => void {
  watchers.add(watcher);
  return () => {
    watchers.delete(watcher);
  };
}

/**
 * Offer to take a carried session under `key`, which is the value the place wears in
 * `DROP_TARGET_ATTRIBUTE`. Answers how to withdraw the offer.
 */
export function offerLift(key: string, take: (sid: string) => void): () => void {
  takers.set(key, take);
  return () => {
    takers.delete(key);
  };
}

/** The row is up. */
export function beginLift(sessionId: string): void {
  carried = { sessionId, over: null };
  announce();
}

/** The finger is over this place now — or, with `null`, over none of them. */
export function carryOver(over: string | null): void {
  if (!carried || carried.over === over) return;
  carried = { sessionId: carried.sessionId, over };
  announce();
}

/**
 * Let go. The place under the finger takes the session; answers whether one did, so
 * a row released over nothing is simply put back rather than reported as filed.
 */
export function releaseLift(): boolean {
  const landing = carried;
  carried = null;
  announce();
  if (!landing?.over) return false;
  const take = takers.get(landing.over);
  if (!take) return false;
  take(landing.sessionId);
  return true;
}

/** The browser took the gesture away. Nothing is filed. */
export function cancelLift(): void {
  if (!carried) return;
  carried = null;
  announce();
}

/** Every place on screen that is offering to take a session. */
function placesOnScreen(): Iterable<Element> {
  if (typeof document === 'undefined') return [];
  return document.querySelectorAll(`[${DROP_TARGET_ATTRIBUTE}]`);
}

/** The row as it travels: a copy held where the finger took it. */
interface Ghost {
  readonly element: HTMLElement;
  /** How far down the row the finger landed, so the copy is held there too. */
  readonly grab: number;
}

/**
 * Lift a copy of the row out of the list. The original stays where it is — a list
 * that closed the gap would reflow under the finger mid-gesture — and the copy is a
 * PICTURE, not a row: nothing looking for this session may find it twice.
 */
function carryGhost(row: HTMLElement, at: LiftPoint): Ghost {
  const box = row.getBoundingClientRect();
  const element = row.cloneNode(true) as HTMLElement;
  element.removeAttribute('data-session-row');
  element.querySelectorAll('[data-session-id]').forEach((mark) => {
    mark.removeAttribute('data-session-id');
  });
  element.setAttribute('aria-hidden', 'true');
  element.style.position = 'fixed';
  element.style.left = `${box.left}px`;
  element.style.top = `${box.top}px`;
  element.style.width = `${box.width}px`;
  element.style.margin = '0';
  element.style.pointerEvents = 'none';
  element.style.opacity = '0.92';
  element.style.borderRadius = '12px';
  element.style.boxShadow = '0 12px 32px rgb(0 0 0 / 45%)';
  // Above the anchored-menu scrim (`z-50`), which is the highest thing a list screen
  // can have standing over it while a row is being carried.
  element.style.zIndex = '60';
  document.body.append(element);
  return { element, grab: at.y - box.top };
}

/** Keep the copy under the finger. */
function moveGhost(ghost: Ghost, at: LiftPoint): void {
  ghost.element.style.top = `${at.y - ghost.grab}px`;
}

/**
 * While a row is out, the page must not select text or raise a callout under it:
 * a long press is exactly the gesture the browser reads as "select this word".
 */
function holdStill(on: boolean): void {
  if (typeof document === 'undefined') return;
  const style = document.body.style as CSSStyleDeclaration & { webkitUserSelect?: string };
  style.userSelect = on ? 'none' : '';
  style.webkitUserSelect = on ? 'none' : '';
}

/** The list the row stands in, or `null` when the page itself is the scroller. */
function scrollerOf(row: HTMLElement): HTMLElement | null {
  let node = row.parentElement;
  while (node) {
    const flow = getComputedStyle(node).overflowY;
    if ((flow === 'auto' || flow === 'scroll') && node.scrollHeight > node.clientHeight) {
      return node;
    }
    node = node.parentElement;
  }
  return null;
}

/** What the reader can see of that scroller. */
function viewOf(scroller: HTMLElement | null): { top: number; bottom: number } {
  if (!scroller) return { top: 0, bottom: window.innerHeight };
  const box = scroller.getBoundingClientRect();
  return { top: box.top, bottom: box.bottom };
}

function scrollAlong(scroller: HTMLElement | null, step: number): void {
  if (scroller) scroller.scrollTop += step;
  else window.scrollBy(0, step);
}

/**
 * A ROW A FINGER CAN PICK UP. Answers whether this row is the one in the air, which
 * is how the list paints the gap it left behind.
 *
 * The listeners are the row's own, and only `touchmove` is non-passive: preventing it
 * is the single thing that stops the swipe scroller under the finger and the list
 * behind it from reading a carry as a scroll. It is prevented only once the row is
 * actually up, so an ordinary swipe or flick is never made heavier.
 */
export function useSessionLift(
  sessionId: string,
  row: RefObject<HTMLElement | null>,
  canFile: boolean,
): boolean {
  const [isCarried, setIsCarried] = useState(false);
  // Read only from an event, so a re-render never resubscribes four listeners.
  const latest = useRef(sessionId);
  useEffect(() => {
    latest.current = sessionId;
  });

  useEffect(() => {
    const element = row.current;
    if (!element || !canFile) return;

    let origin: LiftPoint | null = null;
    let at: LiftPoint = { x: 0, y: 0 };
    let waiting: ReturnType<typeof setTimeout> | null = null;
    let ghost: Ghost | null = null;
    let scroller: HTMLElement | null = null;
    let frame: number | null = null;

    const stopWaiting = () => {
      if (waiting !== null) clearTimeout(waiting);
      waiting = null;
      origin = null;
    };

    const putDown = () => {
      stopWaiting();
      if (frame !== null) cancelAnimationFrame(frame);
      frame = null;
      ghost?.element.remove();
      ghost = null;
      scroller = null;
      holdStill(false);
      setIsCarried(false);
    };

    // The list keeps moving while the finger stands still near an edge, so a group
    // that is off screen is still somewhere the row can be taken.
    const pullAlong = () => {
      frame = null;
      if (!ghost) return;
      const step = edgePull(at.y, viewOf(scroller));
      if (!step) return;
      scrollAlong(scroller, step);
      carryOver(dropTargetAt(at, placesOnScreen()));
      frame = requestAnimationFrame(pullAlong);
    };

    const pickUp = () => {
      waiting = null;
      ghost = carryGhost(element, at);
      scroller = scrollerOf(element);
      holdStill(true);
      beginLift(latest.current);
      carryOver(dropTargetAt(at, placesOnScreen()));
      setIsCarried(true);
    };

    const pointOf = (event: TouchEvent): LiftPoint | null => {
      const touch = event.touches[0] ?? event.changedTouches[0];
      return touch ? { x: touch.clientX, y: touch.clientY } : null;
    };

    const onStart = (event: Event) => {
      const touchEvent = event as TouchEvent;
      if (ghost) return;
      const landed = pointOf(touchEvent);
      if (!landed || touchEvent.touches.length !== 1) {
        stopWaiting();
        return;
      }
      origin = landed;
      at = landed;
      waiting = setTimeout(pickUp, LIFT_DELAY_MS);
    };

    const onMove = (event: Event) => {
      const touchEvent = event as TouchEvent;
      const now = pointOf(touchEvent);
      if (!now) return;
      if (!ghost) {
        const strayed = touchEvent.touches.length !== 1 || movedBeyond(origin ?? now, now);
        if (origin && strayed) stopWaiting();
        return;
      }
      if (event.cancelable) event.preventDefault();
      at = now;
      moveGhost(ghost, now);
      carryOver(dropTargetAt(now, placesOnScreen()));
      if (frame === null && edgePull(now.y, viewOf(scroller)) !== 0) {
        frame = requestAnimationFrame(pullAlong);
      }
    };

    const onEnd = () => {
      if (!ghost) {
        stopWaiting();
        return;
      }
      releaseLift();
      putDown();
    };

    // A cancel is not a release: a gesture the browser took away files nothing.
    const onCancel = () => {
      if (!ghost) {
        stopWaiting();
        return;
      }
      cancelLift();
      putDown();
    };

    element.addEventListener('touchstart', onStart, { passive: true });
    element.addEventListener('touchmove', onMove, { passive: false });
    element.addEventListener('touchend', onEnd, { passive: true });
    element.addEventListener('touchcancel', onCancel, { passive: true });
    return () => {
      element.removeEventListener('touchstart', onStart);
      element.removeEventListener('touchmove', onMove);
      element.removeEventListener('touchend', onEnd);
      element.removeEventListener('touchcancel', onCancel);
      if (ghost) cancelLift();
      putDown();
    };
  }, [canFile, row]);

  return isCarried;
}

/**
 * A PLACE THAT TAKES A CARRIED SESSION: the mark the hit test looks for, and whether
 * the finger is over it right now — the same highlight a mouse drag lights, so both
 * hands file a session the same way.
 *
 * A place with nothing to do with a dropped row offers nothing and never lights.
 */
export function useSessionDropTarget(onDropSession?: (sid: string) => void): {
  isOver: boolean;
  targetProps: DropTargetProps;
} {
  const key = useId();
  const [isOver, setIsOver] = useState(false);
  const take = useRef(onDropSession);
  useEffect(() => {
    take.current = onDropSession;
  });

  const canTake = Boolean(onDropSession);
  useEffect(() => {
    if (!canTake) return;
    const withdraw = offerLift(key, (sid) => take.current?.(sid));
    const stopWatching = watchLift(() => setIsOver(liftedSession()?.over === key));
    return () => {
      withdraw();
      stopWatching();
      setIsOver(false);
    };
  }, [canTake, key]);

  return { isOver, targetProps: canTake ? { 'data-session-drop': key } : {} };
}

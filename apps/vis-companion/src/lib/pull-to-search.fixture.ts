/**
 * A touch drag that jsdom can actually carry.
 *
 * jsdom implements `TouchEvent` but not `Touch`, so `fireEvent.touchStart(el, {
 * touches: [...] })` cannot describe a finger at all — the init dictionary
 * refuses a plain object, and an event built without one arrives with an empty
 * `TouchList`. These helpers dispatch the same events carrying the only two
 * things the gesture reads: how many fingers are down, and where the first one
 * is. That is the whole surface `usePullToSearch` listens to.
 */

import type { PullPoint } from './pull-to-search';

export type TouchPhase = 'touchstart' | 'touchmove' | 'touchend' | 'touchcancel';

/** Dispatch one touch event on `element` with `fingers` down. */
export function fireTouch(element: Element, type: TouchPhase, fingers: PullPoint[]): void {
  const event = new Event(type, { bubbles: true, cancelable: true });
  const touches = fingers.map((finger) => ({ clientX: finger.x, clientY: finger.y }));
  Object.defineProperty(event, 'touches', { value: touches });
  Object.defineProperty(event, 'changedTouches', { value: touches });
  element.dispatchEvent(event);
}

/**
 * A whole gesture: land at `from`, travel through `through`, and leave. A drag
 * that ends in `cancel` is the one the browser took away mid-flight, which is
 * never a release.
 */
export function drag(
  element: Element,
  from: PullPoint,
  through: PullPoint[],
  ending: 'lift' | 'cancel' = 'lift',
): void {
  fireTouch(element, 'touchstart', [from]);
  for (const at of through) fireTouch(element, 'touchmove', [at]);
  fireTouch(element, ending === 'lift' ? 'touchend' : 'touchcancel', []);
}

/** Pull straight down by `distance` from the top of the list, and lift. */
export function pullDown(element: Element, distance: number, ending: 'lift' | 'cancel' = 'lift'): void {
  const from = { x: 180, y: 120 };
  const steps = [Math.round(distance / 2), distance].map((down) => ({ x: from.x, y: from.y + down }));
  drag(element, from, steps, ending);
}

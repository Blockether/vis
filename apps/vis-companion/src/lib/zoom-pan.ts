/**
 * The zoom and pan of a picture viewer, as pure geometry.
 *
 * Nothing here touches the DOM. A gesture is a value, a transform is a value,
 * and the component that owns the pointers only decides WHEN to ask — so
 * "two fingers scale about their own midpoint" and "back to 1:1 recentres" are
 * things a test can state, instead of things you can only find out by pinching
 * a phone.
 */

export type Point = { x: number; y: number };
export type Transform = { scale: number; x: number; y: number };

/**
 * A picture zooms out to half the frame and in to 6x. Below 1:1 there is paper
 * around it, which is the whole point of zooming out: seeing the page whole.
 */
export const MIN_SCALE = 0.5;
export const MAX_SCALE = 6;

/** 1:1 — the scale the viewer opens at, and the floor for panning. */
export const FIT_SCALE = 1;

/** Fitted and centred: what the viewer opens at and what Reset returns to. */
export const NO_TRANSFORM: Transform = { scale: 1, x: 0, y: 0 };

export type PinchGesture = {
  kind: "pinch";
  distance: number;
  midpoint: Point;
  transform: Transform;
};
export type PanGesture = {
  kind: "pan";
  pointerId: number;
  start: Point;
  transform: Transform;
};
export type SwipeGesture = {
  kind: "swipe";
  pointerId: number;
  start: Point;
};
export type Gesture = PanGesture | PinchGesture | SwipeGesture | null;

export function clamp(value: number, min: number, max: number): number {
  return Math.min(max, Math.max(min, value));
}

export function distance(a: Point, b: Point): number {
  return Math.hypot(b.x - a.x, b.y - a.y);
}

export function midpoint(a: Point, b: Point): Point {
  return { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 };
}

/**
 * Inside the scale range — and centred again once it is at or under 1:1, so a
 * picture pinched down can never be left parked off screen.
 */
export function clampTransform(next: Transform): Transform {
  const scale = clamp(next.scale, MIN_SCALE, MAX_SCALE);
  // At or below 1:1 the picture fits, so an offset could only strand it.
  return scale <= FIT_SCALE
    ? { ...NO_TRANSFORM, scale }
    : { scale, x: next.x, y: next.y };
}

/** A button or a wheel notch: scale about the centre, keeping the offset. */
export function zoomedBy(current: Transform, factor: number): Transform {
  return clampTransform({ ...current, scale: current.scale * factor });
}

/**
 * A wheel event's travel in CSS pixels, whatever unit the browser chose to report
 * it in: Firefox sends lines, a page-scroll key sends pages, everything else pixels.
 */
const LINE_PIXELS = 16;
const PAGE_PIXELS = 800;

/**
 * How much of a zoom one scrolled pixel buys. A trackpad reports a glide as ~60
 * events a second carrying 1-4px each; a mouse notch arrives as a single ~100px
 * event. Zoom is therefore a function of the DISTANCE scrolled — `e^(-pixels·rate)`,
 * which composes: two half-flicks equal one whole one, in any browser, at any event
 * rate. A ctrl+wheel is a trackpad PINCH, whose deltas are far smaller for the same
 * intent, so it answers on its own rate.
 */
const SCROLL_RATE = 0.0025;
const PINCH_RATE = 0.01;

/** No single event may move the picture more than one mouse notch. */
export const WHEEL_STEP_LIMIT = 1.25;

/** The factor one wheel event asks for: proportional, normalized and capped. */
export function wheelFactor(
  deltaY: number,
  deltaMode: number,
  isPinch: boolean,
): number {
  const pixels =
    deltaMode === 1
      ? deltaY * LINE_PIXELS
      : deltaMode === 2
        ? deltaY * PAGE_PIXELS
        : deltaY;
  const factor = Math.exp(-pixels * (isPinch ? PINCH_RATE : SCROLL_RATE));
  return clamp(factor, 1 / WHEEL_STEP_LIMIT, WHEEL_STEP_LIMIT);
}

/**
 * Zoom about a POINT rather than about the frame's middle: the pixel under the
 * cursor (or between the two trackpad fingers) stays under it. Zooming about the
 * centre slides whatever you aimed at off the screen, which is what makes a
 * cursor-driven zoom feel like it went somewhere else.
 *
 * `point` and `center` are both in client coordinates; `center` is the frame's own
 * middle, which is the origin the transform is written against.
 */
export function zoomedAbout(
  current: Transform,
  factor: number,
  point: Point,
  center: Point,
): Transform {
  const scale = clamp(current.scale * factor, MIN_SCALE, MAX_SCALE);
  const applied = scale / current.scale;
  const dx = point.x - center.x;
  const dy = point.y - center.y;
  return clampTransform({
    scale,
    x: dx - (dx - current.x) * applied,
    y: dy - (dy - current.y) * applied,
  });
}

/** The transform a pinch has reached: scaled by the spread, following the midpoint. */
export function pinchTransform(
  gesture: PinchGesture,
  a: Point,
  b: Point,
): Transform {
  const center = midpoint(a, b);
  return clampTransform({
    scale: gesture.transform.scale * (distance(a, b) / gesture.distance),
    x: gesture.transform.x + center.x - gesture.midpoint.x,
    y: gesture.transform.y + center.y - gesture.midpoint.y,
  });
}

/** The transform a drag has reached: the picture follows the finger exactly. */
export function panTransform(gesture: PanGesture, point: Point): Transform {
  return clampTransform({
    scale: gesture.transform.scale,
    x: gesture.transform.x + point.x - gesture.start.x,
    y: gesture.transform.y + point.y - gesture.start.y,
  });
}

/** The gesture two live pointers start, pinned to the transform they start from. */
export function pinchFrom(
  a: Point,
  b: Point,
  transform: Transform,
): PinchGesture {
  return {
    kind: "pinch",
    distance: Math.max(1, distance(a, b)),
    midpoint: midpoint(a, b),
    transform: { ...transform },
  };
}

/** The gesture one live pointer starts, pinned to the transform it starts from. */
export function panFrom(
  pointerId: number,
  start: Point,
  transform: Transform,
): PanGesture {
  return { kind: "pan", pointerId, start, transform: { ...transform } };
}

/**
 * How far a finger carries a FITTED picture sideways before it means the
 * neighbouring one: short enough for a thumb on a phone, long enough that a
 * double tap or a crooked lift is never mistaken for a swipe.
 */
export const SWIPE_TRAVEL = 56;

/** At either end the picture only creeps: there is nothing to bring in. */
export const SWIPE_RESISTANCE = 4;

/** The gesture one live pointer starts on a picture that is already fitted. */
export function swipeFrom(pointerId: number, start: Point): SwipeGesture {
  return { kind: "swipe", pointerId, start };
}

/**
 * How far the picture follows a swiping finger.
 *
 * A drag steeper than it is wide is not a swipe, so a thumb sliding down a tall
 * picture never drags the gallery sideways; and at the ends the picture RESISTS,
 * which is how glass says "nothing that way" now that no disabled arrow says it.
 */
export function swipeShift(
  gesture: SwipeGesture,
  point: Point,
  neighbours: { back: boolean; forward: boolean },
): number {
  const shift = point.x - gesture.start.x;
  if (Math.abs(shift) <= Math.abs(point.y - gesture.start.y)) return 0;
  return (shift < 0 ? neighbours.forward : neighbours.back)
    ? shift
    : shift / SWIPE_RESISTANCE;
}

/**
 * Which neighbour a finished swipe asked for: -1 the previous picture, 1 the
 * next, 0 a drag that never became a swipe. Dragging LEFT pushes this picture
 * off screen, which brings the NEXT one in.
 */
export function swipeStep(gesture: SwipeGesture, point: Point): -1 | 0 | 1 {
  const shift = point.x - gesture.start.x;
  if (Math.abs(shift) < SWIPE_TRAVEL) return 0;
  if (Math.abs(shift) <= Math.abs(point.y - gesture.start.y)) return 0;
  return shift < 0 ? 1 : -1;
}

/** GPU-composited: a transform is written to style, never re-rendered through React. */
export function transformCss(transform: Transform): string {
  return `translate3d(${transform.x}px, ${transform.y}px, 0) scale(${transform.scale})`;
}

/** What the zoom readout says. */
export function zoomLabel(transform: Transform): string {
  return `${Math.round(transform.scale * 100)}%`;
}

/** A rectangle in FRACTIONS of a picture: 0,0,1,1 is the whole of it. */
export type Rect = { x: number; y: number; width: number; height: number };

/** Any box in client coordinates — a `DOMRect` is one. */
export type Box = { left: number; top: number; width: number; height: number };

/**
 * How much of a picture may be lost before "trim to view" has anything to do.
 * A pinch parks the picture a fraction of a pixel off 1:1 all the time, and
 * cropping half a pixel off a screenshot is a no-op the human paid a tap for.
 */
const TRIM_EPSILON = 0.005;

/**
 * The part of `picture` that `frame` actually SHOWS, as fractions of the
 * picture's own pixels — the crop "trim to view" writes.
 *
 * It is read off the two boxes on screen rather than re-derived from the
 * transform, so it stays true however the picture arrived at where it is:
 * pinched, panned, wheeled, or laid out by the frame's own padding.
 *
 * `null` means there is nothing to trim — either the frame already shows the
 * whole picture, or the picture has been panned entirely out of it.
 */
export function visiblePart(picture: Box, frame: Box): Rect | null {
  if (picture.width <= 0 || picture.height <= 0) return null;
  const left = Math.max(picture.left, frame.left);
  const top = Math.max(picture.top, frame.top);
  const right = Math.min(picture.left + picture.width, frame.left + frame.width);
  const bottom = Math.min(picture.top + picture.height, frame.top + frame.height);
  if (right <= left || bottom <= top) return null;
  const part: Rect = {
    x: (left - picture.left) / picture.width,
    y: (top - picture.top) / picture.height,
    width: (right - left) / picture.width,
    height: (bottom - top) / picture.height,
  };
  const whole =
    part.x <= TRIM_EPSILON &&
    part.y <= TRIM_EPSILON &&
    part.width >= 1 - TRIM_EPSILON &&
    part.height >= 1 - TRIM_EPSILON;
  return whole ? null : part;
}

/** What that crop measures in the picture's OWN pixels, rounded to whole ones. */
export function partPixels(
  part: Rect,
  width: number,
  height: number,
): { width: number; height: number } {
  return {
    width: Math.max(1, Math.round(part.width * width)),
    height: Math.max(1, Math.round(part.height * height)),
  };
}

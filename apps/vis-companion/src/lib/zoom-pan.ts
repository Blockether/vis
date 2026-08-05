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

/** A picture never shrinks below the frame it is shown in, and never past 6x. */
export const MIN_SCALE = 1;
export const MAX_SCALE = 6;

/** Fitted and centred: what the viewer opens at and what Reset returns to. */
export const NO_TRANSFORM: Transform = { scale: 1, x: 0, y: 0 };

export type PinchGesture = {
  kind: 'pinch';
  distance: number;
  midpoint: Point;
  transform: Transform;
};
export type PanGesture = {
  kind: 'pan';
  pointerId: number;
  start: Point;
  transform: Transform;
};
export type Gesture = PanGesture | PinchGesture | null;

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
 * Inside the scale range — and centred again the moment it is back to 1:1, so a
 * picture pinched down can never be left parked off screen.
 */
export function clampTransform(next: Transform): Transform {
  const scale = clamp(next.scale, MIN_SCALE, MAX_SCALE);
  return scale === MIN_SCALE
    ? { ...NO_TRANSFORM }
    : { scale, x: next.x, y: next.y };
}

/** A button or a wheel notch: scale about the centre, keeping the offset. */
export function zoomedBy(current: Transform, factor: number): Transform {
  return clampTransform({ ...current, scale: current.scale * factor });
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
    kind: 'pinch',
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
  return { kind: 'pan', pointerId, start, transform: { ...transform } };
}

/** GPU-composited: a transform is written to style, never re-rendered through React. */
export function transformCss(transform: Transform): string {
  return `translate3d(${transform.x}px, ${transform.y}px, 0) scale(${transform.scale})`;
}

/** What the zoom readout says. */
export function zoomLabel(transform: Transform): string {
  return `${Math.round(transform.scale * 100)}%`;
}

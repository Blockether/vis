import { describe, expect, it } from "vitest";
import {
  MAX_SCALE,
  MIN_SCALE,
  NO_TRANSFORM,
  clampTransform,
  panFrom,
  panTransform,
  pinchFrom,
  pinchTransform,
  transformCss,
  zoomLabel,
  zoomedBy,
} from "./zoom-pan";

// The viewer's geometry, stated without a screen: pinching a phone used to be
// the only way to find out what any of this did.
describe("zoom limits", () => {
  it("zooms out to half the frame and never magnifies past 6x", () => {
    expect(clampTransform({ scale: 0.2, x: 0, y: 0 }).scale).toBe(MIN_SCALE);
    expect(clampTransform({ scale: 0.5, x: 0, y: 0 }).scale).toBe(0.5);
    expect(clampTransform({ scale: 99, x: 0, y: 0 }).scale).toBe(MAX_SCALE);
  });

  // A picture pinched back down but left dragged off screen is a picture the
  // human cannot get back, so 1:1 always means centred.
  it("recentres at or below 1:1", () => {
    expect(clampTransform({ scale: 1, x: -400, y: 260 })).toEqual(NO_TRANSFORM);
    expect(clampTransform({ scale: 0.5, x: -400, y: 260 })).toEqual({
      scale: 0.5,
      x: 0,
      y: 0,
    });
    expect(clampTransform({ scale: 2, x: -400, y: 260 })).toEqual({
      scale: 2,
      x: -400,
      y: 260,
    });
  });

  it("keeps the offset while it is zoomed in", () => {
    expect(zoomedBy({ scale: 2, x: 30, y: -10 }, 1.35)).toEqual({
      scale: 2.7,
      x: 30,
      y: -10,
    });
    expect(zoomedBy({ scale: 1.2, x: 30, y: -10 }, 0.1)).toEqual({
      scale: MIN_SCALE,
      x: 0,
      y: 0,
    });
  });
});

describe("gestures", () => {
  it("scales a pinch by the spread of the two fingers", () => {
    const gesture = pinchFrom({ x: 0, y: 0 }, { x: 100, y: 0 }, NO_TRANSFORM);
    const next = pinchTransform(gesture, { x: 0, y: 0 }, { x: 200, y: 0 });
    expect(next.scale).toBeCloseTo(2, 5);
  });

  // The picture has to stay under the fingers: a pinch that scales without
  // following its own midpoint slides away from the detail being examined.
  it("follows the midpoint the fingers moved to", () => {
    const gesture = pinchFrom(
      { x: 0, y: 0 },
      { x: 100, y: 0 },
      { scale: 2, x: 0, y: 0 },
    );
    const next = pinchTransform(gesture, { x: 40, y: 20 }, { x: 140, y: 20 });
    expect(next.x).toBeCloseTo(40, 5);
    expect(next.y).toBeCloseTo(20, 5);
    expect(next.scale).toBeCloseTo(2, 5);
  });

  it("moves a pan by exactly what the finger travelled", () => {
    const gesture = panFrom(1, { x: 10, y: 10 }, { scale: 3, x: 5, y: 5 });
    expect(panTransform(gesture, { x: 30, y: 0 })).toEqual({
      scale: 3,
      x: 25,
      y: -5,
    });
  });

  // A pan at 1:1 has nothing to reveal, so it is absorbed rather than left as a
  // picture floating away from its own frame.
  it("cannot drag a picture that is not zoomed", () => {
    const gesture = panFrom(1, { x: 0, y: 0 }, NO_TRANSFORM);
    expect(panTransform(gesture, { x: 120, y: 90 })).toEqual(NO_TRANSFORM);
  });
});

describe("what the screen shows", () => {
  it("composites on the GPU and reads out a whole percentage", () => {
    expect(transformCss({ scale: 2, x: -3, y: 4 })).toBe(
      "translate3d(-3px, 4px, 0) scale(2)",
    );
    expect(zoomLabel({ scale: 1.356, x: 0, y: 0 })).toBe("136%");
  });
});

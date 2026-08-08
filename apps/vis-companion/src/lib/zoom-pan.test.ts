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
  zoomedAbout,
  zoomedBy,
  wheelFactor,
  WHEEL_STEP_LIMIT,
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

// Regression, user report ("on desktop and safari the zooming is too fast and not
// reliable"): a wheel notch and a trackpad's continuous glide arrived at the same
// handler and both moved the picture a FIXED 15%. A Magic Trackpad emits ~60 wheel
// events a second carrying deltaY of 1-4px each, so a flick that meant "a little
// closer" multiplied by 1.15 sixty times and slammed into the 6x ceiling. Zoom is a
// function of HOW MUCH was scrolled, not of how many events the browser chose to
// send, and one event may never move it more than a notch.
describe("a wheel, a trackpad and a Safari pinch", () => {
  it("zooms by the distance scrolled, not by the event count", () => {
    // One mouse notch on a desktop: about a quarter closer, in one step.
    expect(wheelFactor(-100, 0, false)).toBeCloseTo(WHEEL_STEP_LIMIT, 5);
    expect(wheelFactor(100, 0, false)).toBeCloseTo(1 / WHEEL_STEP_LIMIT, 5);

    // One frame of a trackpad glide: a fraction of a percent, so sixty of them in a
    // second stay in the same neighbourhood as a single notch instead of pinning at 6x.
    const glide = wheelFactor(-2, 0, false);
    expect(glide).toBeGreaterThan(1);
    expect(glide).toBeLessThan(1.01);
    expect(glide ** 60).toBeLessThan(4);
  });

  it("reads a line and a page of scroll as the pixels they stand for", () => {
    expect(wheelFactor(-3, 1, false)).toBeCloseTo(
      wheelFactor(-48, 0, false),
      6,
    );
    expect(wheelFactor(-1, 2, false)).toBeCloseTo(
      wheelFactor(-800, 0, false),
      6,
    );
  });

  it("never lets one event move more than a notch", () => {
    expect(wheelFactor(-100000, 0, false)).toBe(WHEEL_STEP_LIMIT);
    expect(wheelFactor(100000, 0, true)).toBe(1 / WHEEL_STEP_LIMIT);
  });

  // A trackpad pinch (ctrl+wheel on Chrome/Firefox) carries much smaller deltas than
  // a scroll for the same intent, so it answers on its own rate — otherwise pinching
  // to zoom feels dead next to scrolling.
  it("answers a pinch faster than a scroll of the same delta", () => {
    expect(wheelFactor(-4, 0, true)).toBeGreaterThan(wheelFactor(-4, 0, false));
  });

  // Zooming about the FRAME centre moves whatever was under the cursor away from it,
  // which is what "not reliable" is: you aim at a detail and the picture slides off.
  it("keeps the pixel under the cursor under the cursor", () => {
    const center = { x: 200, y: 150 };
    const cursor = { x: 260, y: 110 };
    const before = { scale: 1.5, x: 12, y: -8 };
    const after = zoomedAbout(before, 1.6, cursor, center);

    const at = (t: typeof before) => ({
      x: (cursor.x - center.x - t.x) / t.scale,
      y: (cursor.y - center.y - t.y) / t.scale,
    });
    expect(at(after).x).toBeCloseTo(at(before).x, 6);
    expect(at(after).y).toBeCloseTo(at(before).y, 6);
    expect(after.scale).toBeCloseTo(2.4, 6);
  });

  it("still obeys the ceiling and recentres at or below 1:1", () => {
    const center = { x: 200, y: 150 };
    expect(
      zoomedAbout({ scale: 5, x: 0, y: 0 }, 4, { x: 260, y: 110 }, center)
        .scale,
    ).toBe(6);
    expect(
      zoomedAbout(
        { scale: 1.2, x: 40, y: 40 },
        0.1,
        { x: 260, y: 110 },
        center,
      ),
    ).toEqual({
      scale: MIN_SCALE,
      x: 0,
      y: 0,
    });
  });
});

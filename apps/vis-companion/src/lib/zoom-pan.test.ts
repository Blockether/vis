import { describe, expect, it } from 'vitest';
import {
  MAX_SCALE,
  MIN_SCALE,
  NO_TRANSFORM,
  KEEP_IN_VIEW,
  clampTransform,
  keptInView,
  panFrom,
  panTransform,
  pinchFrom,
  pinchTransform,
  zoomedAbout,
  zoomedBy,
  wheelFactor,
  WHEEL_STEP_LIMIT,
  partPixels,
  visiblePart,
  SWIPE_TRAVEL,
  swipeFrom,
  swipeShift,
  swipeStep,
  DOUBLE_TAP_MS,
  DOUBLE_TAP_PX,
  DOUBLE_TAP_ZOOM,
  isDoubleTap,
  toggledZoom,
} from './zoom-pan';

// The viewer's geometry, stated without a screen: pinching a phone used to be
// the only way to find out what any of this did.
describe('zoom limits', () => {
  it('zooms out to half the frame and never magnifies past 6x', () => {
    expect(clampTransform({ scale: 0.2, x: 0, y: 0 }).scale).toBe(MIN_SCALE);
    expect(clampTransform({ scale: 0.5, x: 0, y: 0 }).scale).toBe(0.5);
    expect(clampTransform({ scale: 99, x: 0, y: 0 }).scale).toBe(MAX_SCALE);
  });

  // A picture zoomed out is a page on a desk: it stays where it was moved to.
  // Reset is the way back to fitted and centred.
  it('keeps the offset at every scale', () => {
    expect(clampTransform({ scale: 0.5, x: -400, y: 260 })).toEqual({
      scale: 0.5,
      x: -400,
      y: 260,
    });
    expect(clampTransform({ scale: 2, x: -400, y: 260 })).toEqual({
      scale: 2,
      x: -400,
      y: 260,
    });
  });

  // A button zooms about the middle of the frame: what is there stays there, so
  // a picture that was moved far at 3x comes back toward the middle on the way out.
  it('zooms a button step about the middle of the frame', () => {
    const closer = zoomedBy({ scale: 2, x: 30, y: -10 }, 1.5);
    expect(closer).toEqual({ scale: 3, x: 45, y: -15 });
    const out = zoomedBy({ scale: 3, x: -900, y: 300 }, 0.01);
    expect(out.scale).toBe(MIN_SCALE);
    expect(out.x).toBeCloseTo(-150, 6);
    expect(out.y).toBeCloseTo(50, 6);
  });

  // A picture that can be moved at every scale must not be lost: a part of it
  // always stays in the frame, so there is always something to take hold of.
  it('keeps a part of a moved picture in the frame', () => {
    const frame = { width: 1000, height: 600 };
    const picture = { width: 800, height: 400 };
    const far = keptInView({ scale: 0.5, x: 5000, y: -5000 }, picture, frame);
    expect(far).toEqual({ scale: 0.5, x: 700 - KEEP_IN_VIEW, y: -(400 - KEEP_IN_VIEW) });
    const near = { scale: 2, x: 120, y: -80 };
    expect(keptInView(near, picture, frame)).toEqual(near);
    // A picture smaller than the kept part stays in the frame whole.
    expect(keptInView({ scale: 0.5, x: 5000, y: 0 }, { width: 100, height: 100 }, frame).x).toBe(
      475,
    );
  });
});

describe('gestures', () => {
  it('scales a pinch by the spread of the two fingers', () => {
    const gesture = pinchFrom({ x: 0, y: 0 }, { x: 100, y: 0 }, NO_TRANSFORM);
    const next = pinchTransform(gesture, { x: 0, y: 0 }, { x: 200, y: 0 });
    expect(next.scale).toBeCloseTo(2, 5);
  });

  // The picture has to stay under the fingers: a pinch that scales without
  // following its own midpoint slides away from the detail being examined.
  it('follows the midpoint the fingers moved to', () => {
    const gesture = pinchFrom({ x: 0, y: 0 }, { x: 100, y: 0 }, { scale: 2, x: 0, y: 0 });
    const next = pinchTransform(gesture, { x: 40, y: 20 }, { x: 140, y: 20 });
    expect(next.x).toBeCloseTo(40, 5);
    expect(next.y).toBeCloseTo(20, 5);
    expect(next.scale).toBeCloseTo(2, 5);
  });

  it('moves a pan by exactly what the finger travelled', () => {
    const gesture = panFrom(1, { x: 10, y: 10 }, { scale: 3, x: 5, y: 5 });
    expect(panTransform(gesture, { x: 30, y: 0 })).toEqual({
      scale: 3,
      x: 25,
      y: -5,
    });
  });

  // Regression, user report (paraphrased: "to move the picture I must zoom in a
  // lot; a drag on the dark area beside it does not move it"): every pan at or
  // below 1:1 was set back to the centre, so a zoomed-out picture did not move.
  it('drags a picture that is zoomed out', () => {
    const gesture = panFrom(1, { x: 0, y: 0 }, { scale: 0.69, x: 0, y: 0 });
    expect(panTransform(gesture, { x: 120, y: 90 })).toEqual({ scale: 0.69, x: 120, y: 90 });
  });
});

// Regression, user report ("on desktop and safari the zooming is too fast and not
// reliable"): a wheel notch and a trackpad's continuous glide arrived at the same
// handler and both moved the picture a FIXED 15%. A Magic Trackpad emits ~60 wheel
// events a second carrying deltaY of 1-4px each, so a flick that meant "a little
// closer" multiplied by 1.15 sixty times and slammed into the 6x ceiling. Zoom is a
// function of HOW MUCH was scrolled, not of how many events the browser chose to
// send, and one event may never move it more than a notch.
describe('a wheel, a trackpad and a Safari pinch', () => {
  it('zooms by the distance scrolled, not by the event count', () => {
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

  it('reads a line and a page of scroll as the pixels they stand for', () => {
    expect(wheelFactor(-3, 1, false)).toBeCloseTo(wheelFactor(-48, 0, false), 6);
    expect(wheelFactor(-1, 2, false)).toBeCloseTo(wheelFactor(-800, 0, false), 6);
  });

  it('never lets one event move more than a notch', () => {
    expect(wheelFactor(-100000, 0, false)).toBe(WHEEL_STEP_LIMIT);
    expect(wheelFactor(100000, 0, true)).toBe(1 / WHEEL_STEP_LIMIT);
  });

  // Zooming about the FRAME centre moves whatever was under the cursor away from it,
  // which is what "not reliable" is: you aim at a detail and the picture slides off.
  it('keeps the pixel under the cursor under the cursor', () => {
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

  it('still obeys the ceiling and the floor', () => {
    const center = { x: 200, y: 150 };
    expect(zoomedAbout({ scale: 5, x: 0, y: 0 }, 4, { x: 260, y: 110 }, center).scale).toBe(6);
    expect(zoomedAbout({ scale: 1.2, x: 40, y: 40 }, 0.1, { x: 260, y: 110 }, center).scale).toBe(
      MIN_SCALE,
    );
  });
});
// "Trim to view" is the answer to zooming into a detail and then wanting only
// that detail: the region is chosen with the same pinch that reads it, and
// stated here as two boxes so no phone is needed to find out what it keeps.
describe('trim to view', () => {
  const frame = { left: 0, top: 0, width: 400, height: 300 };

  it('keeps only the part of the picture the frame shows', () => {
    // Twice as wide and twice as tall as the frame, centred: the middle quarter
    // is what is on screen.
    const part = visiblePart({ left: -200, top: -150, width: 800, height: 600 }, frame);
    expect(part).toEqual({ x: 0.25, y: 0.25, width: 0.5, height: 0.5 });
  });

  it('follows a pan, so the crop is wherever the picture was dragged to', () => {
    const part = visiblePart({ left: -400, top: -300, width: 800, height: 600 }, frame);
    expect(part).toEqual({ x: 0.5, y: 0.5, width: 0.5, height: 0.5 });
  });

  // A fitted picture has nothing outside the frame, and a pinch parks a
  // fraction of a pixel off 1:1 constantly — cropping that is a tap wasted.
  it('has nothing to trim when the whole picture is in view', () => {
    expect(visiblePart({ left: 40, top: 20, width: 320, height: 260 }, frame)).toBeNull();
    expect(visiblePart({ left: -0.4, top: -0.3, width: 400.8, height: 300.6 }, frame)).toBeNull();
  });

  it('has nothing to trim when the picture is off screen or has no size', () => {
    expect(visiblePart({ left: 900, top: 0, width: 200, height: 200 }, frame)).toBeNull();
    expect(visiblePart({ left: 0, top: 0, width: 0, height: 0 }, frame)).toBeNull();
  });

  // The screen only says WHICH pixels: the crop is cut at the original's own
  // resolution, never at the resolution it happened to be displayed with.
  it("measures the crop in the picture's own pixels", () => {
    expect(partPixels({ x: 0.25, y: 0.25, width: 0.5, height: 0.5 }, 4000, 3000)).toEqual({
      width: 2000,
      height: 1500,
    });
    // Never a zero-pixel image, however thin the sliver.
    expect(partPixels({ x: 0, y: 0, width: 0.0001, height: 1 }, 100, 100)).toEqual({
      width: 1,
      height: 100,
    });
  });
});

// Regression, user report ("in the application we should not have the left right
// … on iOS and android we should have swipes working"): a gallery could only be
// walked from two arrow buttons on the toolbar, so on glass the one gesture a
// reader actually makes moved nothing.
describe('a swipe through a gallery', () => {
  const swipe = swipeFrom(1, { x: 200, y: 300 });
  const both = { back: true, forward: true };

  it('asks for the next picture when the finger pushes this one off to the left', () => {
    expect(swipeStep(swipe, { x: 200 - SWIPE_TRAVEL, y: 300 })).toBe(1);
    expect(swipeStep(swipe, { x: 200 + SWIPE_TRAVEL, y: 300 })).toBe(-1);
  });

  it('ignores a drag that stopped short, and one steeper than it is wide', () => {
    expect(swipeStep(swipe, { x: 200 - SWIPE_TRAVEL + 1, y: 300 })).toBe(0);
    expect(swipeStep(swipe, { x: 100, y: 420 })).toBe(0);
  });

  it('follows the finger exactly while there is a neighbour to bring in', () => {
    expect(swipeShift(swipe, { x: 160, y: 306 }, both)).toBe(-40);
    expect(swipeShift(swipe, { x: 240, y: 306 }, both)).toBe(40);
  });

  // The ends of a gallery are told by the picture refusing to travel, now that no
  // greyed-out arrow is there to say it.
  it('resists at the ends, where nothing can come in', () => {
    expect(swipeShift(swipe, { x: 160, y: 300 }, { back: true, forward: false })).toBe(-10);
    expect(swipeShift(swipe, { x: 240, y: 300 }, { back: false, forward: true })).toBe(10);
  });
});

describe('a double tap', () => {
  const center = { x: 200, y: 300 };

  it('zooms a fitted picture in about the tapped point, and a zoomed one back', () => {
    const zoomed = toggledZoom(NO_TRANSFORM, { x: 300, y: 300 }, center);
    expect(zoomed).toEqual({ scale: DOUBLE_TAP_ZOOM, x: -100, y: 0 });
    expect(toggledZoom(zoomed, { x: 50, y: 50 }, center)).toEqual(NO_TRANSFORM);
  });

  it('is two taps close in time and space', () => {
    const first = { at: { x: 100, y: 100 }, time: 1000 };
    expect(isDoubleTap(null, first)).toBe(false);
    expect(isDoubleTap(first, { at: { x: 110, y: 100 }, time: 1000 + DOUBLE_TAP_MS })).toBe(true);
    expect(isDoubleTap(first, { at: { x: 100, y: 100 }, time: 1001 + DOUBLE_TAP_MS })).toBe(false);
    expect(isDoubleTap(first, { at: { x: 101 + DOUBLE_TAP_PX, y: 100 }, time: 1100 })).toBe(false);
  });
});

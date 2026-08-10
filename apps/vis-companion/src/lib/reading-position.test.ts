import { beforeEach, describe, expect, it } from 'vitest';
import {
  applyReadingPosition,
  followEnd,
  forgetReadingPosition,
  isAtBottom,
  isCorrectionEcho,
  markReadingPosition,
  parkedReadingPosition,
  rememberReadingPosition,
  shouldOfferLatest,
  type ScrollBox,
} from './reading-position';

// Regression, reported: reading a session part-way up, switching to another tab
// (or back to the list) and returning put the reader at the BOTTOM again — the
// opening correction pinned to the newest turn with no memory of where they were.
describe('transcript reading position', () => {
  const box = (scrollTop: number, scrollHeight: number, clientHeight: number): ScrollBox => ({
    scrollTop,
    scrollHeight,
    clientHeight,
  });

  beforeEach(() => forgetReadingPosition('s1'));

  it('marks how far above the end the reader parked', () => {
    expect(markReadingPosition(box(1000, 5000, 800))).toBe(3200);
  });

  it('parks nothing for a reader who was at the bottom', () => {
    expect(markReadingPosition(box(4200, 5000, 800))).toBeNull();
    expect(markReadingPosition(box(4160, 5000, 800))).toBeNull();
    expect(markReadingPosition(null)).toBeNull();
  });

  it('remembers per session and forgets on demand', () => {
    rememberReadingPosition('s1', 3200);
    expect(parkedReadingPosition('s1')).toBe(3200);
    expect(parkedReadingPosition('s2')).toBeNull();
    rememberReadingPosition('s1', null);
    expect(parkedReadingPosition('s1')).toBeNull();
  });

  it('restores the same distance from the end, not the same pixel', () => {
    // History hydrated above: the transcript grew by 2000px at the TOP.
    const viewport = box(0, 7000, 800);
    expect(applyReadingPosition(viewport, 3200)).toBe(true);
    expect(viewport.scrollTop).toBe(3000);
  });

  it('reports failure while the transcript is still too short', () => {
    const viewport = box(0, 1200, 800);
    expect(applyReadingPosition(viewport, 3200)).toBe(false);
    expect(applyReadingPosition(box(0, 800, 800), 3200)).toBe(false);
  });
});

// Regression, session 004cb1f6: an iPad held in landscape sat on the newest turn of
// a live session and still wore the "↓ Latest" pill, offering the reader a way to
// where they already were. The offer was a remembered flag that only a scroll event
// refreshed — and the screen drops every scroll event for the whole rotation.
describe('the "Latest" offer', () => {
  const box = (scrollTop: number, scrollHeight: number, clientHeight: number): ScrollBox => ({
    scrollTop,
    scrollHeight,
    clientHeight,
  });

  it('offers nothing while the end is already on screen', () => {
    expect(shouldOfferLatest(box(4200, 5000, 800), false)).toBe(false);
    expect(shouldOfferLatest(box(4160, 5000, 800), false)).toBe(false);
  });

  it('offers nothing to a transcript already chasing the end', () => {
    expect(shouldOfferLatest(box(1000, 5000, 800), true)).toBe(false);
  });

  it('offers the way down to a reader parked above the end', () => {
    expect(shouldOfferLatest(box(1000, 5000, 800), false)).toBe(true);
  });

  it('offers nothing when there is nothing to scroll', () => {
    expect(shouldOfferLatest(box(0, 600, 800), false)).toBe(false);
    expect(shouldOfferLatest(null, false)).toBe(false);
  });

  it('reads the end with the same slack the parked mark uses', () => {
    expect(isAtBottom(box(4160, 5000, 800))).toBe(true);
    expect(isAtBottom(box(4135, 5000, 800))).toBe(false);
    expect(markReadingPosition(box(4135, 5000, 800))).toBe(65);
  });
});

// Regression, reported: opening a session flickered. While the transcript grew
// under a reader pinned to the newest turn — the hydration ramp, then images and
// code blocks finishing their layout — the correction was scheduled for the NEXT
// animation frame, so every growth painted once with the old `scrollTop` against
// the new height and was snapped away again. Measured on a 47 189 px session:
// 12 painted frames in 135 ms, each showing a different part of the transcript,
// and the screen came to rest 1 281 px short of the end it was following.
describe('following the end while the transcript grows', () => {
  const box = (scrollTop: number, scrollHeight: number, clientHeight: number): ScrollBox => ({
    scrollTop,
    scrollHeight,
    clientHeight,
  });

  it('takes the reader to the end however much height just landed', () => {
    const grown = box(4200, 47189, 767);
    expect(followEnd(grown)).toBe(true);
    expect(grown.scrollTop).toBe(46422);
  });

  it('bills one growth exactly once', () => {
    const grown = box(4200, 47189, 767);
    followEnd(grown);
    expect(followEnd(grown)).toBe(false);
    expect(grown.scrollTop).toBe(46422);
  });

  it('never scrolls a transcript shorter than its scroller', () => {
    const short = box(0, 600, 800);
    expect(followEnd(short)).toBe(false);
    expect(short.scrollTop).toBe(0);
  });
});


// Regression, session 15db52fb-9a44-45db-96e7-13b423eff274: opening a session walked
// the transcript for a few frames and then stopped 6 917 px above its newest turn
// with "↓ Latest" over the composer. The scroll event our own pin-to-the-end raised
// was measured a frame later, by which time the hydrating history had grown the
// transcript, so "am I at the bottom?" answered no and the screen stopped chasing.
describe('a correction is not a gesture', () => {
  const box = (scrollTop: number, scrollHeight: number, clientHeight: number): ScrollBox => ({
    scrollTop,
    scrollHeight,
    clientHeight,
  });

  it('recognises the scroller resting where the corrector left it', () => {
    expect(isCorrectionEcho(box(35_099, 35_866, 767), 35_099)).toBe(true);
  });

  it('still recognises it after the transcript grew underneath', () => {
    // The pin left 35 099; 11 000 px of history landed since, and the scroller has
    // not moved. That is our echo, not a reader who scrolled up.
    expect(isCorrectionEcho(box(35_099, 46_373, 767), 35_099)).toBe(true);
  });

  it('lets a real gesture through', () => {
    expect(isCorrectionEcho(box(28_000, 46_373, 767), 35_099)).toBe(false);
  });

  it('claims nothing before the first correction', () => {
    expect(isCorrectionEcho(box(0, 46_373, 767), -1)).toBe(false);
  });
});

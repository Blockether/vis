import { afterEach, describe, expect, it } from 'vitest';

import {
  applyListScroll,
  forgetListScroll,
  markListScroll,
  parkedListScroll,
  rememberListScroll,
  type ScrollBox,
} from './list-scroll';

// Regression: opening a session UNMOUNTS the sessions list, and the remount
// started at `scrollTop = 0`. A reader forty rows down came back to the top of
// the fleet and had to find their place again on every single return.

function box(scrollTop: number, scrollHeight: number, clientHeight: number): ScrollBox {
  return { scrollTop, scrollHeight, clientHeight };
}

afterEach(() => forgetListScroll());

describe('markListScroll', () => {
  it('has nothing to remember about a list parked at the top', () => {
    expect(markListScroll(box(0, 5000, 800), { id: 's7', offset: 0 })).toBeNull();
    expect(markListScroll(box(2, 5000, 800), { id: 's7', offset: 0 })).toBeNull();
  });

  it('keeps both the pixel offset and the row under the top edge', () => {
    expect(markListScroll(box(1200, 5000, 800), { id: 's7', offset: -20 })).toEqual({
      top: 1200,
      anchor: { id: 's7', offset: -20 },
    });
  });

  it('remembers the pixels even when no row could be identified', () => {
    expect(markListScroll(box(1200, 5000, 800), null)).toEqual({ top: 1200, anchor: null });
  });
});

describe('applyListScroll', () => {
  it('puts the anchored row back under the top edge, wherever it moved to', () => {
    // The session you just left jumps to the top of the fleet, so every row
    // below it shifts: the remembered PIXEL is one row off, the row is not.
    const viewport = box(0, 5000, 800);

    const applied = applyListScroll(viewport, { top: 1200, anchor: { id: 's7', offset: -20 } }, () => 1400);

    expect(applied).toBe(true);
    expect(viewport.scrollTop).toBe(1420);
  });

  it('falls back to the pixel offset when the anchored row is gone', () => {
    const viewport = box(0, 5000, 800);

    const applied = applyListScroll(viewport, { top: 1200, anchor: { id: 'deleted', offset: -20 } }, () => null);

    expect(applied).toBe(true);
    expect(viewport.scrollTop).toBe(1200);
  });

  it('keeps waiting while the list is still a skeleton', () => {
    const viewport = box(0, 800, 800);

    const applied = applyListScroll(viewport, { top: 1200, anchor: { id: 's7', offset: -20 } }, () => null);

    expect(applied).toBe(false);
    expect(viewport.scrollTop).toBe(0);
  });

  it('scrolls as far as it can, and says it is not done, while rows are still arriving', () => {
    const viewport = box(0, 1500, 800);

    const applied = applyListScroll(viewport, { top: 1200, anchor: null }, () => null);

    expect(applied).toBe(false);
    expect(viewport.scrollTop).toBe(700);
  });

  it('never scrolls past the end of a list that really did get shorter', () => {
    const viewport = box(0, 1000, 800);

    applyListScroll(viewport, { top: 5000, anchor: null }, () => null);

    expect(viewport.scrollTop).toBe(200);
  });

  it('clamps an anchor correction to the top of the list', () => {
    const viewport = box(300, 5000, 800);

    applyListScroll(viewport, { top: 300, anchor: { id: 's7', offset: 0 } }, () => -900);

    expect(viewport.scrollTop).toBe(0);
  });
});

describe('the parked position', () => {
  it('survives the unmount that loses the component state', () => {
    rememberListScroll({ top: 1200, anchor: { id: 's7', offset: -20 } });

    expect(parkedListScroll()).toEqual({ top: 1200, anchor: { id: 's7', offset: -20 } });
  });

  it('is forgotten once it has been put back, so a later cold list starts at the top', () => {
    rememberListScroll({ top: 1200, anchor: null });
    forgetListScroll();

    expect(parkedListScroll()).toBeNull();
  });
});

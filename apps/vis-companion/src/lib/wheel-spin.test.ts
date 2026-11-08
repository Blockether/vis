// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { keepWheelSpin } from './wheel-spin';

let now = 1_000_000;

/** A scroller as the browser lays it out: `overflow-y: auto`, with room to move. */
function mountScroller(): HTMLDivElement {
  const scroller = document.body.appendChild(document.createElement('div'));
  scroller.style.overflowY = 'auto';
  Object.defineProperties(scroller, {
    clientHeight: { value: 500 },
    scrollHeight: { value: 5_000 },
    scrollTop: { value: 0, writable: true },
  });
  scroller.scrollTo = ((options: ScrollToOptions) => {
    scroller.scrollTop = options.top ?? scroller.scrollTop;
  }) as typeof scroller.scrollTo;
  return scroller;
}

/** One notch over `target`; true when the browser may still scroll `target` itself. */
function notch(target: Element, deltaY = 100): boolean {
  now += 30;
  return target.dispatchEvent(new WheelEvent('wheel', { deltaY, bubbles: true, cancelable: true }));
}

function movePointer(): void {
  window.dispatchEvent(new Event('pointermove'));
}

describe('a wheel spin', () => {
  let transcript: HTMLDivElement;
  let list: HTMLDivElement;
  let unguard: () => void;

  beforeEach(() => {
    now += 10_000;
    vi.spyOn(Date, 'now').mockImplementation(() => now);
    transcript = mountScroller();
    list = mountScroller();
    unguard = keepWheelSpin(list);
  });

  afterEach(() => {
    unguard();
    vi.restoreAllMocks();
    document.body.replaceChildren();
  });

  // Regression, user report: a wheel spun over the session, then drifting over the
  // session list, scrolled the list.
  it('keeps moving the transcript when the pointer drifts onto the list', () => {
    notch(transcript);
    movePointer();
    expect(notch(list)).toBe(false);
    expect(notch(list)).toBe(false);

    expect(transcript.scrollTop).toBe(200);
    expect(list.scrollTop).toBe(0);
  });

  // A smooth glide is still on its way when the next notch comes. Each notch must add
  // its own distance to the target, not to where the glide is now.
  it('keeps every notch of a spin faster than its glide', () => {
    const asked: ScrollToOptions[] = [];
    transcript.scrollTo = ((options: ScrollToOptions) => {
      asked.push(options);
    }) as typeof transcript.scrollTo;
    notch(transcript);
    movePointer();
    notch(list);
    notch(list);
    notch(list);

    expect(asked.map((options) => options.top)).toEqual([100, 200, 300]);
    expect(asked.every((options) => options.behavior === 'smooth')).toBe(true);
  });

  it('starts from the scroller when the browser moved it between notches', () => {
    notch(transcript);
    movePointer();
    notch(list);
    transcript.scrollTop = 700;
    notch(transcript);
    notch(list);

    expect(transcript.scrollTop).toBe(800);
  });

  it('lets the list scroll after a pause in the spin', () => {
    notch(transcript);
    now += 301;
    expect(notch(list)).toBe(true);
  });

  it('lets the list scroll when the hand stops spinning to move', () => {
    notch(transcript);
    now += 151;
    movePointer();
    expect(notch(list)).toBe(true);
  });

  it('lets a spin that started on the list scroll the list', () => {
    expect(notch(list)).toBe(true);
    expect(notch(list)).toBe(true);
  });
});

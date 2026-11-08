// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { glideTo } from './scroll-glide';

let frames: FrameRequestCallback[] = [];

/** A 600 px scroller over 50 000 px of content. `asked` keeps each smooth scroll. */
function mountScroller(asked: ScrollToOptions[]): HTMLDivElement {
  const scroller = document.body.appendChild(document.createElement('div'));
  Object.defineProperties(scroller, {
    clientHeight: { value: 600 },
    scrollHeight: { value: 50_000 },
    scrollTop: { value: 0, writable: true },
  });
  scroller.scrollTo = ((options: ScrollToOptions) => {
    asked.push(options);
  }) as typeof scroller.scrollTo;
  return scroller;
}

function runFrame(): void {
  const due = frames;
  frames = [];
  for (const callback of due) callback(0);
}

function reduceMotion(isReduced: boolean): void {
  vi.spyOn(window, 'matchMedia').mockImplementation(
    (query: string) => ({ matches: isReduced && query.includes('reduce'), media: query }) as MediaQueryList,
  );
}

describe('glideTo', () => {
  beforeEach(() => {
    frames = [];
    vi.spyOn(window, 'requestAnimationFrame').mockImplementation((callback) => frames.push(callback));
    reduceMotion(false);
  });

  afterEach(() => {
    vi.restoreAllMocks();
    document.body.replaceChildren();
  });

  it('moves only the last screen of a long jump', () => {
    const asked: ScrollToOptions[] = [];
    const scroller = mountScroller(asked);

    glideTo(scroller, () => 40_000);

    expect(scroller.scrollTop).toBe(39_400);
    runFrame();
    expect(asked).toEqual([{ top: 40_000, behavior: 'smooth' }]);
  });

  it('jumps at once for a reader who asked for reduced motion', () => {
    reduceMotion(true);
    const asked: ScrollToOptions[] = [];
    const scroller = mountScroller(asked);
    const onEnd = vi.fn();

    glideTo(scroller, () => 40_000, onEnd);

    expect(scroller.scrollTop).toBe(40_000);
    expect(asked).toEqual([]);
    expect(onEnd).toHaveBeenCalledTimes(1);
  });

  // Measured on iOS 18: it sends no `scrollend`, and a glide ended only at a 1 s timer.
  it('ends in a browser without scrollend once the scroller rests', () => {
    vi.useFakeTimers();
    try {
      const asked: ScrollToOptions[] = [];
      const scroller = mountScroller(asked);
      const onEnd = vi.fn();

      glideTo(scroller, () => 400, onEnd);
      runFrame();
      for (let step = 0; step < 10; step++) {
        vi.advanceTimersByTime(30);
        scroller.dispatchEvent(new Event('scroll'));
      }
      expect(onEnd).not.toHaveBeenCalled();
      vi.advanceTimersByTime(200);
      expect(onEnd).toHaveBeenCalledTimes(1);
    } finally {
      vi.useRealTimers();
    }
  });

  // Measured in Chromium: the seat of a long jump sends its own `scrollend`, before
  // the smooth part starts.
  it('ends when the glide stops, not when the seat does', () => {
    const asked: ScrollToOptions[] = [];
    const scroller = mountScroller(asked);
    const onEnd = vi.fn();

    glideTo(scroller, () => 40_000, onEnd);
    scroller.dispatchEvent(new Event('scrollend'));
    expect(onEnd).not.toHaveBeenCalled();

    runFrame();
    scroller.scrollTop = 40_000;
    scroller.dispatchEvent(new Event('scrollend'));
    scroller.dispatchEvent(new Event('scrollend'));
    expect(onEnd).toHaveBeenCalledTimes(1);
  });
});

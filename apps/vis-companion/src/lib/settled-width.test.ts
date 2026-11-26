// @vitest-environment jsdom
// A window drag must not lay out the prose on each frame.
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { holdWidthWhileResizing, SETTLE_MS } from './settled-width';

let callback: () => void = () => {};
let scrollerWidth = 0;
const report = (width: number) => {
  scrollerWidth = width;
  callback();
};

beforeEach(() => {
  vi.useFakeTimers();
  vi.stubGlobal(
    'ResizeObserver',
    class {
      constructor(observed: ResizeObserverCallback) {
        callback = () => observed([], this as unknown as ResizeObserver);
      }
      observe() {}
      disconnect() {}
    },
  );
});

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

function mount() {
  const scroller = document.createElement('div');
  const column = document.createElement('div');
  Object.defineProperty(scroller, 'clientWidth', { get: () => scrollerWidth });
  let laidOut = 800;
  column.getBoundingClientRect = () => ({ width: laidOut }) as DOMRect;
  const stop = holdWidthWhileResizing(column, scroller);
  report(800);
  return { column, stop, layOut: (width: number) => (laidOut = width) };
}

describe('the reading column during a resize', () => {
  it('takes a single width change at once', () => {
    const { column, layOut } = mount();
    vi.advanceTimersByTime(500);
    layOut(500);
    report(500);
    expect(column.style.width).toBe('');
  });

  it('keeps its width while a drag steps, and takes the last width when it stops', () => {
    const { column, layOut } = mount();
    vi.advanceTimersByTime(500);
    layOut(780);
    report(780);
    for (const width of [760, 740, 720]) {
      vi.advanceTimersByTime(16);
      report(width);
    }
    expect(column.style.width).toBe('780px');

    vi.advanceTimersByTime(SETTLE_MS);
    expect(column.style.width).toBe('');
  });

  it('gives the width back when the screen goes', () => {
    const { column, stop } = mount();
    vi.advanceTimersByTime(500);
    report(780);
    vi.advanceTimersByTime(16);
    report(760);
    stop();
    expect(column.style.width).toBe('');
  });
});

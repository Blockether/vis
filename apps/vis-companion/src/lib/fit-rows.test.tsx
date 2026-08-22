// @vitest-environment jsdom
import { act } from 'react';
import { createRoot, type Root } from 'react-dom/client';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { fitRows, useFitRows, type ListGeometry } from './fit-rows';

const shell = vi.hoisted(() => ({ keyboardUp: false, rotate: null as ((phase: string) => void) | null }));

vi.mock('./viewport', () => ({
  isSoftKeyboardUp: () => shell.keyboardUp,
  onViewportRotation: (listener: (phase: string) => void) => {
    shell.rotate = listener;
    return () => {
      shell.rotate = null;
    };
  },
}));

globalThis.IS_REACT_ACT_ENVIRONMENT = true;

/** The sessions list's own geometry, as `SessionsScreen` measured it. */
const LIST: ListGeometry = { row: 49, chrome: 251, min: 3 };

describe('rows that fit the screen', () => {
  it('fills the screen and never runs past it', () => {
    // Measured on the live screen at 390x844: a row is 48px + a 1px rule and the
    // first one starts at y=211, so twelve rows end at 839 and a thirteenth would
    // be 49px below the fold. The setting that shipped before this said 10, on
    // every device, and left 144px of empty paper under the last row.
    expect(fitRows(844, LIST)).toBe(12);
    expect(251 + 12 * 49).toBeLessThanOrEqual(844);
    expect(251 + 13 * 49).toBeGreaterThan(844);

    // The same ten rows ran three past the fold on the shortest phone.
    expect(fitRows(568, LIST)).toBe(6);
    // A cursor makes the row 14px shorter, so the same window holds more of them.
    expect(fitRows(900, { row: 35, chrome: 255, min: 3 })).toBe(18);
  });

  it('never cuts a page shorter than a list, and can be given a share of the screen', () => {
    expect(fitRows(0, LIST)).toBe(3);
    expect(fitRows(120, LIST)).toBe(3);
    // `DataTable`'s sheet fills 60% of the viewport, not the whole of it.
    expect(fitRows(1000, { row: 26, chrome: 30, fraction: 0.6, min: 4 })).toBe(21);
  });
});

describe('the screen a page is cut for', () => {
  let host: HTMLDivElement;
  let root: Root;

  const mount = () => {
    function Probe() {
      return <output>{useFitRows(LIST)}</output>;
    }
    act(() => root.render(<Probe />));
  };
  const shown = () => Number(host.textContent);
  const resizeTo = (height: number) => {
    window.innerHeight = height;
    act(() => {
      window.dispatchEvent(new Event('resize'));
      vi.advanceTimersByTime(300);
    });
  };

  beforeEach(() => {
    vi.useFakeTimers();
    shell.keyboardUp = false;
    window.innerHeight = 844;
    host = document.createElement('div');
    document.body.append(host);
    root = createRoot(host);
  });

  afterEach(() => {
    act(() => root.unmount());
    host.remove();
    vi.useRealTimers();
  });

  it('recuts the page when the device really changes shape', () => {
    mount();
    expect(shown()).toBe(12);
    resizeTo(390);
    expect(shown()).toBe(fitRows(390, LIST));
    // A rotation is announced by `viewport.ts` as well, and its end is the frame
    // whose numbers are worth measuring.
    window.innerHeight = 844;
    act(() => {
      shell.rotate?.('end');
      vi.advanceTimersByTime(300);
    });
    expect(shown()).toBe(12);
  });

  it('ignores furniture: a toolbar, a scrollbar, a rounding wobble', () => {
    mount();
    expect(shown()).toBe(12);
    resizeTo(832);
    expect(shown()).toBe(12);
  });

  it('never lets a keyboard count as a smaller device', () => {
    mount();
    expect(shown()).toBe(12);
    // Android resizes its webview under the keyboard. Recutting there would take
    // half the rows out from under the thumb typing into the filter above them.
    shell.keyboardUp = true;
    resizeTo(430);
    expect(shown()).toBe(12);
    // The keyboard leaves, the screen is a screen again.
    shell.keyboardUp = false;
    resizeTo(430);
    expect(shown()).toBe(fitRows(430, LIST));
  });
});

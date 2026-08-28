import { useEffect, useState } from 'react';

import { isSoftKeyboardUp, onViewportRotation } from './viewport';
import { layoutHeight, readViewportMetrics } from './viewport-metrics';

/**
 * HOW MANY ROWS FIT — answered by the device, never by a setting.
 *
 * A page size is a question about the SCREEN, and the reader cannot answer it
 * better than the box in their hand. The setting that used to ask them
 * (`vis.sessionsPerProject`: 5, 10 or 15, spelled compact/balanced/detailed)
 * sized every project's page for a device nobody had measured — at 390x844 its
 * ten rows ended 144px above the fold and left the bottom sixth of the screen
 * empty, at 320x568 the same ten ran three rows past it, and under `mouse:`,
 * where a row is 14px shorter, it wasted half a 900px window.
 *
 * Nothing here fetches: a page is cut from rows the poll already holds, so
 * recutting one on rotation costs no request.
 */

/** A list's geometry in px: what one row costs and what a page pays around it. */
export type ListGeometry = {
  /** One row with the hairline under it — the unit a page is cut in. */
  row: number;
  /**
   * Everything a page pays for besides its rows: the bands standing above the
   * first one, and whatever has to stay visible under the last.
   */
  chrome: number;
  /** The share of the screen this list may fill; 1 unless it shares the page. */
  fraction?: number;
  /** The fewest rows a page may hold, however short the screen. */
  min: number;
};

/**
 * Rows that FILL the screen.
 *
 * Measured off the VIEWPORT and never off the scroller they are poured into: a
 * page sized from the list's own height feeds that height back into the next
 * measurement and oscillates between two counts.
 */
export function fitRows(height: number, { row, chrome, fraction = 1, min }: ListGeometry): number {
  return Math.max(min, Math.floor((height * fraction - chrome) / row));
}

/**
 * The screen a page is cut for: the LAYOUT viewport, clamped by the hardware
 * ruler (`viewport-metrics`), so a resumed WKWebView that reports a box taller
 * than the device cannot cut a page running off the bottom of it.
 *
 * Never `visualViewport` — that is the box a software keyboard shrinks.
 */
export function screenHeight(): number {
  return layoutHeight(readViewportMetrics());
}

/**
 * A height change under this is furniture, not a new screen: a scrollbar, a
 * browser toolbar collapsing, a CSS-pixel rounding wobble. Recutting on one of
 * those moves every row under the reader's thumb and almost never changes the
 * count.
 */
const SETTLED_DELTA = 24;

/** A resize arrives as a burst; the height a page is cut for is its last frame. */
const SETTLE_MS = 120;

/** The rows this screen fits right now, recut when the screen really changes. */
export function useFitRows(geometry: ListGeometry): number {
  const [height, setHeight] = useState(screenHeight);
  useEffect(() => {
    let timer: number | undefined;
    const measure = () => {
      // A KEYBOARD IS NOT A SMALLER DEVICE. Android resizes the webview under
      // its keyboard, so a list recut here would lose half its rows the moment
      // the reader typed into the filter above it and get them back on blur —
      // the same event that once closed an open menu (`anchored-menu`).
      if (isSoftKeyboardUp()) return;
      setHeight((current) => {
        const next = screenHeight();
        return Math.abs(next - current) < SETTLED_DELTA ? current : next;
      });
    };
    const settle = () => {
      window.clearTimeout(timer);
      timer = window.setTimeout(measure, SETTLE_MS);
    };
    window.addEventListener('resize', settle);
    // Every intermediate frame of a rotation is real layout at the wrong shape;
    // `viewport.ts` already owns that window, so the page is cut at its end.
    const stopRotation = onViewportRotation((phase) => {
      if (phase === 'end') settle();
    });
    return () => {
      window.clearTimeout(timer);
      window.removeEventListener('resize', settle);
      stopRotation();
    };
  }, []);
  return fitRows(height, geometry);
}

/**
 * `mouse:` as `index.css` defines it, asked at runtime.
 *
 * The variant is what makes a row 14px shorter, so a page cut for the wrong
 * density is a page that does not fit; the query is spelled here exactly as the
 * CSS spells it, and a trackpad is a fine pointer too.
 */
export const MOUSE_DENSITY = '(width >= 40rem) and (pointer: fine)';

/** True while `query` matches this screen, and it keeps up when the window changes. */
function useMediaMatch(query: string): boolean {
  const [isMatch, setMatch] = useState(
    () => (typeof window === 'undefined' ? false : (window.matchMedia?.(query).matches ?? false)),
  );
  useEffect(() => {
    const media = window.matchMedia?.(query);
    if (!media) return;
    const read = () => setMatch(media.matches);
    media.addEventListener('change', read);
    read();
    return () => media.removeEventListener('change', read);
  }, [query]);
  return isMatch;
}

/** True while the `mouse:` variant applies to this screen. */
export function useMouseDensity(): boolean {
  return useMediaMatch(MOUSE_DENSITY);
}

/**
 * THE DESK: a fine pointer AND a window wide enough to stand a second column in.
 *
 * `mouse:` asks only how the pointer moves, which is why it starts at 40rem — a
 * narrow window on a laptop still wants the dense row. A rail is a COLUMN, and a
 * column is width the list has to give up: under 64rem the fleet goes back on top
 * of the list, where a phone keeps it.
 */
export const DESK_RAIL = '(width >= 64rem) and (pointer: fine)';

/** True while there is room to stand the fleet rail beside the list. */
export function useDeskRail(): boolean {
  return useMediaMatch(DESK_RAIL);
}

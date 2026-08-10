/**
 * Where the sessions list was left, across the unmount that loses it.
 *
 * Opening a session replaces the whole list screen, so returning to it mounts a
 * brand new scroller parked at `scrollTop = 0`. Reading down a fleet of a
 * hundred rows then meant finding your place again after every single visit.
 *
 * The position is remembered TWICE, because a pixel alone lies: the session you
 * just left comes back with a fresher timestamp and jumps up the list, shifting
 * every row below it. So the mark also names the row that was under the top
 * edge; put that row back where it was and the frame looks unchanged even
 * though the order is not. The pixel is the fallback for when that row is gone
 * (deleted, or filtered away) and the seed for a list that is still hydrating.
 *
 * The mark outlives the screen in a module variable and outlives a RELOAD in
 * `sessionStorage` (see `lib/parked`): pressing reload runs no cleanup and
 * unmounts nothing, so a place kept only in memory was lost by the one gesture
 * a reader repeats all day. It still dies with the visit, which is right — a
 * cold start has no reading position to honour.
 */

import { useEffect, useLayoutEffect, useRef, type RefObject } from 'react';

import { readParked, writeParked } from './parked';

/** Minimal view of a scroll container; a real element satisfies it. */
export interface ScrollBox {
  scrollTop: number;
  readonly scrollHeight: number;
  readonly clientHeight: number;
}

/** A row, and how far its top sat below the top edge of the scroller (px). */
export interface ListAnchor {
  id: string;
  offset: number;
}

export interface ListScrollMark {
  /** The pixel offset the reader parked at. */
  top: number;
  /** The row under the top edge, when one could be identified. */
  anchor: ListAnchor | null;
}

// Under this the list is at the top for all human purposes, and restoring it is
// a no-op worth skipping entirely.
const AT_TOP_PX = 2;

const PARKED_KEY = 'vis.listScroll';

function reviveMark(raw: unknown): ListScrollMark | null {
  if (!raw || typeof raw !== 'object') return null;
  const row = raw as { top?: unknown; anchor?: unknown };
  if (typeof row.top !== 'number' || !Number.isFinite(row.top)) return null;
  const anchor = row.anchor as { id?: unknown; offset?: unknown } | null | undefined;
  const revived: ListScrollMark = {
    top: row.top,
    anchor:
      anchor && typeof anchor.id === 'string' && typeof anchor.offset === 'number'
        ? { id: anchor.id, offset: anchor.offset }
        : null,
  };
  return revived;
}

// Read on first ASK, never at import: a module that touches storage while it is
// being loaded runs before a test (or a webview) has one.
let parked: ListScrollMark | null = null;
let hydrated = false;

function hydrate(): void {
  if (hydrated) return;
  hydrated = true;
  parked = readParked(PARKED_KEY, reviveMark);
}

/** What to remember about `box`, or `null` when it is parked at the top. */
export function markListScroll(box: ScrollBox | null, anchor: ListAnchor | null): ListScrollMark | null {
  if (!box || box.scrollTop <= AT_TOP_PX) return null;
  return { top: box.scrollTop, anchor };
}

/**
 * Put `mark` back into `box`. `offsetOf` reports where a row sits now, relative
 * to the top edge of the scroller, or `null` when that row is not rendered.
 *
 * Returns whether the reading position was actually recovered. `false` means
 * the list is still short — a skeleton, or rows still arriving — so the caller
 * should keep the mark and try again on the next paint.
 */
export function applyListScroll(
  box: ScrollBox,
  mark: ListScrollMark,
  offsetOf: (id: string) => number | null,
): boolean {
  const maximum = Math.max(0, box.scrollHeight - box.clientHeight);
  if (maximum <= 0) return false;

  const current = mark.anchor ? offsetOf(mark.anchor.id) : null;
  const wanted =
    current === null || !mark.anchor ? mark.top : box.scrollTop + (current - mark.anchor.offset);
  box.scrollTop = Math.max(0, Math.min(maximum, wanted));
  // The anchored row is authoritative: once it is back under the top edge the
  // reader is home even if the list below it is still growing.
  return current !== null || wanted <= maximum;
}

export function rememberListScroll(mark: ListScrollMark | null): void {
  hydrated = true;
  parked = mark;
  writeParked(PARKED_KEY, mark);
}

export function parkedListScroll(): ListScrollMark | null {
  hydrate();
  return parked;
}

export function forgetListScroll(): void {
  rememberListScroll(null);
}

/** The first row still visible at the top edge of `viewport`. */
export function topVisibleRow(viewport: HTMLElement | null): ListAnchor | null {
  if (!viewport) return null;
  const viewportTop = viewport.getBoundingClientRect().top;
  const row = Array.from(viewport.querySelectorAll<HTMLElement>('[data-session-id]')).find(
    (element) => element.getBoundingClientRect().bottom > viewportTop,
  );
  const id = row?.dataset.sessionId;
  return id ? { id, offset: row.getBoundingClientRect().top - viewportTop } : null;
}

/** Where row `id` sits now, relative to the top edge of `viewport`. */
export function rowOffset(viewport: HTMLElement | null, id: string): number | null {
  if (!viewport) return null;
  const row = viewport.querySelector<HTMLElement>(`[data-session-id="${CSS.escape(id)}"]`);
  return row ? row.getBoundingClientRect().top - viewport.getBoundingClientRect().top : null;
}


/**
 * Keep `viewport`'s place parked for as long as this screen owns it.
 *
 * Three exits, one measurement. A screen UNMOUNT (opening a session) runs the
 * cleanup; a RELOAD or a navigation away runs no cleanup at all and only
 * `pagehide` is left; an iOS tab or app discarded in the background may never
 * fire either, so `visibilitychange` takes the mark the moment the screen stops
 * being looked at. Measuring the live scroller is the only truthful answer, so
 * all three end in the same read.
 *
 * `onReaderScrolled` fires when the reader takes over with a wheel or a finger:
 * the mark is dropped, because a restore that lands after that is fighting them.
 */
export function useListScrollPark(
  viewport: RefObject<HTMLElement | null>,
  onReaderScrolled: () => void,
): void {
  // The callback is read only from an event, so it is kept in a ref rather than
  // in the wiring effect's dependencies: re-subscribing four listeners on every
  // render of a list this long is a cost with nothing to show for it.
  const abandoned = useRef(onReaderScrolled);
  useEffect(() => {
    abandoned.current = onReaderScrolled;
  }, [onReaderScrolled]);

  useLayoutEffect(() => {
    const element = viewport.current;
    if (!element) return;
    const capture = () => {
      if (element.isConnected) rememberListScroll(markListScroll(element, topVisibleRow(element)));
    };
    const abandon = () => {
      forgetListScroll();
      abandoned.current();
    };
    const onHide = () => capture();
    const onVisibility = () => {
      if (document.visibilityState === 'hidden') capture();
    };
    element.addEventListener('wheel', abandon, { passive: true });
    element.addEventListener('touchstart', abandon, { passive: true });
    window.addEventListener('pagehide', onHide);
    document.addEventListener('visibilitychange', onVisibility);
    return () => {
      element.removeEventListener('wheel', abandon);
      element.removeEventListener('touchstart', abandon);
      window.removeEventListener('pagehide', onHide);
      document.removeEventListener('visibilitychange', onVisibility);
      // A layout cleanup still runs against the live DOM, which is the last
      // moment this scroller can be measured at all.
      capture();
    };
  }, [viewport]);
}

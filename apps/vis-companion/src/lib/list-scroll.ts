/**
 * Where the sessions list was left while another screen covers it.
 *
 * A phone keeps the list mounted behind a session, but its hidden scroll box
 * can lose its offset. Remember the last visible position before that happens.
 * An anchor names the row under the top edge so a reordered list can put it
 * back; the pixel offset is a fallback when that row has gone away.
 *
 * The mark also survives a reload in `sessionStorage` (see `lib/parked`).
 * It dies with the visit, so a cold start has no reading position to honour.
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
export function markListScroll(
  box: ScrollBox | null,
  anchor: ListAnchor | null,
): ListScrollMark | null {
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
 * Keep the list's last visible place across screen changes and reloads.
 *
 * A scroll event records the position without a storage write. When the list
 * goes behind another screen, the browser may reset its hidden scroll box, so
 * park that last visible reading position rather than measuring the hidden DOM.
 * Pagehide and backgrounding also park the live position for a reload; an
 * unmount captures it only while the list was still visible.
 *
 * `onReaderScrolled` fires when the reader takes over with a wheel or finger:
 * the mark is dropped, because a late restore would fight them.
 */
export function useListScrollPark(
  viewport: RefObject<HTMLElement | null>,
  onReaderScrolled: () => void,
  isVisible: boolean,
): void {
  // Rewiring listeners on every list render costs more than reading the latest
  // callback and visibility from refs when an event actually happens.
  const abandoned = useRef(onReaderScrolled);
  const visible = useRef(isVisible);
  const lastVisible = useRef<ListScrollMark | null>(null);
  useEffect(() => {
    abandoned.current = onReaderScrolled;
  }, [onReaderScrolled]);

  useLayoutEffect(() => {
    if (visible.current && !isVisible) rememberListScroll(lastVisible.current);
    visible.current = isVisible;
  }, [isVisible]);

  useLayoutEffect(() => {
    const element = viewport.current;
    if (!element) return;
    const capture = () => {
      if (!visible.current || !element.isConnected) return;
      lastVisible.current = markListScroll(element, topVisibleRow(element));
      rememberListScroll(lastVisible.current);
    };
    const onScroll = () => {
      if (visible.current) lastVisible.current = markListScroll(element, topVisibleRow(element));
    };
    const abandon = () => {
      if (!visible.current) return;
      forgetListScroll();
      abandoned.current();
    };
    const onHide = () => capture();
    const onVisibility = () => {
      if (document.visibilityState === 'hidden') capture();
    };
    element.addEventListener('scroll', onScroll, { passive: true });
    element.addEventListener('wheel', abandon, { passive: true });
    element.addEventListener('touchstart', abandon, { passive: true });
    window.addEventListener('pagehide', onHide);
    document.addEventListener('visibilitychange', onVisibility);
    return () => {
      element.removeEventListener('scroll', onScroll);
      element.removeEventListener('wheel', abandon);
      element.removeEventListener('touchstart', abandon);
      window.removeEventListener('pagehide', onHide);
      document.removeEventListener('visibilitychange', onVisibility);
      // A layout cleanup still runs against the live DOM when the list unmounts.
      capture();
    };
  }, [viewport]);
}

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
 * Memory only, and one list: it dies with the JavaScript context, which is
 * exactly right — a cold start has no reading position to honour.
 */

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

let parked: ListScrollMark | null = null;

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
  parked = mark;
}

export function parkedListScroll(): ListScrollMark | null {
  return parked;
}

export function forgetListScroll(): void {
  parked = null;
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

/**
 * Where a transcript was being READ, across the unmount that loses it.
 *
 * Leaving a session tears the whole screen down, so coming back mounts a fresh
 * scroller and the opening correction pins it to the newest turn. Someone forty
 * turns up — reading what the agent did an hour ago, stepping out to the list or
 * to Machines and back — lost their place every single time.
 *
 * The place is remembered as a DISTANCE FROM THE BOTTOM, not as a pixel from the
 * top: this screen hydrates its history upward (older turns keep arriving ABOVE
 * the viewport for as long as the ramp runs), so the top pixel means something
 * different on every frame while the bottom one holds still.
 *
 * A reader who was AT the bottom parks nothing: the newest turn is their place,
 * and reopening must land there.
 *
 * The place outlives the screen in a module map and outlives a RELOAD in
 * `sessionStorage` (see `lib/parked`): reload runs no cleanup and unmounts
 * nothing, so a place kept only in memory was lost by the one gesture a reader
 * repeats all day. It still dies with the visit, which is right — a cold start
 * has no reading position to honour.
 */

import { readParked, writeParked } from './parked';

/** Minimal view of a scroll container; a real element satisfies it. */
export interface ScrollBox {
  scrollTop: number;
  readonly scrollHeight: number;
  readonly clientHeight: number;
}

// The same slack `handleScroll` calls "following": within this of the end, the
// reader is at the bottom and has no position worth restoring.
const AT_BOTTOM_PX = 64;

const PARKED_KEY = 'vis.readingPositions';

function revivePlaces(raw: unknown): Map<string, number> | null {
  if (!raw || typeof raw !== 'object' || Array.isArray(raw)) return null;
  const places = new Map<string, number>();
  for (const [sid, distance] of Object.entries(raw as Record<string, unknown>)) {
    if (typeof distance === 'number' && Number.isFinite(distance)) places.set(sid, distance);
  }
  return places;
}

// Read on first ASK, never at import: a module that touches storage while it is
// being loaded runs before a test (or a webview) has one.
let parked = new Map<string, number>();
let hydrated = false;

function places(): Map<string, number> {
  if (!hydrated) {
    hydrated = true;
    parked = revivePlaces(readParked(PARKED_KEY, (raw) => raw)) ?? new Map();
  }
  return parked;
}

function persist(): void {
  writeParked(PARKED_KEY, Object.fromEntries(places()));
}

function distanceFromEnd(box: ScrollBox): number {
  return box.scrollHeight - box.scrollTop - box.clientHeight;
}

/** Whether the end of `box` is already on screen. */
export function isAtBottom(box: ScrollBox): boolean {
  return distanceFromEnd(box) <= AT_BOTTOM_PX;
}

/** How far above the end `box` sits, or `null` when it is at the bottom. */
export function markReadingPosition(box: ScrollBox | null): number | null {
  if (!box) return null;
  const distance = distanceFromEnd(box);
  return distance <= AT_BOTTOM_PX ? null : distance;
}

export function rememberReadingPosition(sid: string, distance: number | null): void {
  if (distance === null) places().delete(sid);
  else places().set(sid, distance);
  persist();
}

export function parkedReadingPosition(sid: string): number | null {
  return places().get(sid) ?? null;
}

export function forgetReadingPosition(sid: string): void {
  places().delete(sid);
  persist();
}

/**
 * Put `distance` back into `box`.
 *
 * Returns whether the position was actually recovered. `false` means the
 * transcript is still shorter than the place we are aiming for — rows are still
 * hydrating — so the caller should keep the mark and try again on the next paint.
 */
export function applyReadingPosition(box: ScrollBox, distance: number): boolean {
  const maximum = Math.max(0, box.scrollHeight - box.clientHeight);
  if (maximum <= 0) return false;
  box.scrollTop = Math.max(0, maximum - distance);
  return maximum >= distance;
}

/**
 * Put the END of `box` back under the reader.
 *
 * The answer for someone following the newest turn is ABSOLUTE — the bottom —
 * so however much height just arrived, and however many callers ask in the same
 * frame, the growth is billed exactly once and a scroller already there does not
 * move. Returns whether it had to move at all.
 */
export function followEnd(box: ScrollBox): boolean {
  const bottom = Math.max(0, box.scrollHeight - box.clientHeight);
  if (Math.abs(box.scrollTop - bottom) < 1) return false;
  box.scrollTop = bottom;
  return true;
}

/**
 * Is this scroll event the ECHO of a correction this screen just made?
 *
 * Nothing the corrector does is a reader gesture, and the pixel it left behind is
 * the only honest witness: a scroller sitting exactly where the last correction
 * put it has not been touched, however much the transcript grew underneath it in
 * the meantime. Re-measuring "am I following the end" there answers a question
 * about the GROWTH, not about the reader — while a session opens, history lands
 * every frame, so a scroller pinned to the end one frame ago now measures far
 * from it, and a screen that believes that measurement stops chasing and strands
 * the reader above the newest turn.
 */
export function isCorrectionEcho(box: ScrollBox, correctedTop: number): boolean {
  if (correctedTop < 0) return false;
  return Math.abs(box.scrollTop - correctedTop) < 1;
}

/**
 * Whether the "↓ Latest" offer has anything to offer.
 *
 * Two facts, and either one alone withdraws it: the transcript is already
 * chasing the end (`following`), or the end is already on screen. A screen that
 * remembers instead of measuring eventually offers to take the reader where
 * they are standing — a rotation, a keyboard, a queue tray or a closing fold
 * can all put the end back under their eyes without a scroll event the screen
 * is listening to at that moment.
 */
export function shouldOfferLatest(
  box: ScrollBox | null,
  following: boolean,
): boolean {
  if (!box || following) return false;
  return !isAtBottom(box);
}

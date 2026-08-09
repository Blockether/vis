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
 * Memory only, keyed by session id: it dies with the JavaScript context, which
 * is exactly right — a cold start has no reading position to honour.
 */

/** Minimal view of a scroll container; a real element satisfies it. */
export interface ScrollBox {
  scrollTop: number;
  readonly scrollHeight: number;
  readonly clientHeight: number;
}

// The same slack `handleScroll` calls "following": within this of the end, the
// reader is at the bottom and has no position worth restoring.
const AT_BOTTOM_PX = 64;

const parked = new Map<string, number>();

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
  if (distance === null) parked.delete(sid);
  else parked.set(sid, distance);
}

export function parkedReadingPosition(sid: string): number | null {
  return parked.get(sid) ?? null;
}

export function forgetReadingPosition(sid: string): void {
  parked.delete(sid);
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

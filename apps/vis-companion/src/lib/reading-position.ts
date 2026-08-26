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

/**
 * Whether one content resize is small enough to keep following without replacing
 * the page under the reader.
 *
 * Line-sized stream flushes should reveal themselves. A card-sized tool result is
 * different: moving more than a quarter of the viewport in one frame makes the
 * transcript look as though it jumped, so the caller should hold the current line
 * and offer Latest instead. The bottom tolerance is the floor so compact viewports
 * still admit an ordinary line. A zero height is only an unmeasured first sample.
 */
export function growthFitsFollowWindow(
  box: ScrollBox,
  previousHeight: number,
): boolean {
  if (previousHeight <= 0) return true;
  const growth = box.scrollHeight - previousHeight;
  return growth <= Math.max(AT_BOTTOM_PX, box.clientHeight / 4);
}

/**
 * How long a reader who was FOLLOWING, and whose page was held still for one large
 * live batch, is left above it before the end is carried to them.
 *
 * A batch that replaces the visible page is held (`growthFitsFollowWindow`) — but a
 * reader parked at the end asked for the newest, and on a phone a quarter of the
 * viewport is ~180 px, so every result card surrendered the follow and left them
 * tapping "Latest" for the rest of the turn. The hand is the only signal that says
 * otherwise: a reader who answers the batch by scrolling keeps the line they chose,
 * and only a reader who touched nothing is carried on. Long enough to reach for the
 * glass, short enough that "I should be at the bottom" is true again.
 */
export const FOLLOW_RESUME_QUIET_MS = 2_000;

/**
 * Whether a smaller `scrollTop` is the reader RETREATING, or the scroller being
 * CLAMPED by content that shrank underneath it.
 *
 * A collapsing Activity card, a keyboard, a live bubble replaced by its shorter
 * persisted row: each removes height, and the browser answers by pulling `scrollTop`
 * down to the new end — no gesture involved. Measured against the previous top alone
 * that reads as an upward gesture, which is what dropped follow on a tap.
 */
export function readerRetreatedFrom(
  box: ScrollBox,
  previousTop: number,
  previousHeight: number,
): boolean {
  if (box.scrollTop >= previousTop) return false;
  const shrink = Math.max(0, previousHeight - box.scrollHeight);
  return previousTop - box.scrollTop > shrink;
}

/**
 * Whether the reader ARRIVED at the end — with `aimed` as the end they were
 * reaching for, not the one the transcript has now.
 *
 * A live turn grows under the finger: a flush every 150 ms, hundreds of pixels
 * at a time. Judged against the end as it stands, a reader dragging down is
 * always short of an end that keeps moving, so the follow never re-engages and
 * the newest turn is never carried to them again. Measured on the simulator
 * against a streaming session: six hard drags ended 512 px above the end, the
 * transcript having grown 174 px underneath them, and "↓ Latest" stayed offered
 * while the gap only widened. Growth the reader could not have seen is not
 * distance they chose to keep.
 *
 * A transcript SHORTER than the aim — a turn collapsed, history dropped — is
 * honoured as it stands: `aimed` is a memory, `scrollHeight` is the fact.
 */
export function arrivedAtEnd(box: ScrollBox, aimed: number): boolean {
  const end = aimed > 0 ? Math.min(aimed, box.scrollHeight) : box.scrollHeight;
  return box.scrollTop + box.clientHeight >= end - AT_BOTTOM_PX;
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
 * How many frames a transcript's height must REPEAT before the opening veil
 * believes it. Two frames is a coincidence on a device that drops one; three
 * consecutive identical measurements is the cheapest reading that survives a
 * chunk of markdown landing late.
 */
export const OPENING_QUIET_FRAMES = 3;

/**
 * Watch an opening transcript until it stops GROWING.
 *
 * A mounted turn is not a painted one: deferred markdown, syntax highlighting
 * and `content-visibility` each land their pixels a frame or two after React
 * commits, and the corrector's re-pin is always one frame behind that growth.
 * Measured on a 50 000 px session, the last turn mounted ~270ms before the
 * scroller stopped growing, and each of those ten growth frames showed a slice
 * up to 3 300 px above the newest turn — the flicker a reader sees on open.
 * Counting hydrated TURNS cannot see any of that; the scroller's own height
 * can, so the watcher takes one height per frame and answers true once the same
 * height has come back `quietFrames` times in a row.
 */
export function heightSettler(
  quietFrames: number = OPENING_QUIET_FRAMES,
): (height: number) => boolean {
  let previous = -1;
  let quiet = 0;
  return (height: number) => {
    quiet = height === previous ? quiet + 1 : 0;
    previous = height;
    return quiet >= quietFrames;
  };
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

/**
 * Geometry and follow-state decisions for a mounted transcript.
 *
 * Every session visit starts at its newest turn. These helpers govern manual
 * movement and live growth only while that screen remains mounted.
 */

/** Minimal view of a scroll container; a real element satisfies it. */
export interface ScrollBox {
  scrollTop: number;
  readonly scrollHeight: number;
  readonly clientHeight: number;
}

// The same slack `handleScroll` calls “following”: within this, the end is on screen.
const AT_BOTTOM_PX = 64;

function distanceFromEnd(box: ScrollBox): number {
  return box.scrollHeight - box.scrollTop - box.clientHeight;
}

/** Whether the end of `box` is already on screen. */
export function isAtBottom(box: ScrollBox): boolean {
  return distanceFromEnd(box) <= AT_BOTTOM_PX;
}

/** The largest `scrollTop` this box can hold: where its end is. */
export function bottomOf(box: ScrollBox): number {
  return Math.max(0, box.scrollHeight - box.clientHeight);
}


/**
 * Whether a smaller `scrollTop` is the reader RETREATING, or the scroller being
 * CLAMPED because its own end moved up underneath them.
 *
 * `previousBottom` is where that end WAS: the largest `scrollTop` the box could
 * hold at the last measurement. Two ordinary events lower it and neither is a
 * gesture — content LEAVES (a collapsing Activity card, a running-turn bubble replaced
 * by its shorter persisted row), or the viewport GROWS, which is exactly what
 * the iOS keyboard does on the way down: 274 px of a 568 px screen handed back
 * to the scroller, whose maximum offset drops by that much and takes the reader
 * with it. Measured against the previous top alone, both read as an upward
 * gesture, which is what dropped follow on a tap — and a reader whose follow
 * died that way was offered "↓ Latest" the next time they touched the composer.
 */
export function readerRetreatedFrom(
  box: ScrollBox,
  previousTop: number,
  previousBottom: number,
): boolean {
  if (box.scrollTop >= previousTop) return false;
  const clamp = Math.max(0, previousBottom - bottomOf(box));
  return previousTop - box.scrollTop > clamp;
}

/**
 * Whether the reader ARRIVED at the end — with `aimed` as the end they were
 * reaching for, not the one the transcript has now.
 *
 * A running turn grows under the finger: a flush every 150 ms, hundreds of pixels
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

/**
 * Put the END of `box` back under the reader.
 *
 * The answer for someone following the newest turn is ABSOLUTE — the bottom —
 * so however much height just arrived, and however many callers ask in the same
 * frame, the growth is billed exactly once and a scroller already there does not
 * move. Returns whether it had to move at all.
 */
export function followEnd(box: ScrollBox): boolean {
  const bottom = bottomOf(box);
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

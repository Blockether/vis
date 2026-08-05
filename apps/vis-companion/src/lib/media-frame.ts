/**
 * The box a transcript picture occupies — decided BEFORE anyone knows what is
 * in it.
 *
 * An attachment descriptor carries no pixel dimensions, so a tile cannot size
 * itself from its picture without first fetching it. It used to try anyway: a
 * 96 px pulse while the object URL was in flight, then whatever the decoded
 * image turned out to be (up to 60svh — about 500 px on a phone). Every one of
 * those swaps that lands ABOVE the reader moves the line they are on, and this
 * scroller cannot absorb it: WebKit has no scroll anchoring, the transcript
 * runs `overflow-anchor:none` on purpose, and `SessionScreen`'s own corrector
 * deliberately stands down while a hand is on the glass — writing `scrollTop`
 * mid-gesture fights iOS momentum. So a session full of screenshots jumped
 * exactly while it was being scrolled.
 *
 * The cure is to never grow. One reserved box, derived from the COLUMN width
 * and the viewport alone, shared by the placeholder, the picture, the clip and
 * the failure notice, with the media contained inside it. Nothing about the
 * bytes can change the layout, so there is nothing left to compensate for —
 * mid-gesture or otherwise. Full resolution is one tap away in the viewer.
 */

/** The three things one media slot can be showing. All reserve the same box. */
export type MediaSlotState = 'pending' | 'ready' | 'failed';

/**
 * The reserved box itself: an aspect ratio off the column width, capped so a
 * wide desktop bubble cannot hand a single picture the whole screen. Both terms
 * are known at layout time, which is the whole point — no term comes from the
 * image.
 */
export const mediaFrameClass = 'block w-full aspect-[4/3] max-h-[60svh]';

/** The pulse a slot paints while its bytes are in flight, filling the frame. */
export const mediaPendingClass = 'block h-full w-full animate-pulse bg-thinking-surface';

/**
 * A picture or clip filling the reserved frame. `object-contain` keeps every
 * pixel of a tall phone screenshot visible, `object-left` keeps it on the text
 * column's own edge instead of floating in the middle of the letterbox.
 */
export const mediaContentClass = 'block h-full w-full object-contain object-left';

/**
 * The frame ONE media slot reserves in a given state.
 *
 * The answer is the same string every time, on purpose: that identity IS the
 * fix, and the test that pins it is the regression gate.
 */
export function mediaSlotFrame(_state: MediaSlotState): string {
  return mediaFrameClass;
}

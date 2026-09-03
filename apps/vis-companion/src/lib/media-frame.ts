/**
 * Reserve attachment geometry from column and viewport dimensions before bytes arrive.
 * Placeholder, media and failure states share it, preventing asynchronous scroll shifts.
 */

/** Fixed aspect ratio capped independently of the media's decoded dimensions. */
export const mediaFrameClass =
  "block w-full aspect-[4/3] max-h-[60svh] overflow-hidden border border-code-edge bg-code";

/** The pulse a slot paints while its bytes are in flight, filling the frame. */
export const mediaPendingClass =
  "block h-full w-full animate-pulse bg-thinking-surface";

/**
 * A picture or clip inside the reserved frame. `object-contain` keeps every
 * pixel of a tall phone screenshot visible; the leftover paper is the frame's
 * own MAT, so the picture is CENTRED in it. Shoved to `object-left` it looked
 * like a small image that had failed to fill a broken box, and the caption
 * beneath the empty half read as a label sitting ON the picture.
 */
export const mediaContentClass =
  "block h-full w-full object-contain object-center";

/**
 * The plate's label, DOCKED under the mat and sharing its frame: one strip of
 * paper carrying the file's name, so nothing about the name can be mistaken for
 * part of the picture.
 */
export const mediaCaptionClass =
  "flex min-w-0 items-center gap-2 border border-t-0 border-code-edge bg-thinking-surface px-2 py-1 font-mono text-chip text-footer-muted";

/**
 * ONE picture is a PLATE; several are a GALLERY.
 *
 * A rail that gave every picture the full 4/3 plate turned four dropped
 * screenshots into four 60svh boxes stacked down the column — a wall to scroll
 * past rather than something to look at. A lone picture still gets its plate
 * and its caption, because that is the whole content of the message; from the
 * second one the rail becomes a grid of square tiles and the names move into
 * the viewer, where there is room for them.
 */
export type MediaLayout = "plate" | "grid";

/** The layout a rail of `count` pictures takes. The rule is the same on BOTH
 *  rails: what the human sent and what the model produced read alike. */
export function mediaGroupLayout(count: number): MediaLayout {
  return count > 1 ? "grid" : "plate";
}

/**
 * The gallery itself: two columns on a phone, more when there is room.
 *
 * Columns are the only thing width decides. A 390px phone gives ~183px tiles
 * and the widest desktop still gives ~160px, so no tile ever approaches a hit
 * box worth policing — and no `sm:` utility here shrinks one, because adding a
 * column is not the same as pinning a smaller box.
 */
export const mediaGridClass =
  "grid grid-cols-2 gap-2 sm:grid-cols-3 mouse:grid-cols-4";

/** One gallery cell: square, reserved before its bytes land, same paper and
 *  same edge as the plate — a tile is the plate at gallery size, not a
 *  different control. */
export const mediaTileFrameClass =
  "block w-full aspect-square overflow-hidden border border-code-edge bg-code";

/**
 * A picture inside a tile FILLS it. The plate mats a tall screenshot because it
 * is the message; a contact sheet is read by what each frame is OF, and a grid
 * of letterboxed slivers separated by their own empty paper answers that worse
 * than a crop does. Full pixels stay one tap away in the viewer.
 */
export const mediaTileContentClass =
  "block h-full w-full object-cover object-center";

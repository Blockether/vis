/**
 * Where an anchored popover sits — and whether it can stay open at all.
 *
 * The sessions list hangs its "start in" menu under a machine header's `⋯` — or under
 * the New session button beside it, for a machine with no project to start in yet.
 * Both answers live here because it is the SAME question asked
 * twice: once when the menu opens, and again every time the viewport moves under
 * an open one.
 */

/** The part of a `DOMRect` an anchored menu is placed from. */
export type AnchorBox = { bottom: number; right: number };

/** Viewport-space position of the menu, in CSS pixels. */
export type MenuPosition = { top: number; left: number };

/** Air between the anchor and the menu it drops. */
const ANCHOR_GAP = 6;

/** Closest the menu ever comes to the left edge of the screen. */
const EDGE_MARGIN = 12;

/**
 * A menu pinned under `anchor`, right-aligned to it, never crowding the left edge.
 *
 * `null` — close the menu — means only ONE thing: there is no anchor to hang from
 * any more. A live anchor always yields a position, including across a resize: a
 * phone fires `resize` for its own reasons (the on-screen keyboard alone fires one
 * every time it hides), and a menu that reads that as "close" is a control that
 * does nothing.
 */
export function menuPosition(
  anchor: AnchorBox | null | undefined,
  width: number,
): MenuPosition | null {
  if (!anchor) return null;
  return {
    top: Math.round(anchor.bottom + ANCHOR_GAP),
    left: Math.round(Math.max(EDGE_MARGIN, anchor.right - width)),
  };
}

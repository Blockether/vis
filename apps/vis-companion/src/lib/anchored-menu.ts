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
export type AnchorBox = { top: number; bottom: number; right: number };

/** Viewport-space position of the menu, in CSS pixels. */
export type MenuPosition = { top: number; left: number };

/** Air between the anchor and the menu it drops. */
const ANCHOR_GAP = 6;

/** Closest the menu ever comes to any edge of the screen. */
const EDGE_MARGIN = 12;

/**
 * How tall an anchored panel is allowed to be, as a fraction of the viewport.
 * It must stay equal to the `sm:max-h-[70vh]` the panels paint, because the
 * placement below decides whether the panel FITS before it has ever been
 * measured — a popover is positioned on the frame it mounts.
 */
const MAX_HEIGHT_FRACTION = 0.7;

/** The viewport a menu is being placed inside. Injectable, so the maths is testable. */
export type Viewport = { width: number; height: number };

function currentViewport(): Viewport {
  return {
    width: typeof window === 'undefined' ? 0 : window.innerWidth,
    height: typeof window === 'undefined' ? 0 : window.innerHeight,
  };
}

/**
 * A menu pinned to `anchor`, right-aligned to it, never crowding an edge of the
 * screen — including the BOTTOM one.
 *
 * The bottom edge is not symmetric with the others. A menu that runs off the left
 * is merely ugly; a menu that runs off the bottom takes its footer with it, and
 * this app puts the committing button in that footer. Opening `Manage projects`
 * from a project header two thirds of the way down a 900px desktop window placed
 * the panel at y=300 with a 630px height budget: `Use project` rendered 30px below
 * the window and could not be reached by scrolling, because the page behind it does
 * not scroll and the panel's own scroller is INSIDE the clipped box. The control the
 * whole sheet exists to reach was simply not on the screen.
 *
 * So a panel that does not fit below its anchor FLIPS above it, and one that fits in
 * neither direction is clamped to the taller side. The panel keeps the same
 * `70vh` budget either way, so what moves is only where it starts.
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
  viewport: Viewport = currentViewport(),
): MenuPosition | null {
  if (!anchor) return null;
  const left = Math.round(Math.max(EDGE_MARGIN, anchor.right - width));
  // A zero-height viewport is a non-browser render (jsdom, SSR): fall back to the
  // plain drop, because clamping against nothing would pin every menu to the top.
  if (viewport.height <= 0) return { top: Math.round(anchor.bottom + ANCHOR_GAP), left };

  const budget = viewport.height * MAX_HEIGHT_FRACTION;
  const below = viewport.height - EDGE_MARGIN - (anchor.bottom + ANCHOR_GAP);
  const above = anchor.top - ANCHOR_GAP - EDGE_MARGIN;

  // Below is the natural reading direction and wins whenever the panel fits there.
  if (below >= budget) return { top: Math.round(anchor.bottom + ANCHOR_GAP), left };
  // Flipping is only worth the disorientation if it actually buys the whole panel.
  if (above >= budget) return { top: Math.round(anchor.top - ANCHOR_GAP - budget), left };
  // Neither side fits: take the taller one and sit flush against its margin, so the
  // panel is short but WHOLE rather than tall and beheaded.
  if (above > below) return { top: EDGE_MARGIN, left };
  return {
    top: Math.round(Math.max(EDGE_MARGIN, viewport.height - EDGE_MARGIN - budget)),
    left,
  };
}

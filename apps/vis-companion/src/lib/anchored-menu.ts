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

/**
 * Where the menu sits in the viewport, in CSS pixels: `left`, plus the ONE vertical
 * edge that is pinned. A panel dropping below its anchor pins its head (`top`); a
 * panel standing above one pins its foot (`bottom`, measured up from the foot of the
 * viewport), because the foot is the end that has to touch the control — and how far
 * the other end reaches is not known until the panel has been painted.
 */
export type MenuPosition = { left: number; top?: number; bottom?: number };

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
 * So a panel that does not fit below its anchor FLIPS above it and STANDS on it,
 * foot pinned just clear of the anchor's head; one that fits in neither direction is
 * clamped to the taller side. A measured panel uses its real height, capped at
 * `70vh`; an unmeasured panel reserves that maximum — and that reserve chooses the
 * DIRECTION only. Pinning a flipped panel's HEAD to it instead put a three-row menu
 * hundreds of pixels above the glyph that dropped it, beside another project's name
 * (reported: pressing one project's menu opened the menu somewhere else).
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
  panelHeight?: number,
): MenuPosition | null {
  if (!anchor) return null;
  const left = Math.round(Math.max(EDGE_MARGIN, anchor.right - width));
  // A zero-height viewport is a non-browser render (jsdom, SSR): fall back to the
  // plain drop, because clamping against nothing would pin every menu to the top.
  if (viewport.height <= 0) return { top: Math.round(anchor.bottom + ANCHOR_GAP), left };

  const budget = Math.min(panelHeight ?? Infinity, viewport.height * MAX_HEIGHT_FRACTION);
  const below = viewport.height - EDGE_MARGIN - (anchor.bottom + ANCHOR_GAP);
  const above = anchor.top - ANCHOR_GAP - EDGE_MARGIN;

  // Below is the natural reading direction and wins whenever the panel fits there.
  if (below >= budget) return { top: Math.round(anchor.bottom + ANCHOR_GAP), left };
  // Flipping is only worth the disorientation if it actually buys the whole panel,
  // and a flipped panel STANDS on its anchor: the reserve above chose the side, the
  // anchor itself sets the foot, so a short menu still touches the control it left.
  if (above >= budget)
    return { bottom: Math.round(viewport.height - anchor.top + ANCHOR_GAP), left };
  // Neither side fits: take the taller one and sit flush against its margin, so the
  // panel is short but WHOLE rather than tall and beheaded.
  if (above > below) return { top: EDGE_MARGIN, left };
  return {
    top: Math.round(Math.max(EDGE_MARGIN, viewport.height - EDGE_MARGIN - budget)),
    left,
  };
}

/**
 * The box a CURSOR stands in, for a menu asked for by right-clicking a row rather
 * than by pressing its `⋯`.
 *
 * `menuPosition` right-aligns a panel to its anchor, which is what a trigger wants:
 * the menu hangs back under the button that dropped it. A pointer wants the opposite
 * — every desktop menu opens away from the click, down and to the right — so the
 * point is handed over as the anchor's RIGHT edge plus one panel width. Close to the
 * right edge of the window that would push the panel off the screen, and there the
 * cursor becomes the right edge instead and the menu opens leftward from it.
 */
export function pointerAnchor(
  point: { x: number; y: number },
  width: number,
  viewport: Viewport = currentViewport(),
): AnchorBox {
  const rightward = point.x + width;
  const fits = viewport.width <= 0 || rightward <= viewport.width - EDGE_MARGIN;
  return { top: point.y, bottom: point.y, right: fits ? rightward : point.x };
}

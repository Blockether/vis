import { describe, expect, it } from 'vitest';

import { menuPosition, pointerAnchor } from './anchored-menu';

/** A desktop window with room to drop a 70vh panel under a header. */
const DESKTOP = { width: 1440, height: 900 };
/** No viewport at all: jsdom and SSR, where clamping has nothing to clamp against. */
const HEADLESS = { width: 0, height: 0 };

describe('menuPosition', () => {
  it('hangs the menu under the anchor and right-aligns it', () => {
    expect(menuPosition({ top: 60, bottom: 88, right: 1268 }, 320, DESKTOP)).toEqual({
      top: 94,
      left: 948,
    });
  });

  it('places a measured short dropdown next to its trigger, including near the bottom', () => {
    const middle = { top: 400, bottom: 428, right: 600 };
    expect(menuPosition(middle, 320, DESKTOP, 150)).toEqual({ top: 434, left: 280 });
    const bottom = { top: 800, bottom: 828, right: 600 };
    // Flipped, it is pinned by its FOOT: the same geometry read from the other edge,
    // 900 - 106 = 794, six pixels over the trigger.
    expect(menuPosition(bottom, 320, DESKTOP, 150)).toEqual({ bottom: 106, left: 280 });
  });

  it('keeps a menu wider than its anchor allows away from the left edge', () => {
    expect(menuPosition({ top: 60, bottom: 88, right: 200 }, 320, DESKTOP)).toEqual({
      top: 94,
      left: 12,
    });
  });

  it('closes a menu whose anchor has left the document', () => {
    expect(menuPosition(null, 320, DESKTOP)).toBeNull();
    expect(menuPosition(undefined, 320, DESKTOP)).toBeNull();
  });

  // Regression (reported: "New session ▾ does nothing in the app"): the
  // sessions screen closed this menu on EVERY `window.resize`. On a phone the
  // caret is one tap away from the filter field, and the keyboard hiding fires a
  // resize inside the same tap that opened the menu — so the menu died on the
  // frame it was born and the dropdown looked dead. A viewport change re-anchors;
  // only a missing anchor closes.
  it('follows its anchor across a viewport change instead of closing', () => {
    const keyboardUp = menuPosition({ top: 60, bottom: 88, right: 378 }, 320, DESKTOP);
    const keyboardHidden = menuPosition({ top: 104, bottom: 132, right: 378 }, 320, DESKTOP);

    expect(keyboardUp).toEqual({ top: 94, left: 58 });
    expect(keyboardHidden).toEqual({ top: 138, left: 58 });
  });

  // Regression, user report (paraphrased: the dropdowns in Settings are not positioned
  // correctly on the phone). A machine row's `Address` verb lives in a swipe drawer,
  // and the drawer snaps home in the same tap that opens the menu — so the cell that
  // was pressed hands over a box 216px PAST the right edge of a 393px screen. The
  // placement right-aligned to it faithfully: a 320px panel at left=289, a quarter of
  // it on the screen and every address on it off the side.
  describe('the right edge', () => {
    /** A phone in portrait, wide enough for the panel's full 320px. */
    const PHONE = { width: 393, height: 852 };
    /** The painted width of a menu panel, which is what `w-[min(20rem,…)]` clamps. */
    const paintedOn = (viewport: { width: number }, width: number) =>
      Math.min(width, viewport.width - 24);

    it('keeps a panel on the screen when the anchor it came from is not', () => {
      const snappedHomeCell = { top: 300, bottom: 328, right: 609 };
      const at = menuPosition(snappedHomeCell, 320, PHONE)!;

      expect(at).toEqual({ top: 334, left: 61, maxHeight: 506 });
      // Right-aligned to the screen's own margin, because the anchor has none to give.
      expect(at.left + paintedOn(PHONE, 320)).toBe(PHONE.width - 12);
    });

    it('places from the width the panel paints, not the width it asked for', () => {
      // A small phone: the panel paints `100vw - 24px` = 296px, so 296 is what it
      // right-aligns from. Placing a 320px reserve would hang it 24px off the edge.
      const small = { width: 320, height: 568 };
      const at = menuPosition({ top: 100, bottom: 128, right: 312 }, 320, small)!;

      expect(at).toEqual({ top: 134, left: 12 });
      expect(at.left + paintedOn(small, 320)).toBe(small.width - 12);
    });

    it('never crosses either side margin, wherever the anchor says it is', () => {
      for (const right of [-40, 0, 120, 380, 393, 609, 1200])
        for (const width of [320, 384]) {
          const at = menuPosition({ top: 300, bottom: 328, right }, width, PHONE)!;
          expect(at.left).toBeGreaterThanOrEqual(12);
          expect(at.left + paintedOn(PHONE, width)).toBeLessThanOrEqual(PHONE.width - 12);
        }
    });
  });

  // Regression (reported: "the manage projects looks absolutely awful
  // on the desktop"). `Manage projects` opened from a project header at
  // y=300 of a 900px window was placed at top=300 with a 630px (70vh) budget: its
  // footer — which carries `Use project`, the only control that commits the whole
  // sheet — rendered 30px BELOW the window. Nothing could scroll it back: the page
  // behind does not scroll, and the panel's own scroller is inside the clipped box.
  describe('the bottom edge', () => {
    it('hangs a panel that cannot fit below from the anchor it came from', () => {
      // A header near the foot of the window: 54px of room below, 782px above. The
      // panel's FOOT is pinned over the anchor, so it hugs the control it came from
      // whatever height it turns out to have.
      expect(menuPosition({ top: 800, bottom: 828, right: 1400 }, 384, DESKTOP)).toEqual({
        bottom: 900 - 800 + 6,
        left: 1016,
      });
    });

    // Regression, user report (paraphrased: pressing a project's menu glyph opened
    // the menu somewhere else entirely): a header at the foot of a tall window
    // reserved 70vh above the anchor and pinned the panel's HEAD there. The sheet
    // was three rows tall, so it hung hundreds of pixels over the button that
    // opened it, beside a different project's name.
    it('leaves no gap between a flipped panel and its anchor', () => {
      const tall = { width: 1000, height: 1168 };
      const header = { top: 985, bottom: 1013, right: 965 };
      const at = menuPosition(header, 320, tall);

      expect(at).toEqual({ bottom: tall.height - 985 + 6, left: 645 });
      // The foot sits where it sits whatever the panel measures; only a panel short
      // enough to fit below is placed below instead.
      for (const height of [285, 700])
        expect(menuPosition(header, 320, tall, height)).toEqual(at);
      expect(menuPosition(header, 320, tall, 120)).toEqual({ top: 1019, left: 645 });
    });

    it('hangs below the anchor, capped, when neither side has the full budget', () => {
      // Mid-window: 254px above, 582px below. Below is roomier, so the panel's head
      // stays under the anchor it came from and its HEIGHT is what gives way — 582px,
      // which stands its foot exactly on the bottom margin.
      expect(menuPosition({ top: 272, bottom: 300, right: 1400 }, 384, DESKTOP)).toEqual({
        top: 306,
        left: 1016,
        maxHeight: 582,
      });
    });

    it('never places a panel whose head or foot would leave the window', () => {
      // 70vh is what an unmeasured panel reserves, so that is the band every
      // placement has to keep inside the window, head and foot alike — unless the
      // placement capped the panel, in which case the cap is the band.
      const reserved = DESKTOP.height * 0.7;
      for (const bottom of [100, 300, 500, 700, 880]) {
        const at = menuPosition({ top: bottom - 28, bottom, right: 1400 }, 384, DESKTOP)!;
        const height = at.maxHeight ?? reserved;
        const head = at.top ?? DESKTOP.height - at.bottom! - height;
        expect(head).toBeGreaterThanOrEqual(12);
        expect(head + height).toBeLessThanOrEqual(DESKTOP.height - 12);
      }
    });

    it('stands on its anchor and gives up height when neither side can hold it', () => {
      // A short window: 70vh is 280px and neither 158px above nor 92px below fits.
      // Above is the roomier side, so the panel stands on the anchor and takes the
      // 152px there is room for, head on the top margin.
      const short = { width: 1440, height: 400 };
      const at = menuPosition({ top: 170, bottom: 280, right: 1400 }, 384, short);
      expect(at).toEqual({ bottom: 400 - 170 + 6, left: 1016, maxHeight: 152 });
    });

    // Regression, user report (paraphrased: the menu is STILL misplaced). A project
    // band two thirds of the way down a tall window has room for the 70vh reserve on
    // neither side, and the squeezed placement used to drop the anchor and clamp the
    // panel to the top margin: a three-row menu opened in the CORNER of the window,
    // over the app's own header, while the glyph that dropped it sat 470px below.
    it('keeps a squeezed menu on the glyph that opened it', () => {
      const pane = { width: 1400, height: 700 };
      const glyph = { top: 484, bottom: 512, right: 486 };
      const at = menuPosition(glyph, 320, pane)!;

      expect(at).toEqual({ bottom: 700 - 484 + 6, left: 166, maxHeight: 466 });
      // Foot six pixels over the glyph, head on the top margin: the whole menu is on
      // the screen AND touching the control it came from.
      expect(pane.height - at.bottom!).toBe(glyph.top - 6);
      expect(pane.height - at.bottom! - at.maxHeight!).toBe(12);
    });

    it('falls back to the plain drop where there is no viewport to measure', () => {
      expect(menuPosition({ top: 60, bottom: 88, right: 1268 }, 320, HEADLESS)).toEqual({
        top: 94,
        left: 948,
      });
    });
  });
});

// A right-click on a session row drops that row's `⋯` menu at the cursor (BLO-169),
// so the point itself is the anchor the placement above is asked about.
describe('pointerAnchor', () => {
  it('opens the menu down and to the right of the cursor', () => {
    const anchor = pointerAnchor({ x: 400, y: 300 }, 320, DESKTOP);
    expect(anchor).toEqual({ top: 300, bottom: 300, right: 720 });
    // A measured panel: the menu starts at the cursor and drops away from it.
    expect(menuPosition(anchor, 320, DESKTOP, 150)).toEqual({ top: 306, left: 400 });
  });

  it('opens leftward from a cursor that has no room on its right', () => {
    const anchor = pointerAnchor({ x: 1300, y: 300 }, 320, DESKTOP);
    expect(anchor).toEqual({ top: 300, bottom: 300, right: 1300 });
    expect(menuPosition(anchor, 320, DESKTOP, 150)).toEqual({ top: 306, left: 980 });
  });

  it('keeps the natural direction where there is no viewport to measure', () => {
    expect(pointerAnchor({ x: 400, y: 300 }, 320, HEADLESS)).toEqual({
      top: 300,
      bottom: 300,
      right: 720,
    });
  });
});

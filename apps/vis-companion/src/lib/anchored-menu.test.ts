import { describe, expect, it } from 'vitest';

import { menuPosition } from './anchored-menu';

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

    expect(keyboardUp).not.toBeNull();
    expect(keyboardHidden).toEqual({ top: 138, left: 58 });
  });

  // Regression (reported: "the manage projects looks absolutely awful
  // on the desktop"). `Manage projects` opened from a project header at
  // y=300 of a 900px window was placed at top=300 with a 630px (70vh) budget: its
  // footer — which carries `Use project`, the only control that commits the whole
  // sheet — rendered 30px BELOW the window. Nothing could scroll it back: the page
  // behind does not scroll, and the panel's own scroller is inside the clipped box.
  describe('the bottom edge', () => {
    it('flips a panel above its anchor when it cannot fit below', () => {
      // A header near the foot of the window: 54px of room below, 782px above.
      const at = menuPosition({ top: 800, bottom: 828, right: 1400 }, 384, DESKTOP);
      expect(at).toEqual({ top: 800 - 6 - 630, left: 1016 });
      expect(at!.top).toBeGreaterThanOrEqual(12);
    });

    it('sits below the anchor and clamps when neither side has the full budget', () => {
      // Mid-window: 254px above, 582px below. Below is roomier, so it stays below
      // and its FOOT — not its head — is what gets pinned inside the margin.
      expect(menuPosition({ top: 272, bottom: 300, right: 1400 }, 384, DESKTOP)).toEqual({
        top: 258,
        left: 1016,
      });
    });

    it('never places a panel whose foot would leave the window', () => {
      for (const bottom of [100, 300, 500, 700, 880]) {
        const at = menuPosition({ top: bottom - 28, bottom, right: 1400 }, 384, DESKTOP);
        expect(at!.top).toBeGreaterThanOrEqual(12);
        expect(at!.top + DESKTOP.height * 0.7).toBeLessThanOrEqual(DESKTOP.height - 12);
      }
    });

    it('clamps rather than flips when neither side can hold the whole panel', () => {
      // A short window: 70vh is 280px and neither 158px above nor 92px below fits.
      const short = { width: 1440, height: 400 };
      const at = menuPosition({ top: 170, bottom: 280, right: 1400 }, 384, short);
      expect(at).toEqual({ top: 12, left: 1016 });
    });

    it('falls back to the plain drop where there is no viewport to measure', () => {
      expect(menuPosition({ top: 60, bottom: 88, right: 1268 }, 320, HEADLESS)).toEqual({
        top: 94,
        left: 948,
      });
    });
  });
});

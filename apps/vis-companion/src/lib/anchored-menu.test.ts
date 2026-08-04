import { describe, expect, it } from 'vitest';

import { menuPosition } from './anchored-menu';

describe('menuPosition', () => {
  it('hangs the menu under the anchor and right-aligns it', () => {
    expect(menuPosition({ bottom: 88, right: 1268 }, 320)).toEqual({ top: 94, left: 948 });
  });

  it('keeps a menu wider than its anchor allows away from the left edge', () => {
    expect(menuPosition({ bottom: 88, right: 200 }, 320)).toEqual({ top: 94, left: 12 });
  });

  it('closes a menu whose anchor has left the document', () => {
    expect(menuPosition(null, 320)).toBeNull();
    expect(menuPosition(undefined, 320)).toBeNull();
  });

  // Regression (reported: "New session ▾ does fucking nothing in the app"): the
  // sessions screen closed this menu on EVERY `window.resize`. On a phone the
  // caret is one tap away from the filter field, and the keyboard hiding fires a
  // resize inside the same tap that opened the menu — so the menu died on the
  // frame it was born and the dropdown looked dead. A viewport change re-anchors;
  // only a missing anchor closes.
  it('follows its anchor across a viewport change instead of closing', () => {
    const keyboardUp = menuPosition({ bottom: 88, right: 378 }, 320);
    const keyboardHidden = menuPosition({ bottom: 132, right: 378 }, 320);

    expect(keyboardUp).not.toBeNull();
    expect(keyboardHidden).toEqual({ top: 138, left: 58 });
  });
});

/**
 * Shell geometry as pure arithmetic over one set of viewport numbers.
 *
 * It lives apart from `viewport.ts` — which owns the React state and the
 * Capacitor listeners — so the rules below can be exercised directly with
 * metrics recorded off a device, with no DOM and no plugins.
 */

/** Shrink (px) that counts as the keyboard — not a toolbar rounding wobble. */
export const COVERED_EPSILON = 12;

/**
 * Slack (px) before a layout viewport counts as taller than the device.
 *
 * CSS pixels against UIKit points are worth a pixel or two of rounding; a
 * webview that came back from suspension wrong is wrong by tens.
 */
export const OVERSIZE_EPSILON = 8;

/** Everything the shell's box is derived from, read straight off `window`. */
export type ViewportMetrics = {
  innerWidth: number;
  innerHeight: number;
  screenWidth: number;
  screenHeight: number;
  visualHeight: number;
};

/** The live window's metrics; zeroes where there is no DOM. */
export function readViewportMetrics(): ViewportMetrics {
  if (typeof window === 'undefined') {
    return { innerWidth: 0, innerHeight: 0, screenWidth: 0, screenHeight: 0, visualHeight: 0 };
  }
  const screen = window.screen;
  return {
    innerWidth: window.innerWidth,
    innerHeight: window.innerHeight,
    screenWidth: screen?.width ?? 0,
    screenHeight: screen?.height ?? 0,
    visualHeight: window.visualViewport?.height ?? window.innerHeight,
  };
}

/**
 * The tallest box the device can actually show, in CSS pixels.
 *
 * `screen` describes the hardware and survives everything the app does to
 * itself; the layout viewport does not. iOS hands a resumed WKWebView a box
 * that is TALLER than the screen — the well-known "innerHeight comes back as
 * the full display instead of the safe area" resume bug — and it fires no
 * event afterwards, so `100dvh` (and `visualViewport`, which measures that
 * same wrong box) keeps the shell hanging off the bottom of the device: the
 * tab bar and the composer sit below the fold with nothing to scroll them
 * back. This is the only fixed ruler the web layer has.
 *
 * `screen` may or may not swap its axes with the orientation depending on the
 * iOS version, so the orientation is taken from the window and the screen only
 * supplies the pair of edge lengths.
 */
export function deviceHeightLimit(m: ViewportMetrics): number {
  const short = Math.min(m.screenWidth, m.screenHeight);
  const long = Math.max(m.screenWidth, m.screenHeight);
  if (!(short > 0)) return Number.POSITIVE_INFINITY;
  return m.innerWidth > m.innerHeight ? short : long;
}

/** True when the layout viewport claims more height than the device has. */
export function isViewportOversized(m: ViewportMetrics): boolean {
  return m.innerHeight - deviceHeightLimit(m) > OVERSIZE_EPSILON;
}

/**
 * True when something (the keyboard, a form accessory bar) covers the bottom
 * of the layout viewport. Measured against the clamped layout height, so an
 * oversized viewport does not read as a keyboard.
 */
export function isKeyboardCovering(m: ViewportMetrics): boolean {
  const layout = Math.min(m.innerHeight, deviceHeightLimit(m));
  return layout - m.visualHeight > COVERED_EPSILON;
}

/** A shell height that can never exceed what the device can show. */
export function clampShellHeight(height: number, m: ViewportMetrics): number {
  return Math.max(0, Math.min(Math.round(height), deviceHeightLimit(m)));
}

/**
 * The layout viewport's height, never more than the device can show — the
 * ruler the keyboard pin subtracts from.
 */
export function layoutHeight(m: ViewportMetrics): number {
  return Math.min(m.innerHeight, deviceHeightLimit(m));
}

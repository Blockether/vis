/**
 * Shell geometry as pure arithmetic over one set of viewport numbers.
 *
 * It lives apart from `viewport.ts` — which owns the React state and the
 * Capacitor listeners — so the rules below can be exercised directly with
 * metrics recorded off a device, with no DOM and no plugins.
 */

import { readNativeViewportBox } from './native-viewport';

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
  /** Width UIKit reports for the app's own view; 0 where native stays silent. */
  nativeWidth: number;
  /** Height UIKit reports for the app's own view; 0 where native stays silent. */
  nativeHeight: number;
  /** What the OS says the device is doing; null when it will not say. */
  isLandscape: boolean | null;
};

/**
 * The orientation according to the OS, not to the layout viewport.
 *
 * A rotation is the one moment the two disagree: WebKit swaps `screen`'s and
 * `screen.orientation`'s axes with the hardware, while the layout viewport can
 * stay at its pre-rotation size for several frames — and after a rotation that
 * happened while the app was suspended it can stay wrong indefinitely, because
 * iOS emits no further event. Deriving "are we landscape?" from
 * `innerWidth > innerHeight` therefore asks the broken ruler whether it is
 * broken: a portrait-shaped stale viewport in a landscape device measures as
 * portrait, its limit comes out as the LONG screen edge, nothing looks
 * oversized, and the shell keeps a portrait height inside a landscape window —
 * the composer and tab bar hanging off the screen.
 *
 * `screen.orientation` is WKWebView-backed from iOS 16.4; `window.orientation`
 * covers older builds; a browser that offers neither (some desktops) gets
 * `null` and the viewport-derived guess below.
 */
export function readScreenOrientation(): boolean | null {
  if (typeof window === 'undefined') return null;
  const type = window.screen?.orientation?.type;
  if (typeof type === 'string') return type.startsWith('landscape');
  const angle = (window as { orientation?: number }).orientation;
  if (typeof angle === 'number') return Math.abs(angle) === 90;
  return null;
}

/** The live window's metrics; zeroes where there is no DOM. */
export function readViewportMetrics(): ViewportMetrics {
  if (typeof window === 'undefined') {
    return {
      innerWidth: 0,
      innerHeight: 0,
      screenWidth: 0,
      screenHeight: 0,
      visualHeight: 0,
      nativeWidth: 0,
      nativeHeight: 0,
      isLandscape: null,
    };
  }
  const screen = window.screen;
  const native = readNativeViewportBox();
  return {
    innerWidth: window.innerWidth,
    innerHeight: window.innerHeight,
    screenWidth: screen?.width ?? 0,
    screenHeight: screen?.height ?? 0,
    visualHeight: window.visualViewport?.height ?? window.innerHeight,
    isLandscape: readScreenOrientation(),
    nativeWidth: native?.width ?? 0,
    nativeHeight: native?.height ?? 0,
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
 * iOS version, so it only supplies the pair of edge lengths; which one is the
 * height comes from the OS orientation, and from the layout viewport's own
 * shape only when the OS will not say (see `readScreenOrientation`).
 */
export function deviceHeightLimit(m: ViewportMetrics): number {
  // UIKit's own bounds beat every web ruler when the host reports them: they
  // are the box the app really occupies, so they also cover what `screen`
  // cannot express — an iPad split view, a Stage Manager window, and the exact
  // post-rotation size while the flip is still animating.
  if (m.nativeHeight > 0) return m.nativeHeight;
  const short = Math.min(m.screenWidth, m.screenHeight);
  const long = Math.max(m.screenWidth, m.screenHeight);
  if (!(short > 0)) return Number.POSITIVE_INFINITY;
  const landscape = m.isLandscape ?? m.innerWidth > m.innerHeight;
  return landscape ? short : long;
}

/** True when the layout viewport claims more height than the device has. */
export function isViewportOversized(m: ViewportMetrics): boolean {
  return m.innerHeight - deviceHeightLimit(m) > OVERSIZE_EPSILON;
}

/** The widest box the device can show in its current orientation. */
export function deviceWidthLimit(m: ViewportMetrics): number {
  if (m.nativeWidth > 0) return m.nativeWidth;
  const short = Math.min(m.screenWidth, m.screenHeight);
  const long = Math.max(m.screenWidth, m.screenHeight);
  if (!(short > 0)) return Number.POSITIVE_INFINITY;
  const landscape = m.isLandscape ?? m.innerWidth > m.innerHeight;
  return landscape ? long : short;
}

/**
 * True when the layout viewport already fits the orientation the OS is in.
 *
 * The rotation window has to stay open until the reflow has actually happened,
 * and "the numbers stopped moving" is not that: iOS often does not touch
 * `innerWidth`/`innerHeight` for the first frames of a flip, so they read as
 * held-still while they still describe the previous orientation. Closing there
 * hands the reflow to an app that is no longer freezing motion or replaying its
 * scroll anchor — the transcript jumps a screenful and the shell flashes at the
 * wrong size. A viewport that still sticks out past EITHER edge of the current
 * orientation box is by definition pre-rotation. Sizes only, never the exact
 * dimensions: iPad split view and Android multi-window legitimately give the
 * app less than the whole screen.
 */
export function isViewportSettled(m: ViewportMetrics): boolean {
  // With a native box there is an exact target to compare against, orientation
  // or not. Without one, an OS orientation and a hardware ruler are the only
  // way to tell a pre-rotation viewport from a settled one; a browser that has
  // neither is never mid-flip in a way we could detect, so it is always settled.
  const native = m.nativeWidth > 0 && m.nativeHeight > 0;
  if (!native && (m.isLandscape === null || !(Math.min(m.screenWidth, m.screenHeight) > 0)))
    return true;
  return (
    m.innerWidth - deviceWidthLimit(m) <= OVERSIZE_EPSILON &&
    m.innerHeight - deviceHeightLimit(m) <= OVERSIZE_EPSILON
  );
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

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
  /** What the OS says the device is doing; null when it will not say. */
  isLandscape: boolean | null;
  /**
   * True only in a native shell, where the webview IS the device screen.
   *
   * `screen` only rules the layout viewport when the two describe the same
   * box. In a browser window they do not: `screen` is the display (or, under
   * an automation driver, whatever the driver decided to report) while the
   * window is an arbitrary rectangle inside it, and `screen.orientation` is
   * the DISPLAY's orientation, which says nothing about the window's shape. A
   * 393x852 window on a landscape display therefore measured its own limit as
   * the screen's SHORT edge and pinned the whole shell to 393px tall, with the
   * tab bar and the transcript clipped at a third of the window. Only the
   * native shell gets the hardware ruler; a browser is measured by its window.
   */
  isDeviceShell: boolean;
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
      isLandscape: null,
      isDeviceShell: false,
    };
  }
  const screen = window.screen;
  // Read off the injected global rather than importing `@capacitor/core`, so
  // this module stays loadable with no plugins and no DOM.
  const capacitor = (window as { Capacitor?: { isNativePlatform?: () => boolean } }).Capacitor;
  return {
    innerWidth: window.innerWidth,
    innerHeight: window.innerHeight,
    screenWidth: screen?.width ?? 0,
    screenHeight: screen?.height ?? 0,
    visualHeight: window.visualViewport?.height ?? window.innerHeight,
    isLandscape: readScreenOrientation(),
    isDeviceShell: capacitor?.isNativePlatform?.() === true,
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
  if (!m.isDeviceShell) return Number.POSITIVE_INFINITY;
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
  if (!m.isDeviceShell) return Number.POSITIVE_INFINITY;
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
  // An OS orientation and a hardware ruler are the only way to tell a
  // pre-rotation viewport from a settled one; a browser that has neither is
  // never mid-flip in a way we could detect, so it is always settled.
  if (m.isLandscape === null || !(Math.min(m.screenWidth, m.screenHeight) > 0)) return true;
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

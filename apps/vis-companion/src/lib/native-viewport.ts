/**
 * The box UIKit says the app occupies, pushed in from native code.
 *
 * Everything else in the web layer measures the same WKWebView that is wrong:
 * `innerHeight`, `100dvh` and `visualViewport` all describe the layout viewport,
 * and WebKit leaves that at its previous size for a while after a rotation — and
 * indefinitely after a resume, since iOS then fires no further event. `screen`
 * is the only fixed web-side ruler (see `viewport-metrics.ts`), and it is a
 * *hardware* ruler: it cannot express an iPad split view, a Stage Manager
 * window, or the moment during a flip when the animation is half-way through.
 *
 * There is no Capacitor plugin for this — `@capacitor/screen-orientation` only
 * reports which way up the device is, which `screen.orientation` already tells
 * us. What is missing is the SIZE, and only UIKit has it: `viewWillTransition`
 * receives the exact post-rotation size before a single frame is drawn, its
 * coordinator says when the animation finished, and `view.bounds` after a
 * resume is authoritative while the webview's own numbers are still stale.
 *
 * So the iOS host (`ios/App/App/AppDelegate.swift`, `VisBridgeViewController`)
 * dispatches those points onto `window` and this module keeps the newest one.
 * Nothing here is required: on the web build and on Android the event never
 * arrives, `readNativeViewportBox()` stays `null`, and every consumer falls
 * back to the `screen`-derived rules unchanged.
 */

/** Why native measured: a flip starting, its end, a relayout, a resume. */
export type NativeViewportPhase = 'rotate' | 'settled' | 'layout' | 'resume';

/** One native measurement, in points (== CSS px: the webview cannot zoom). */
export type NativeViewportBox = {
  width: number;
  height: number;
  phase: NativeViewportPhase;
  /** `performance.now()` when it arrived. */
  at: number;
};

/** The `window` event `VisBridgeViewController` dispatches. */
export const NATIVE_VIEWPORT_EVENT = 'visviewport';

const PHASES: readonly string[] = ['rotate', 'settled', 'layout', 'resume'];

let latest: NativeViewportBox | null = null;
let installed = false;
const listeners = new Set<(box: NativeViewportBox) => void>();

function size(value: unknown): number {
  return typeof value === 'number' && Number.isFinite(value) && value > 0 ? value : 0;
}

/**
 * Capacitor's `triggerWindowJSEvent` copies the payload onto the event object
 * itself; a plain `CustomEvent` (what a test or a future plugin would send)
 * carries it under `detail`. Accept both.
 */
export function parseNativeViewport(event: Event): NativeViewportBox | null {
  const source = event as unknown as Record<string, unknown>;
  const detail = source.detail as Record<string, unknown> | undefined;
  const payload = detail && typeof detail === 'object' ? detail : source;
  const width = size(payload.width);
  const height = size(payload.height);
  if (!width || !height) return null;
  const phase = payload.phase;
  return {
    width,
    height,
    phase: (typeof phase === 'string' && PHASES.includes(phase)
      ? phase
      : 'layout') as NativeViewportPhase,
    at: typeof performance === 'undefined' ? 0 : performance.now(),
  };
}

/** The newest native measurement, or null where native never speaks. */
export function readNativeViewportBox(): NativeViewportBox | null {
  return latest;
}

/** Starts listening. Idempotent, and a no-op without a DOM. */
export function installNativeViewport(): void {
  if (installed || typeof window === 'undefined') return;
  installed = true;
  window.addEventListener(NATIVE_VIEWPORT_EVENT, (event: Event) => {
    const box = parseNativeViewport(event);
    if (!box) return;
    latest = box;
    for (const listener of [...listeners]) listener(box);
  });
}

/** Subscribe to native measurements. Returns an unsubscribe. */
export function onNativeViewport(listener: (box: NativeViewportBox) => void): () => void {
  installNativeViewport();
  listeners.add(listener);
  return () => {
    listeners.delete(listener);
  };
}

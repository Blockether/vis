import { App } from '@capacitor/app';

/**
 * One wake bus for the whole app.
 *
 * "Wake" is the moment the app becomes live again after the OS froze it:
 * returning from the background, a tab switch, a bfcache restore, a network
 * coming back. Every screen needs the same reaction (re-poll, reconnect the
 * SSE stream, re-measure), and every screen used to wire its own subset of
 * `visibilitychange` / `pageshow` / `online` handlers.
 *
 * Two reasons that failed and left the UI frozen until a full app restart:
 *
 * 1. A Capacitor iOS webview can resume without firing ANY DOM wake event.
 *    Only the native `App` plugin (`resume` / `appStateChange`) is reliable,
 *    and a screen that listened to DOM events alone simply never heard about
 *    the resume — stale list, dead stream, no error anywhere.
 * 2. When the events do fire they arrive in a burst (three or four in one
 *    tick), so each listener re-ran its work several times over.
 *
 * So: subscribe to every signal exactly once, coalesce the burst, and fan a
 * single `wake` out to subscribers.
 */

type WakeListener = () => void;

/** Collapse the burst of wake signals the platforms fire together. */
const COALESCE_MS = 250;

const listeners = new Set<WakeListener>();
let installed = false;
let uninstallNative: (() => void) | null = null;
let timer: ReturnType<typeof setTimeout> | null = null;

function emit(): void {
  timer = null;
  for (const listener of [...listeners]) {
    try {
      listener();
    } catch {
      // One bad subscriber must never starve the others.
    }
  }
}

function schedule(): void {
  if (typeof document !== 'undefined' && document.visibilityState === 'hidden') return;
  if (timer !== null) return;
  timer = setTimeout(emit, COALESCE_MS);
}

function onVisibility(): void {
  if (document.visibilityState === 'visible') schedule();
}

function install(): void {
  if (installed || typeof window === 'undefined') return;
  installed = true;
  document.addEventListener('visibilitychange', onVisibility);
  window.addEventListener('pageshow', schedule);
  window.addEventListener('focus', onVisibility);
  window.addEventListener('online', schedule);

  // Native resume: the one signal an iOS/Android webview always delivers.
  // No-op on the web build, where the plugin is a stub.
  try {
    const pending: Array<{ remove: () => void }> = [];
    let removed = false;
    const track = (sub: { remove: () => void }) => {
      if (removed) sub.remove();
      else pending.push(sub);
    };
    void App.addListener('resume', schedule).then(track).catch(() => {});
    void App.addListener('appStateChange', ({ isActive }) => {
      if (isActive) schedule();
    }).then(track).catch(() => {});
    uninstallNative = () => {
      removed = true;
      for (const sub of pending.splice(0)) sub.remove();
    };
  } catch {
    uninstallNative = null;
  }
}

function uninstall(): void {
  if (!installed) return;
  installed = false;
  document.removeEventListener('visibilitychange', onVisibility);
  window.removeEventListener('pageshow', schedule);
  window.removeEventListener('focus', onVisibility);
  window.removeEventListener('online', schedule);
  uninstallNative?.();
  uninstallNative = null;
  if (timer !== null) {
    clearTimeout(timer);
    timer = null;
  }
}

/**
 * Run `listener` whenever the app wakes. Returns the unsubscribe; the shared
 * platform listeners exist only while someone is subscribed.
 */
export function onWake(listener: WakeListener): () => void {
  listeners.add(listener);
  install();
  return () => {
    listeners.delete(listener);
    if (listeners.size === 0) uninstall();
  };
}

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

/**
 * How long the app was frozen before this wake. Screens use it to tell a glance
 * at a notification (come back exactly where you were) from a real absence
 * (come back to what is current).
 */
export type WakeInfo = { awayMs: number };

type WakeListener = (info: WakeInfo) => void;

/** Collapse the burst of wake signals the platforms fire together. */
const COALESCE_MS = 250;

const listeners = new Set<WakeListener>();
let installed = false;
let uninstallNative: (() => void) | null = null;
let timer: ReturnType<typeof setTimeout> | null = null;
// When the app went away, so a wake can say for how long. Null means "never
// left" — a network `online` burst or a bfcache-less reload is a wake with no
// absence behind it, and reporting 0 keeps those from being mistaken for one.
let sleptAt: number | null = null;

function emit(): void {
  timer = null;
  const awayMs = sleptAt === null ? 0 : Math.max(0, Date.now() - sleptAt);
  sleptAt = null;
  const info: WakeInfo = { awayMs };
  for (const listener of [...listeners]) {
    try {
      listener(info);
    } catch {
      // One bad subscriber must never starve the others.
    }
  }
}

// Stamp the FIRST away signal only: iOS fires visibilitychange, pagehide and
// the native pause together, and a later one must not shorten the absence.
function markAway(): void {
  if (sleptAt === null) sleptAt = Date.now();
}

function schedule(): void {
  if (typeof document !== 'undefined' && document.visibilityState === 'hidden') return;
  if (timer !== null) return;
  timer = setTimeout(emit, COALESCE_MS);
}

function onVisibility(): void {
  if (document.visibilityState === 'visible') schedule();
  else markAway();
}

function install(): void {
  if (installed || typeof window === 'undefined') return;
  installed = true;
  document.addEventListener('visibilitychange', onVisibility);
  window.addEventListener('pageshow', schedule);
  window.addEventListener('pagehide', markAway);
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
    void App.addListener('pause', markAway).then(track).catch(() => {});
    void App.addListener('appStateChange', ({ isActive }) => {
      if (isActive) schedule();
      else markAway();
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
  window.removeEventListener('pagehide', markAway);
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

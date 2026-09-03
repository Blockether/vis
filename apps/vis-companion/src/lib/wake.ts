import { App } from '@capacitor/app';

/**
 * Coalesce native resume, DOM visibility/pageshow and network recovery into one app-wide
 * wake event; native webviews do not reliably emit every DOM signal.
 */

/** Time since the app last became inactive. */
export type WakeInfo = { awayMs: number };

type WakeListener = (info: WakeInfo) => void;
type AwayListener = () => void;

/** Collapse the burst of wake signals the platforms fire together. */
const COALESCE_MS = 250;

const listeners = new Set<WakeListener>();
const awayListeners = new Set<AwayListener>();
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

// Stamp and announce the FIRST away signal only: iOS fires visibilitychange,
// pagehide and native pause together. Subscribers must retire live transports
// before WebKit freezes them, while a duplicate signal has nothing left to do.
function markAway(): void {
  if (sleptAt !== null) return;
  sleptAt = Date.now();
  for (const listener of [...awayListeners]) {
    try {
      listener();
    } catch {
      // One bad subscriber must never starve the others.
    }
  }
}

// `force` is for the NATIVE resume signals. A Capacitor iOS webview reports
// `document.visibilityState === 'hidden'` for a while AFTER the app is already
// foreground — sometimes until the user touches the screen — so gating the
// native signal on it threw away the one wake event the platform guarantees,
// and the app reconnected only when a tap/focus finally flipped the DOM. DOM
// signals keep the guard: `online`/`pageshow` do fire in the background.
function schedule(force = false): void {
  if (!force && typeof document !== 'undefined' && document.visibilityState === 'hidden') return;
  if (timer !== null) return;
  timer = setTimeout(emit, COALESCE_MS);
}

// Listeners are wired to these, never to `schedule` itself: a DOM handler is
// called with an Event, which would read as a truthy `force`.
function scheduleFromDom(): void {
  schedule();
}

function scheduleFromNative(): void {
  schedule(true);
}

function onVisibility(): void {
  if (document.visibilityState === 'visible') scheduleFromDom();
  else markAway();
}

function install(): void {
  if (installed || typeof window === 'undefined') return;
  installed = true;
  document.addEventListener('visibilitychange', onVisibility);
  window.addEventListener('pageshow', scheduleFromDom);
  window.addEventListener('pagehide', markAway);
  window.addEventListener('focus', onVisibility);
  window.addEventListener('online', scheduleFromDom);

  // Native resume: the one signal an iOS/Android webview always delivers.
  // No-op on the web build, where the plugin is a stub.
  try {
    const pending: Array<{ remove: () => void }> = [];
    let removed = false;
    const track = (sub: { remove: () => void }) => {
      if (removed) sub.remove();
      else pending.push(sub);
    };
    void App.addListener('resume', scheduleFromNative).then(track).catch(() => {});
    void App.addListener('pause', markAway).then(track).catch(() => {});
    void App.addListener('appStateChange', ({ isActive }) => {
      if (isActive) scheduleFromNative();
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
  window.removeEventListener('pageshow', scheduleFromDom);
  window.removeEventListener('pagehide', markAway);
  window.removeEventListener('focus', onVisibility);
  window.removeEventListener('online', scheduleFromDom);
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
    if (listeners.size === 0 && awayListeners.size === 0) uninstall();
  };
}

/**
 * Run `listener` synchronously on the first signal that the app is leaving the
 * foreground. This is the last safe moment to retire WebKit network streams.
 */
export function onAway(listener: AwayListener): () => void {
  awayListeners.add(listener);
  install();
  return () => {
    awayListeners.delete(listener);
    if (listeners.size === 0 && awayListeners.size === 0) uninstall();
  };
}

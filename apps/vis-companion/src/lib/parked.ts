/**
 * Where a reader was, kept across the RELOAD that used to lose it.
 *
 * `lib/list-scroll` and `lib/reading-position` both park a place in a module
 * variable, which survives a screen unmount and dies with the JavaScript
 * context. That was wrong about one thing a user does constantly: pressing
 * reload. Nothing unmounts, no cleanup runs, the whole context is thrown away —
 * so a reader at the bottom of a list came back at the top of it, on a screen
 * that had just told them their place was remembered.
 *
 * `sessionStorage` is exactly the lifetime those two docstrings describe: it
 * survives a reload of THIS visit and dies when the tab (or the app's webview)
 * goes away, so a genuinely cold start still has no reading position to honour.
 * `localStorage` would outlive the visit and hand someone yesterday's scroll
 * offset for a list that has since changed underneath them.
 *
 * Every access is guarded: private mode, a disabled store and a non-browser
 * (node tests, SSR) all mean "no parked place", never a thrown screen.
 *
 * WRITES ARE COALESCED, because both readers mark their place from a scroll
 * handler — once per animation frame, for as long as a finger is on the glass —
 * and `sessionStorage.setItem` is synchronous: it serializes the value and, in
 * a webview, hands it to the storage process before returning. Paying that in
 * every frame of the one gesture this module exists to serve is a stutter in
 * exactly the wrong place, and on a long transcript it was one of two things
 * making a live session scroll badly on iOS. The place is kept in memory,
 * written at most once per QUIET_MS, and flushed the moment this page could be
 * thrown away (`pagehide`, plus `visibilitychange` for a webview the OS
 * backgrounds) — which is every path a reload or an app switch takes, so what
 * a reader gets back is unchanged.
 */

/** How long a burst of marks may share one store write. */
const QUIET_MS = 400;

/** Marks not yet written. `null` is a forget, exactly as in `writeParked`. */
const pending = new Map<string, unknown>();
let quiet: ReturnType<typeof setTimeout> | null = null;
// True from the moment this page announces it can be thrown away until it is
// back on the glass. Coalescing is for a reader working the scroller; on the way
// out every mark is written through, whatever order the listeners run in.
let leaving = false;

function store(): Storage | null {
  try {
    return globalThis.sessionStorage ?? null;
  } catch {
    return null;
  }
}

function writeNow(key: string, value: unknown): void {
  try {
    if (value === null) store()?.removeItem(key);
    else store()?.setItem(key, JSON.stringify(value));
  } catch {
    // Quota or private mode: the in-memory copy is still the live answer.
  }
}

/** Write every mark still in hand. Safe to call when there is nothing to write. */
export function flushParked(): void {
  if (quiet !== null) {
    clearTimeout(quiet);
    quiet = null;
  }
  if (pending.size === 0) return;
  for (const [key, value] of pending) writeNow(key, value);
  pending.clear();
}

// The page can go away without unmounting anything: reload, a link out, the OS
// backgrounding the webview. Both signals are the last moment a mark can still
// be saved, and both are watched from module load — a screen that parks its
// place IN a `pagehide` handler adds its listener after this one, so waiting for
// a first write to start watching would miss exactly that mark.
function watchForTheWayOut(): void {
  if (typeof globalThis.addEventListener !== "function") return;
  const goingAway = () => {
    leaving = true;
    flushParked();
  };
  const staying = () => {
    leaving = false;
  };
  globalThis.addEventListener("pagehide", goingAway);
  globalThis.addEventListener("pageshow", staying);
  globalThis.addEventListener("visibilitychange", () => {
    if (globalThis.document?.visibilityState === "hidden") goingAway();
    else staying();
  });
}

watchForTheWayOut();

/**
 * The value parked under `key`, passed through `revive` so a stale or corrupt
 * entry from an older build is dropped rather than trusted.
 */
export function readParked<T>(key: string, revive: (raw: unknown) => T | null): T | null {
  // A mark still in hand is the newest one there is: it has simply not been
  // billed to the store yet.
  if (pending.has(key)) {
    const held = pending.get(key);
    return held === null ? null : revive(held);
  }
  const raw = (() => {
    try {
      return store()?.getItem(key) ?? null;
    } catch {
      return null;
    }
  })();
  if (raw === null) return null;
  try {
    return revive(JSON.parse(raw) as unknown);
  } catch {
    return null;
  }
}

/** Park `value` under `key`; `null` forgets it. Written once the marks go quiet. */
export function writeParked(key: string, value: unknown): void {
  pending.set(key, value);
  if (leaving) {
    flushParked();
    return;
  }
  if (quiet !== null) return;
  quiet = setTimeout(() => {
    quiet = null;
    flushParked();
  }, QUIET_MS);
}

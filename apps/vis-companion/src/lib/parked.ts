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
 */

function store(): Storage | null {
  try {
    return globalThis.sessionStorage ?? null;
  } catch {
    return null;
  }
}

/**
 * The value parked under `key`, passed through `revive` so a stale or corrupt
 * entry from an older build is dropped rather than trusted.
 */
export function readParked<T>(key: string, revive: (raw: unknown) => T | null): T | null {
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

/** Park `value` under `key`; `null` forgets it. */
export function writeParked(key: string, value: unknown): void {
  try {
    if (value === null) store()?.removeItem(key);
    else store()?.setItem(key, JSON.stringify(value));
  } catch {
    // Quota or private mode: the in-memory copy is still the live answer.
  }
}

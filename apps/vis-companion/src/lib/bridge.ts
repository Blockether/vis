// The one place that knows the Capacitor native bridge can stop answering.
//
// A plugin call is a message to native code plus a promise that settles when
// native answers back. On iOS that answer travels through the WKWebView bridge,
// and the bridge does not survive everything: iOS routinely kills the
// WebContent process of a backgrounded app (memory pressure, a few hours away),
// the page is reloaded on resume, and replies for calls issued around that
// window are simply never delivered — the plugin listener registrations on the
// native side no longer match the fresh JS context
// (ionic-team/capacitor#7810, #8101, #8143). The promise then neither resolves
// NOR rejects, so `try/catch` catches nothing and `await` waits forever.
//
// That is what made "reopen the app after a night" look like a dead app: the
// boot path awaited `Preferences.get(...)`, the reply never came, and the
// splash stayed up until the app was force-quit. So every bridge call the app
// makes is bounded and carries its own answer for when the bridge gives none.

/**
 * Long enough for a genuinely cold plugin call on a slow device, short enough
 * that a wedged bridge never reads as a hang.
 */
export const BRIDGE_TIMEOUT_MS = 2_000;

/**
 * Run a native plugin call with a deadline. `fallback` produces the value to
 * use when the bridge fails OR stays silent; it must be synchronous and local
 * (no second bridge call — the bridge is the thing that is broken).
 */
export function bridged<T>(
  call: () => Promise<T>,
  fallback: () => T,
  timeoutMs: number = BRIDGE_TIMEOUT_MS,
): Promise<T> {
  return new Promise<T>((resolve) => {
    let settled = false;
    let timer: ReturnType<typeof setTimeout> | null = null;
    const settle = (value: T) => {
      if (settled) return;
      settled = true;
      if (timer !== null) clearTimeout(timer);
      timer = null;
      resolve(value);
    };
    const giveUp = () => {
      try {
        settle(fallback());
      } catch {
        // A fallback must never be the reason a caller hangs; it simply has no
        // value to offer, and `undefined` is what an unbounded call would have
        // produced anyway.
        settle(undefined as T);
      }
    };
    timer = setTimeout(giveUp, timeoutMs);
    try {
      call().then(settle, giveUp);
    } catch {
      giveUp();
    }
  });
}

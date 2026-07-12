/* Jest setup for the companion app's unit tests.

   The suites here exercise PURE logic (SSE reducer, markdown block splitter,
   gateway error unwrapping, time/id formatting) — no device, no native modules.
   We only backfill the couple of web globals the gateway client reaches for
   under the jest-expo (node) environment. */

/* eslint-disable @typescript-eslint/no-explicit-any */

// react-native-sse is mocked per-suite; the RN Fetch globals (fetch/Headers)
// exist natively on Node 18+. Backfill a minimal Headers only if absent so the
// client's header plumbing never throws under an older runtime.
if (typeof (globalThis as any).Headers === "undefined") {
  (globalThis as any).Headers = class {
    private map = new Map<string, string>();
    constructor(init?: Record<string, string>) {
      if (init) for (const [k, v] of Object.entries(init)) this.map.set(k.toLowerCase(), v);
    }
    set(k: string, v: string) {
      this.map.set(k.toLowerCase(), v);
    }
    get(k: string) {
      return this.map.get(k.toLowerCase()) ?? null;
    }
    has(k: string) {
      return this.map.has(k.toLowerCase());
    }
  };
}

// The app guards on `typeof __DEV__ !== "undefined"`, so leaving it undefined is
// safe — but define it false to mirror a production bundle explicitly.
(globalThis as any).__DEV__ = false;

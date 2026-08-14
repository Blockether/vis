// The rule every split point in the app leans on, at its own level: `warm` is
// handed a load with no caller, so a load that fails has to end there.
import { describe, expect, it } from "vitest";

import { settleRejections, watchUnhandledRejections } from "./unhandled.fixture";
import { warm } from "./warm";

describe("warm", () => {
  it("keeps a failed load off the platform's unhandled-rejection channel", async () => {
    const watch = watchUnhandledRejections();
    try {
      warm(Promise.reject(new Error("chunk gone")));
      await settleRejections();
      expect(watch.escaped).toEqual([]);
    } finally {
      watch.stop();
    }
  });
});

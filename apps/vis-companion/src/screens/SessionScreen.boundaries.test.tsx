import { describe, expect, it } from "vitest";

import sessionScreenSource from "./SessionScreen.tsx?raw";

describe("session feature boundaries", () => {
  it("leaves queued-turn interaction outside the screen orchestrator", () => {
    const leaks = [
      ["queued edit state", /\[editingQueued,\s*setEditingQueued\]/],
      ["queued row rendering", /queued\.map\(\(item/],
      ["queued update request", /\.updateQueuedTurn\(/],
      ["queued delete request", /\.deleteQueuedTurn\(/],
      ["queue resume request", /\.resumeQueue\(/],
    ]
      .filter(([, pattern]) => (pattern as RegExp).test(sessionScreenSource))
      .map(([name]) => name);

    expect(leaks).toEqual([]);
  });
});

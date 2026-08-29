import { describe, expect, it } from "vitest";

import { sessionEventBatch } from "./session-stream";
import type { SseEvent } from "./types";

function event(type: string, seq: number, kind?: 'live'): SseEvent {
  return { type, seq, ...(kind ? { kind } : {}) } as SseEvent;
}

// Stream-isolation contract, session 3d6dc388-a21c-4005-b498-87c02668cb34: Activity frames
// entered both reducers, making each visual update schedule a redundant running-turn pass.
describe("Activity stream isolation", () => {
  it("keeps live-view frames out of the turn reducer", () => {
    expect(
      sessionEventBatch([
        event("turn.started", 1),
        event("view.patch", 2, "live"),
        event("content.block.delta", 3),
      ]).map((frame) => frame.type),
    ).toEqual(["turn.started", "content.block.delta"]);
  });
});

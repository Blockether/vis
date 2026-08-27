// @vitest-environment jsdom
import { describe, expect, it } from "vitest";

import { coalesceLiveEvents } from "./SessionScreen";
import type { SseEvent } from "../lib/types";

function event(type: string, seq: number): SseEvent {
  return { type, seq } as unknown as SseEvent;
}

// Regression, session 3d6dc388-a21c-4005-b498-87c02668cb34: Activity frames
// entered both reducers, making each visual update schedule a redundant live-turn pass.
describe("Activity stream isolation", () => {
  it("keeps live-view frames out of the turn reducer", () => {
    expect(
      coalesceLiveEvents([
        event("turn.started", 1),
        event("human_input.live.patch", 2),
        event("content.block.delta", 3),
      ]).map((frame) => frame.type),
    ).toEqual(["turn.started", "content.block.delta"]);
  });
});

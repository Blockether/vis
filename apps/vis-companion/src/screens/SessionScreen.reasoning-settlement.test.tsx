// @vitest-environment jsdom
import { describe, expect, it } from "vitest";

import { reduceLiveEvent } from "./SessionScreen";
import type { SseEvent } from "../lib/types";

const liveTurn = {
  id: "gw-thinking",
  request: "show your plan",
  answer: "",
  iterations: [],
  startedAt: Date.now(),
  status: "running" as const,
};

function event(value: Record<string, unknown>): SseEvent {
  return value as unknown as SseEvent;
}

function reasoning(text: string): SseEvent {
  return event({
    type: "content.block.delta",
    turn_id: "gw-thinking",
    seq: 6,
    iteration: 0,
    block_id: "gw-thinking:reasoning:0",
    field: "text",
    text,
    cumulative: text,
  });
}

function completed(thinking: string | null): SseEvent {
  return event({
    type: "iteration.completed",
    turn_id: "gw-thinking",
    seq: 7,
    iteration: 0,
    thinking,
  });
}

function thought(turn: ReturnType<typeof reduceLiveEvent>): string | undefined {
  return turn?.iterations[0]?.thinking;
}

// Regression, task td-a972c8: the Companion retained its raw live fragment when
// the shared settled boundary deliberately returned no reasoning, while the TUI
// removed it. A complete Markdown heading disappeared from the TUI instead.
describe("shared reasoning settlement", () => {
  it("keeps a complete heading supplied by the settled boundary", () => {
    const live = reduceLiveEvent(liveTurn, reasoning("**Planning**"));
    expect(thought(live)).toBe("**Planning**");
    expect(thought(reduceLiveEvent(live, completed("**Planning**")))).toBe(
      "**Planning**",
    );
  });

  it("removes partially streamed prose when the settled boundary rejects it", () => {
    const live = reduceLiveEvent(
      liveTurn,
      reasoning("I should inspect the parser…"),
    );
    expect(thought(live)).toBe("I should inspect the parser…");
    expect(thought(reduceLiveEvent(live, completed(null)))).toBe("");
  });
});

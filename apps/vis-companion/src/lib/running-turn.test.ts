import { describe, expect, it } from "vitest";

import activityFixture from "./activity.fixture.json";
import { reduceRunningTurnEvent } from "./running-turn";
import type { SseEvent } from "./types";

const runningTurn = {
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

function thought(turn: ReturnType<typeof reduceRunningTurnEvent>): string | undefined {
  return turn?.iterations[0]?.thinking;
}

// Shared-settlement contract, task td-a972c8: the Companion retained its raw streamed fragment when
// the shared settled boundary deliberately returned no reasoning, while the TUI
// removed it. A complete Markdown heading disappeared from the TUI instead.
describe("shared reasoning settlement", () => {
  it("keeps a complete heading supplied by the settled boundary", () => {
    const running = reduceRunningTurnEvent(runningTurn, reasoning("**Planning**"));
    expect(thought(running)).toBe("**Planning**");
    expect(thought(reduceRunningTurnEvent(running, completed("**Planning**")))).toBe(
      "**Planning**",
    );
  });

  it("removes partially streamed prose when the settled boundary rejects it", () => {
    const running = reduceRunningTurnEvent(
      runningTurn,
      reasoning("I should inspect the parser…"),
    );
    expect(thought(running)).toBe("I should inspect the parser…");
    expect(thought(reduceRunningTurnEvent(running, completed(null)))).toBe("");
  });
});

describe("Activity ownership inside a running turn", () => {
  const started = () =>
    reduceRunningTurnEvent(
      reduceRunningTurnEvent(
        null,
        event({ type: "turn.started", turn_id: "t-activity" }),
      ),
      event({
        type: "block.started",
        iteration: 1,
        block_id: 0,
        scope: "python",
        code: "work()",
      }),
    );

  it("never invents a form for an orphan Activity snapshot", () => {
    const turn = reduceRunningTurnEvent(
      reduceRunningTurnEvent(
        null,
        event({ type: "turn.started", turn_id: "t-orphan" }),
      ),
      event({
        type: "block.activity",
        iteration: 1,
        block_id: 0,
        activity: activityFixture,
      }),
    );

    expect(turn?.iterations).toEqual([]);

    // The iteration exists but owns no such block: the frame must change nothing.
    const owner = started();
    expect(
      reduceRunningTurnEvent(
        owner,
        event({
          type: "block.activity",
          iteration: 1,
          block_id: 7,
          activity: activityFixture,
        }),
      ),
    ).toBe(owner);
  });

  it("keeps ticker progress separate from the form-owned Activity snapshot", () => {
    const withActivity = reduceRunningTurnEvent(
      started(),
      event({
        type: "block.activity",
        iteration: 1,
        block_id: 0,
        activity: activityFixture,
      }),
    );
    const withProgress = reduceRunningTurnEvent(
      withActivity,
      event({
        type: "activity",
        activity: "provider-call",
        iteration: 1,
        model: "model-a",
      }),
    );

    expect(withProgress?.progress).toMatchObject({
      kind: "provider-call",
      model: "model-a",
    });
    expect(withProgress?.iterations[0]?.forms?.[0]?.activity?.state).toBe(
      "running",
    );
    expect(withProgress).not.toHaveProperty("activity");
  });


  it("ignores a form frame that has no owner coordinate", () => {
    const turn = reduceRunningTurnEvent(
      reduceRunningTurnEvent(
        null,
        event({ type: "turn.started", turn_id: "t-ownerless" }),
      ),
      event({
        type: "block.started",
        iteration: 1,
        scope: "python",
        code: "work()",
      }),
    );

    expect(turn?.iterations).toEqual([]);
  });
});

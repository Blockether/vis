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
        form_index: 0,
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
        form_index: 0,
        activity: activityFixture,
      }),
    );

    expect(turn?.iterations).toEqual([]);

    // The iteration exists but owns no such form: the frame must change nothing.
    const owner = started();
    expect(
      reduceRunningTurnEvent(
        owner,
        event({
          type: "block.activity",
          iteration: 1,
          form_index: 7,
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
        form_index: 0,
        activity: activityFixture,
      }),
    );
    const withProgress = reduceRunningTurnEvent(
      withActivity,
      event({
        type: "turn.progress",
        progress: "provider-call",
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

  it("accepts settled Activity only from block.activity, never block.output", () => {
    const settledActivity = { ...activityFixture, state: "succeeded" };
    const settled = reduceRunningTurnEvent(
      started(),
      event({
        type: "block.activity",
        iteration: 1,
        form_index: 0,
        activity: settledActivity,
      }),
    );
    const afterOutput = reduceRunningTurnEvent(
      settled,
      event({
        type: "block.output",
        iteration: 1,
        form_index: 0,
        code: "work()",
        stdout: "done\n",
        activity: activityFixture,
      }),
    );

    expect(afterOutput?.iterations[0]?.forms?.[0]?.activity?.state).toBe("succeeded");
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

// Regression, reported from a phone: "when a new turn starts there is sometimes a
// flicker, as if the status went from sent to accepted by the gateway". The
// elapsed line under "Vis" counts `Date.now() - startedAt` on the DEVICE, and
// `turn.started` carries the GATEWAY host's `started_at`: adopting it moved the
// counter by the whole clock difference the instant the frame landed — forward by
// the skew, or back to 0ms on a phone running ahead.
describe("the turn this device started", () => {
  const optimistic = { ...runningTurn, id: "", startedAt: 1_000_000 };
  const startedFrame = (id: string, at: number) =>
    event({ type: "turn.started", turn_id: id, request: "show your plan", started_at: at });

  it("keeps its own clock when turn.started names it", () => {
    const started = reduceRunningTurnEvent(optimistic, startedFrame("gw-1", 1_009_000));

    expect(started?.startedAt).toBe(1_000_000);
    expect(started?.id).toBe("gw-1");
  });

  it("keeps its own clock once the POST has already named the turn", () => {
    const named = { ...optimistic, id: "gw-1" };

    expect(reduceRunningTurnEvent(named, startedFrame("gw-1", 991_000))?.startedAt).toBe(
      1_000_000,
    );
  });

  it("takes the gateway's stamp for a turn it is not painting yet", () => {
    expect(reduceRunningTurnEvent(null, startedFrame("gw-2", 1_700_000))?.startedAt).toBe(
      1_700_000,
    );
  });
});

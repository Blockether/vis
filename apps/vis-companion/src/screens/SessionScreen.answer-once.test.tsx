// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { screen } from "@testing-library/react";

import { renderSessionScreen } from "./session-screen-harness";
import type { SseEvent } from "../lib/types";

const linger = (ms: number) => new Promise((done) => setTimeout(done, ms));

/** How many times the reader can see `text` on the screen right now. */
function painted(text: string): number {
  return (document.body.textContent ?? "").split(text).length - 1;
}

// Regression, issue #145: a turn whose LAST iteration is prose-only used to paint
// its answer TWICE — the live stream stayed on the iteration that produced it
// while the identical Markdown settled as the turn's answer under a fresh block
// id. It was reported against the TUI; this pins the same guarantee here, where
// the same three copies of one answer meet: the streamed live prose, the terminal
// frame's own content, and the persisted row that replaces the bubble.
describe("an answer that arrives more than once", () => {
  const ANSWER = "The final answer, in prose, with no code form.";
  const bubble = {
    id: "gw-1",
    request: "explain the failure",
    answer: "",
    iterations: [{ position: 0, thinking: "weighing it up" }],
    startedAt: Date.now(),
    status: "running" as const,
  };
  // The row the engine persists for that same turn, carrying the answer again.
  const settledRow = {
    id: "engine-row-1",
    user_request: "explain the failure",
    status: "completed",
    created_at: Date.now(),
    content: [{ id: "b1", type: "prose", markdown: ANSWER }],
    iterations: [{ position: 0, thinking: "weighing it up" }],
  };

  function mount() {
    let emit: (event: SseEvent) => void = () => {};
    renderSessionScreen({
      client: {
        cachedLiveTurn: () => ({ turn: bubble, seq: 5 }),
        cachedTranscript: () => [],
        transcript: () => Promise.resolve([settledRow]),
      },
      subscriptions: {
        subscribeSession: (
          _sid: string,
          listener: (event: SseEvent) => void,
        ) => {
          emit = listener;
          return () => {};
        },
      },
    });
    return (event: Record<string, unknown>) => emit(event as unknown as SseEvent);
  }

  it("paints it once when the terminal frame repeats the streamed prose", async () => {
    const emit = mount();
    expect(await screen.findByText("explain the failure")).toBeInTheDocument();

    // The last iteration writes no code, so its prose streams on the TURN's own
    // content block — the gateway's `<turn-id>:content:<n>`, never a
    // `:assistant-prose:` one.
    emit({
      type: "content.block.delta",
      turn_id: "gw-1",
      seq: 10,
      iteration: 2,
      block_id: "gw-1:content:6",
      field: "markdown",
      text: ANSWER,
      cumulative: ANSWER,
    });
    expect(await screen.findByText(ANSWER)).toBeInTheDocument();
    expect(painted(ANSWER)).toBe(1);

    // The terminal frame ships the SAME Markdown under a fresh block id.
    emit({
      type: "turn.completed",
      turn_id: "gw-1",
      seq: 11,
      status: "completed",
      content: [{ id: "block_b879da55", type: "prose", markdown: ANSWER }],
    });
    await linger(50);
    expect(painted(ANSWER)).toBe(1);
  });

  it("still paints it once after the persisted row takes the bubble's place", async () => {
    const emit = mount();
    expect(await screen.findByText("explain the failure")).toBeInTheDocument();
    emit({
      type: "content.block.delta",
      turn_id: "gw-1",
      seq: 10,
      iteration: 2,
      block_id: "gw-1:content:6",
      field: "markdown",
      text: ANSWER,
      cumulative: ANSWER,
    });
    emit({
      type: "turn.completed",
      turn_id: "gw-1",
      seq: 11,
      status: "completed",
      content: [{ id: "block_b879da55", type: "prose", markdown: ANSWER }],
    });

    // Long enough for the settle poll to have read the transcript and swapped the
    // live bubble for the persisted row.
    await linger(600);
    expect(painted(ANSWER)).toBe(1);
  });
});

// Regression, issue #145 in its other carrier: the model narrates its answer and
// then passes the SAME text to `done(...)`, so the persisted row holds it twice —
// once as the iteration's `assistant_prose` (`prose-beyond-code` only strips prose
// that restates the CODE) and once as the turn's answer block. The trace and the
// answer band would then paint one answer twice, at two different widths.
describe("an answer its own trace repeats", () => {
  const ANSWER = "The change is in state.clj and the suite is green.";
  const narratedRow = {
    id: "engine-row-2",
    user_request: "summarize the fix",
    status: "completed",
    created_at: Date.now(),
    content: [{ id: "b2", type: "prose", markdown: ANSWER }],
    iterations: [
      {
        position: 0,
        thinking: "weighing it up",
        assistant_prose: ANSWER,
        forms: [{ block_id: "f1", code: 'done("...")' }],
      },
    ],
  };

  it("paints it once", async () => {
    renderSessionScreen({
      client: {
        cachedTranscript: () => [narratedRow],
        transcript: () => Promise.resolve([narratedRow]),
      },
    });
    expect(await screen.findByText("summarize the fix")).toBeInTheDocument();
    await linger(50);
    expect(painted(ANSWER)).toBe(1);
  });
});

// Regression, issue #145 in the carrier only the TUI normalized: a settled row that
// carries NO content blocks paints its answer from the LAST iteration's own
// `answer` (`fallbackAnswer`), while the trace beside it paints that iteration's
// `assistant_prose` — one answer at two widths again. The TUI promotes exactly this
// prose into a content block on the way in (`terminal-content`) and then drops the
// trace copy, so the app owes the same one-copy guarantee on the same shape.
describe("an answer promoted out of a content-less row", () => {
  const ANSWER = "The suite is green and the pin is unchanged.";

  function mountRow(row: Record<string, unknown>) {
    renderSessionScreen({
      client: {
        cachedTranscript: () => [row],
        transcript: () => Promise.resolve([row]),
      },
    });
  }

  const promotedRow = {
    id: "engine-row-3",
    user_request: "did anything else move",
    status: "completed",
    created_at: Date.now(),
    iterations: [
      {
        position: 0,
        thinking: "weighing it up",
        assistant_prose: ANSWER,
        answer: ANSWER,
      },
    ],
  };

  it("paints it once", async () => {
    mountRow(promotedRow);
    expect(
      await screen.findByText("did anything else move"),
    ).toBeInTheDocument();
    await linger(50);
    expect(painted(ANSWER)).toBe(1);
  });

  it("keeps commentary that only resembles the answer", async () => {
    const commentary = "The suite is green, and the pin is unchanged.";
    mountRow({
      ...promotedRow,
      id: "engine-row-4",
      iterations: [
        { position: 0, assistant_prose: commentary, answer: ANSWER },
      ],
    });
    expect(
      await screen.findByText("did anything else move"),
    ).toBeInTheDocument();
    await linger(50);
    expect(painted(commentary)).toBe(1);
    expect(painted(ANSWER)).toBe(1);
  });
});

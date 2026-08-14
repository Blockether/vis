// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { fireEvent, screen } from "@testing-library/react";

import { renderSessionScreen } from "./session-screen-harness";
import type { SseEvent } from "../lib/types";

function deferred<T>() {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((answer) => {
    resolve = answer;
  });
  return { promise, resolve };
}

const linger = (ms: number) => new Promise((done) => setTimeout(done, ms));

function live(): string {
  return document.querySelector('[data-live="true"]')?.textContent ?? "";
}

// Regression, reported from a phone: "sometimes when I send the message I just
// see Vis and nothing happens — the stream died, or never started — and I have
// to go back to the session list and open it again". The rail had settled the
// message that was still being POSTed: the terminal frame of the PREVIOUS turn
// claimed the optimistic bubble because that bubble carries no id until
// `submitTurn` answers, and a bubble that has stopped running drops every delta
// that arrives after it.
describe("the message just sent", () => {
  it("keeps streaming when the previous turn's terminal frame lands first", async () => {
    let emit: (event: SseEvent) => void = () => {};
    const posted = deferred<unknown>();

    renderSessionScreen({
      client: {
        transcript: () => Promise.resolve([]),
        // The POST is still on the wire, so the bubble on the rail has no id.
        submitTurn: () => posted.promise,
      },
      subscriptions: {
        subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
          emit = listener;
          return () => {};
        },
        subscribeConnection: (on: (connected: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });

    const box = await screen.findByLabelText("Message Vis");
    fireEvent.change(box, { target: { value: "run the tests" } });
    fireEvent.click(screen.getByRole("button", { name: "Send message" }));
    expect(await screen.findByText("run the tests")).toBeInTheDocument();

    emit({
      type: "turn.completed",
      turn_id: "the-previous-turn",
      seq: 10,
      status: "completed",
    } as unknown as SseEvent);
    await linger(300);

    expect(live()).toMatch(/Vis is/);
  });
});

// Regression, same report: "the turn finished — I saw it done — but on the
// transition between the live bubble and its persisted row the ANSWER (just the
// answer) was removed". Completion overtakes the 150 ms body queue, so the
// terminal frame's own `content` is regularly the whole answer; the handover
// guard sampled what the bubble held BEFORE that frame was applied, decided it
// carried no prose, and retired it against a persisted row that carried none
// either.
describe("a finished turn handed to its persisted row", () => {
  const bubble = {
    id: "gw-1",
    request: "explain the failure",
    answer: "",
    iterations: [{ position: 0, thinking: "weighing it up" }],
    startedAt: Date.now(),
    status: "running" as const,
  };
  // The engine writes the row before the answer block is flushed onto it: the
  // trace is already there, the answer is not.
  const proseFreeRow = {
    id: "engine-row-1",
    user_request: "explain the failure",
    status: "completed",
    created_at: Date.now(),
    content: [],
    iterations: [{ position: 0, thinking: "weighing it up" }],
  };

  it("keeps the answer the terminal frame itself delivered", async () => {
    let emit: (event: SseEvent) => void = () => {};
    renderSessionScreen({
      client: {
        cachedLiveTurn: () => ({ turn: bubble, seq: 5 }),
        cachedTranscript: () => [],
        transcript: () => Promise.resolve([proseFreeRow]),
      },
      subscriptions: {
        subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
          emit = listener;
          return () => {};
        },
      },
    });

    expect(await screen.findByText("explain the failure")).toBeInTheDocument();
    emit({
      type: "turn.completed",
      turn_id: "gw-1",
      seq: 11,
      status: "completed",
      content: [{ id: "b1", type: "prose", markdown: "THE FINAL ANSWER" }],
    } as unknown as SseEvent);

    expect(await screen.findByText("THE FINAL ANSWER")).toBeInTheDocument();
    // Long enough for the settle poll to have read the transcript and offered
    // that prose-free row as the replacement.
    await linger(600);
    expect(screen.queryByText("THE FINAL ANSWER")).not.toBeNull();
  });
});

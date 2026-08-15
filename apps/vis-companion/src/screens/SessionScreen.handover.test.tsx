// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { fireEvent, screen, waitFor } from "@testing-library/react";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";
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

// Regression, reported from an iPhone: "the stream finished, the answer is
// ready, but it is not showing — I have to go back to the session list and
// reopen the session". The terminal frame was lost with the suspended socket,
// so only the 5 s reconcile could retire the bubble — and it judged coverage
// ONLY on the tick that fetched a transcript page. The tick that did fetch was
// vetoed by a registry that still named the turn as current, every later tick
// revalidated to "nothing moved" and never asked again, and the stale bubble
// sat on top of the persisted answer until the screen was remounted.
describe("a handover the registry vetoed once", () => {
  const bubble = {
    id: "gw-9",
    request: "explain the failure",
    answer: "",
    iterations: [{ position: 0, thinking: "weighing it up" }],
    startedAt: Date.now(),
    status: "running" as const,
  };
  const answered = {
    id: "engine-row-9",
    user_request: "explain the failure",
    status: "done",
    created_at: Date.now(),
    content: [{ id: "b9", type: "prose", markdown: "THE FINAL ANSWER" }],
    iterations: [{ position: 0, thinking: "weighing it up" }],
  };

  it("retires the bubble on a later tick, with no new transcript page", async () => {
    let registryLive = true;
    let reads = 0;
    renderSessionScreen({
      client: {
        cachedLiveTurn: () => ({ turn: bubble, seq: 5 }),
        cachedTranscript: () => [],
        session: () =>
          Promise.resolve(
            sessionFixture(
              registryLive
                ? { live: true, current_turn_id: "gw-9" }
                : { live: false },
            ),
          ),
        // The transcript moves exactly once; every later revalidation answers
        // `null`, the way a session that has stopped moving does.
        transcriptIfMoved: () => {
          reads += 1;
          return Promise.resolve(reads === 1 ? [answered] : null);
        },
        transcript: () => Promise.resolve([answered]),
      },
    });

    expect(await screen.findByText("explain the failure")).toBeInTheDocument();

    // Wake once while the registry still claims the turn: the page lands, the
    // handover is vetoed.
    window.dispatchEvent(new Event("online"));
    await linger(500);
    registryLive = false;
    // …and again, now that the gateway agrees the turn is over.
    window.dispatchEvent(new Event("online"));
    await waitFor(() =>
      expect(document.querySelector('[data-live="true"]')).toBeNull(),
    );
    expect(screen.getByText("THE FINAL ANSWER")).toBeInTheDocument();
    expect(screen.getAllByText("explain the failure")).toHaveLength(1);
  });
});

// Regression, same report, the half the fix above did not reach: "I write a
// message on a NEW session and all I get back is the label Vis — no progress,
// no answer, nothing — until I go back to the session list and open the session
// again." The first POST of a session is the slowest one it will ever make, and
// a reconcile lands while it is still on the wire: the 5 s tick, or — on a phone
// — the wake an `online` burst fires the moment the radio settles. The registry
// answers "idle", truthfully, because nothing has asked it to run a turn yet,
// and that answer retired the bubble the composer had just painted. Frozen with
// nothing in it, that bubble IS the bare "Vis": `AssistantMessage` prints no
// phase, no clock and no placeholder for a `completed` turn, and every delta
// that arrives afterwards lands in a turn that has stopped running.
describe("a message whose POST is still on the wire", () => {
  it("survives a reconcile against a registry not yet asked to run it", async () => {
    const idle = sessionFixture({ status: "idle", live: false });
    const posted = deferred<unknown>();

    renderSessionScreen({
      session: idle,
      client: {
        // Every read this reconcile makes answers the way a gateway holding no
        // turn answers — because at this instant it holds none.
        session: () => Promise.resolve(idle),
        transcript: () => Promise.resolve([]),
        transcriptIfMoved: () => Promise.resolve([]),
        submitTurn: () => posted.promise,
      },
      subscriptions: {
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
    expect(live()).toMatch(/Vis is/);

    window.dispatchEvent(new Event("online"));
    await linger(400);

    // Still saying what it is doing. A rail that says only "Vis" is the bug.
    expect(live()).toMatch(/Vis is/);
  });
});

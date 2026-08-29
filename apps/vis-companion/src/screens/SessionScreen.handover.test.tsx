// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { fireEvent, screen, waitFor } from "@testing-library/react";

import {
  renderSessionScreen,
  sessionFixture,
  subscriptionHub,
} from "./session-screen-harness";
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
    const events = subscriptionHub();
    const posted = deferred<unknown>();

    renderSessionScreen({
      client: {
        transcript: () => Promise.resolve([]),
        // The POST is still on the wire, so the bubble on the rail has no id.
        submitTurn: () => posted.promise,
      },
      subscriptions: {
        subscribeSession: events.subscribeSession,
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

    events.emit({
      type: "turn.completed",
      turn_id: "the-previous-turn",
      seq: 10,
      status: "completed",
    } as unknown as SseEvent);
    await linger(300);

    // The rail must carry a SENTENCE, not a bare "Vis". With no phase marker
    // from the engine yet, the honest one names what this screen knows: sent.
    expect(live()).toMatch(/Vis sent your message/);
  });
});

// Regression, same report: a terminal answer disappeared while the running bubble
// handed over to its durable row. Protocol 8 makes both carriers the same canonical
// turn, so the durable row and terminal frame carry the same final prose.
describe("a finished turn handed to its persisted row", () => {
  const bubble = {
    id: "gw-1",
    request: "explain the failure",
    answer: "",
    iterations: [{ position: 0, thinking: "weighing it up" }],
    startedAt: Date.now(),
    status: "running" as const,
  };
  const settledRow = {
    turn_id: "gw-1",
    request: "explain the failure",
    status: "completed",
    created_at: Date.now(),
    content: [{ id: "b1", type: "prose", markdown: "THE FINAL ANSWER" }],
    iterations: [{ position: 0, thinking: "weighing it up" }],
  };

  it("keeps one canonical answer through terminal handover", async () => {
    const events = subscriptionHub();
    renderSessionScreen({
      client: {
        cachedRunningTurn: () => ({ turn: bubble, seq: 5 }),
        cachedTranscript: () => [],
        transcript: () => Promise.resolve([settledRow]),
      },
      subscriptions: {
        subscribeSession: events.subscribeSession,
      },
    });

    expect(await screen.findByText("explain the failure")).toBeInTheDocument();
    events.emit({
      type: "turn.completed",
      turn_id: "gw-1",
      seq: 11,
      status: "completed",
      content: [{ id: "b1", type: "prose", markdown: "THE FINAL ANSWER" }],
    } as unknown as SseEvent);

    expect(await screen.findByText("THE FINAL ANSWER")).toBeInTheDocument();
    // Long enough for the settle poll to swap in that same canonical row.
    await linger(600);
    expect(screen.queryByText("THE FINAL ANSWER")).not.toBeNull();
  });
});
// Regression, Vis session 976f705e-fd80-4787-adc6-1ae8388fdaa2: cancelling
// mounted a second loading status beneath the cancellation, so the live row grew
// for the handover and shrank again when its persisted row arrived.
describe("a turn cancelled from this screen", () => {
  it("keeps one stable cancellation status while the transcript catches up", async () => {
    const events = subscriptionHub();
    const persisted = deferred<never[]>();
    const bubble = {
      id: "gw-cancel",
      request: "stop this turn",
      answer: "",
      iterations: [],
      startedAt: Date.now(),
      status: "running" as const,
    };

    renderSessionScreen({
      client: {
        cachedRunningTurn: () => ({ turn: bubble, seq: 5 }),
        cachedTranscript: () => [],
        transcript: () => persisted.promise,
      },
      subscriptions: {
        subscribeSession: events.subscribeSession,
      },
    });

    expect(await screen.findByText("stop this turn")).toBeInTheDocument();
    const liveRow = document.querySelector('[data-live="true"]') as HTMLElement;
    const phaseSlot = liveRow.querySelector('[aria-hidden="true"].mt-5');
    expect(phaseSlot).not.toBeNull();
    fireEvent.click(screen.getByRole("button", { name: "Stop response" }));
    events.emit({
      type: "turn.cancelled",
      turn_id: "gw-cancel",
      seq: 6,
      status: "cancelled",
    } as unknown as SseEvent);

    expect(await screen.findAllByText("Cancelled by user.")).toHaveLength(2);
    expect(screen.queryByText("Loading latest changes")).toBeNull();
    const cancelledRow = document.querySelector(
      '[data-live="true"]',
    ) as HTMLElement;
    expect(cancelledRow).not.toBeNull();
    expect(cancelledRow.querySelector('[aria-hidden="true"].mt-5')).toBe(
      phaseSlot,
    );
    expect(cancelledRow.querySelector(".bg-answer")?.textContent).toBe("");
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
    turn_id: "gw-9",
    request: "explain the failure",
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
        cachedRunningTurn: () => ({ turn: bubble, seq: 5 }),
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
    expect(live()).toMatch(/Vis sent your message/);

    window.dispatchEvent(new Event("online"));
    await linger(400);

    // Still saying what it is doing. A rail that says only "Vis" is the bug.
    expect(live()).toMatch(/Vis sent your message/);
  });
});

// Regression, reported from the Companion app: a fully painted answer looked as if it
// were still incomplete while the internal persisted-row handover ran. The user cannot
// act on that bookkeeping, and the terminal frame already carries the whole answer.
describe("the wait for a finished turn's persisted row", () => {
  it("keeps an already complete answer free of handover loading furniture", async () => {
    const events = subscriptionHub();
    const bubble = {
      id: "gw-slow",
      request: "explain the failure",
      answer: "",
      iterations: [],
      // Two hours and change of real work, exactly as the report had it.
      startedAt: Date.now() - 137 * 60_000,
      status: "running" as const,
    };

    renderSessionScreen({
      client: {
        cachedRunningTurn: () => ({ turn: bubble, seq: 5 }),
        cachedTranscript: () => [],
        // The row this handover is waiting for never arrives.
        transcript: () => Promise.resolve([]),
      },
      subscriptions: {
        subscribeSession: events.subscribeSession,
      },
    });

    expect(await screen.findByText("explain the failure")).toBeInTheDocument();
    events.emit({
      type: "turn.completed",
      turn_id: "gw-slow",
      seq: 6,
      status: "completed",
      content: [{ id: "b1", type: "prose", markdown: "THE FINAL ANSWER" }],
    } as unknown as SseEvent);
    expect(await screen.findByText("THE FINAL ANSWER")).toBeInTheDocument();

    // The terminal frame is already the answer. Persisting its replacement row is
    // internal bookkeeping, not another user-visible loading phase.
    expect(screen.queryByText("Loading latest changes")).toBeNull();

    expect(screen.getByText("THE FINAL ANSWER")).toBeInTheDocument();
  });
});

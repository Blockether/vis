// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { act, screen, waitFor } from "@testing-library/react";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";
import activityFixture from "../lib/activity.fixture.json";
import { reduceRunningTurnEvent } from "../lib/running-turn";
import type { SseEvent } from "../lib/types";

// A transcript row is durable content, not a liveness lease. If the canonical
// session read fails, the client must not invent a running turn from stale SQL.
describe("a running transcript row without canonical session state", () => {
  const runningRow = {
    turn_id: "t1",
    request: "check the logs",
    status: "running",
    created_at: Date.now(),
    iterations: [],
  };

  it("renders the row as history without creating live work", async () => {
    renderSessionScreen({
      client: {
        session: () => Promise.reject(new Error("network down")),
        transcript: () => Promise.resolve([runningRow]),
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });

    expect(await screen.findByText("check the logs")).toBeInTheDocument();
    await waitFor(() =>
      expect(document.querySelector('[data-live="true"]')).toBeNull(),
    );
  });

  // Regression, issue reported in session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25:
  // opening a turn already at iteration 420 replayed its journal from iteration 1,
  // so the live ticker visibly counted through old work before reaching the present.
  it("paints the replay's latest iteration before its older frames", async () => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    const never = new Promise<never>(() => {});

    renderSessionScreen({
      session: sessionFixture({
        status: "running",
        live: true,
        current_turn_id: "t-live",
        running_request: "keep working",
      }),
      client: {
        cachedTranscript: () => [],
        transcript: () => never,
        turnTrace: () => never,
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (
          _sid: string,
          on: (event: Record<string, unknown>) => void,
        ) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });

    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(2));
    const emit = (event: Record<string, unknown>) => {
      for (const listener of listeners) listener(event);
    };

    act(() => {
      emit({
        type: "subscription.ready",
        session_id: "s1",
        current_turn_id: "t-live",
        is_live: true,
        latest_iteration: 420,
      });
      emit({
        type: "turn.started",
        session_id: "s1",
        turn_id: "t-live",
        request: "keep working",
        seq: 1,
      });
    });

    expect(
      (await screen.findAllByText(/Vis is working \(iter 420\)/)).length,
    ).toBeGreaterThan(0);

    act(() => {
      emit({
        type: "iteration.completed",
        session_id: "s1",
        turn_id: "t-live",
        iteration: 1,
        seq: 2,
      });
    });

    expect(screen.queryByText(/\(iter 1\)/)).toBeNull();
    expect(screen.getAllByText(/\(iter 420\)/).length).toBeGreaterThan(0);
  });


  it("keeps an already painted answer when its terminal arrived while away", async () => {
    const visible = {
      id: "gateway-turn",
      request: "current voice turn",
      answer: "This answer was already visible.",
      iterations: [],
      startedAt: Date.now(),
      status: "running" as const,
    };

    renderSessionScreen({
      client: {
        cachedRunningTurn: () => ({ turn: visible, seq: 42 }),
        cachedTranscript: () => [],
        // Keep the persisted handover pending for the duration of the assertion.
        transcript: () => new Promise(() => {}),
      },
      subscriptions: {
        hasEndedTurn: () => true,
      },
    });

    expect(
      await screen.findByText("This answer was already visible."),
    ).toBeInTheDocument();
    expect(screen.queryByText(/Vis is waiting for an update/)).toBeNull();
  });

  // Regression, same report: a submit paints an optimistic bubble with nothing
  // in it, and the hub still remembers the PREVIOUS turn's terminal frame. Seeding
  // that empty bubble as `completed` renders the assistant rail as a bare "Vis" —
  // no phase, no clock, no answer — for the whole turn.
  it("does not seed a bubble that never painted anything", async () => {
    const justSent = {
      request: "run the tests",
      answer: "",
      iterations: [],
      startedAt: Date.now(),
      status: "running" as const,
    };

    renderSessionScreen({
      session: sessionFixture({
        status: "running",
        live: true,
        current_turn_id: "t1",
        running_request: "check the logs",
      }),
      client: {
        cachedRunningTurn: () => ({ turn: justSent, seq: 7 }),
        cachedTranscript: () => [],
        transcript: () => Promise.resolve([runningRow]),
      },
      subscriptions: {
        hasEndedTurn: () => true,
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });

    // Canonical gateway state names the active turn; the empty optimistic bubble
    // cannot claim an older terminal frame.
    expect(await screen.findByText("check the logs")).toBeInTheDocument();
    expect(
      (await screen.findAllByText(/Vis sent your message/)).length,
    ).toBeGreaterThan(0);
  });
  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: with a CI watch on screen,
  // still read "Vis is thinking (iter 30)... 10m 1s" while the panel under it was
  // filling in and offering an Interrupt — a hang, in the one place the answer was.
  it("names the live panel instead of saying Vis is thinking", async () => {
    renderSessionScreen({
      session: sessionFixture({
        status: "running",
        live: true,
        current_turn_id: "t1",
        running_request: "check the logs",
      }),
      client: {
        transcript: () =>
          Promise.resolve([
            {
              ...runningRow,
              iterations: [{ position: 1, thinking: "weighing it up", forms: [] }],
            },
          ]),
        liveViews: () =>
          Promise.resolve([
            { id: "v1", title: "CI · run 42", description: "", nodes: [] },
          ]),
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });

    const phases = await screen.findAllByText(/Vis is showing CI · run 42 — live \(iter 1\)/);
    expect(phases.length).toBeGreaterThan(0);
    expect(screen.queryByText(/Vis is thinking/)).toBeNull();

    const title = screen
      .getAllByText('CI · run 42')
      .find((candidate) => candidate.closest('section')) as HTMLElement;
    const panel = title.closest('section') as HTMLElement;
    const phase = phases[0];
    expect(panel.compareDocumentPosition(phase) & Node.DOCUMENT_POSITION_FOLLOWING).not.toBe(0);
  });

  // Protocol 8 deleted the whole anchoring problem these cases guarded (td-65cdf6:
  // one Activity claimed by two rows, an anchor reused across turns, an unanchored
  // copy stranded in a detached rail). A snapshot is a field of the form that
  // produced it, so it cannot be claimed twice, placed wrongly, or orphaned. What
  // is left to prove is that the frames carrying it land on the right form.
  // The frames land through the real subscription, the same way the screen sees
  // them in production.
  const withLiveBlock = async (
    frames: (emit: (event: Record<string, unknown>) => void) => void,
  ) => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    renderSessionScreen({
      client: {
        cachedRunningTurn: () => ({
          turn: {
            id: "activity-turn",
            request: "inspect the run",
            answer: "",
            status: "running",
            startedAt: Date.now(),
            iterations: [
              {
                id: "iteration-41",
                position: 41,
                forms: [{ block_id: 0, source: "inspect_run()" }],
              },
            ],
          },
          seq: 42,
        }),
        transcript: () => Promise.resolve([]),
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (
          _sid: string,
          on: (event: Record<string, unknown>) => void,
        ) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });
    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(1));
    act(() => {
      frames((event) => {
        for (const listener of listeners) listener(event);
      });
    });
  };

  it("puts a running Activity snapshot on the block that produced it", async () => {
    await withLiveBlock((emit) => {
      emit({
        type: "block.activity",
        iteration: 41,
        form_index: 0,
        activity: activityFixture,
      });
    });

    expect(
      (await screen.findAllByRole("button", { name: "Expand execution trace" })).length,
    ).toBe(1);
  });

  it("replaces a running snapshot with the settled block.activity revision", async () => {
    await withLiveBlock((emit) => {
      emit({
        type: "block.activity",
        iteration: 41,
        form_index: 0,
        activity: activityFixture,
      });
      emit({
        type: "block.activity",
        iteration: 41,
        form_index: 0,
        activity: { ...activityFixture, state: "succeeded" },
      });
      emit({
        type: "block.output",
        iteration: 41,
        form_index: 0,
        code: "inspect_run()",
        result_summary: "done",
        duration_ms: 1_200,
      });
    });

    await waitFor(() => expect(screen.queryByText(/RUNNING · RUN_TESTS/)).toBeNull());
  });
});

describe("the wait between a submit and the first token", () => {
  it("says the message is sent, then names the model it waits on", async () => {
    const listeners = new Set<(event: Record<string, unknown>) => void>();
    const never = new Promise<never>(() => {});

    renderSessionScreen({
      session: sessionFixture({
        status: "running",
        live: true,
        current_turn_id: "t-live",
        running_request: "measure the wait",
      }),
      client: {
        cachedTranscript: () => [],
        transcript: () => never,
        turnTrace: () => never,
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
        subscribeSession: (
          _sid: string,
          on: (event: Record<string, unknown>) => void,
        ) => {
          listeners.add(on);
          return () => listeners.delete(on);
        },
      },
    });

    await waitFor(() => expect(listeners.size).toBeGreaterThanOrEqual(2));
    const emit = (event: Record<string, unknown>) => {
      for (const listener of listeners) listener(event);
    };

    act(() => {
      emit({
        type: "subscription.ready",
        session_id: "s1",
        current_turn_id: "t-live",
        is_live: true,
      });
      emit({
        type: "turn.started",
        session_id: "s1",
        turn_id: "t-live",
        request: "measure the wait",
        seq: 1,
      });
    });

    // Nothing has come back yet, and the one thing this screen knows for certain
    // is that the message left.
    expect(
      (await screen.findAllByText(/Vis sent your message/)).length,
    ).toBeGreaterThan(0);
    expect(screen.queryByText(/Vis is waiting for an update/)).toBeNull();

    act(() => {
      emit({
        type: "turn.progress",
        session_id: "s1",
        turn_id: "t-live",
        progress: "provider-call",
        iteration: 1,
        reason: "user-submit",
        model: "claude-opus-5",
        seq: 2,
      });
    });

    expect(
      (await screen.findAllByText(/Vis is calling claude-opus-5/)).length,
    ).toBeGreaterThan(0);
  });
});

// Regression: `form_index` is a number, and the reducer once read the form
// coordinate with a string-only helper. Every frame then saw no owner, and a second
// form carrying nothing but Activity was painted under the code block.
describe("a form frame's numeric form_index", () => {
  const frame = (event: Record<string, unknown>) => event as unknown as SseEvent;

  it("puts the running snapshot on the block that is already there", () => {
    const started = reduceRunningTurnEvent(
      reduceRunningTurnEvent(null, frame({ type: "turn.started", turn_id: "t-block" })),
      frame({ type: "block.started", iteration: 1, form_index: 0, code: "grep()" }),
    );
    const turn = reduceRunningTurnEvent(
      started,
      frame({
        type: "block.activity",
        iteration: 1,
        form_index: 0,
        activity: activityFixture,
      }),
    );

    const forms = turn?.iterations[0]?.forms ?? [];
    expect(forms).toHaveLength(1);
    expect(forms[0].code).toBe("grep()");
    expect(forms[0].activity?.state).toBe("running");
  });
});

// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { act, fireEvent, screen, waitFor } from "@testing-library/react";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";
import activityFixture from "../lib/activity.fixture.json";

// Reported from a phone: the message was sent and the session title updated, but
// the answer rail stayed a bare "Vis" — no phase, no clock, no trace — for the
// whole turn. A running turn was adopted ONLY from the session read, so that one
// failed request left `running` false and the live bubble null: the freshly
// persisted `running` row was then painted as a finished one, and every delta
// that arrived had no bubble to land in.
describe("a running turn the session read cannot confirm", () => {
  const runningRow = {
    id: "t1",
    user_request: "check the logs",
    status: "running",
    created_at: Date.now(),
    iterations: [],
  };

  it("reports the work the transcript says is under way", async () => {
    renderSessionScreen({
      client: {
        // The registry read is the request that fails on a phone's link; the
        // transcript is the witness that does not go through the registry.
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
    expect(
      (await screen.findAllByText(/Vis is waiting for an update/)).length,
    ).toBeGreaterThan(0);
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

  it("does not hand painted output to a matching but still empty settled row", async () => {
    const now = Date.now();
    const visible = {
      id: "gateway-turn",
      request: "current voice turn",
      answer: "This answer must never blink away.",
      iterations: [],
      startedAt: now,
      status: "completed" as const,
    };
    const emptySettledRow = {
      id: "gateway-turn",
      user_request: "current voice turn",
      status: "completed",
      created_at: now,
      content: [],
      iterations: [],
    };

    renderSessionScreen({
      client: {
        cachedLiveTurn: () => ({ turn: visible, seq: 42 }),
        cachedTranscript: () => [emptySettledRow],
        transcript: () => new Promise(() => {}),
      },
      subscriptions: {
        hasEndedTurn: () => true,
      },
    });

    expect(
      await screen.findByText("This answer must never blink away."),
    ).toBeInTheDocument();
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
        cachedLiveTurn: () => ({ turn: visible, seq: 42 }),
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
      client: {
        cachedLiveTurn: () => ({ turn: justSent, seq: 7 }),
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

    // The transcript's own running row is what says there is work, and it says
    // so out loud instead of leaving the reader a nameless "Vis".
    expect(await screen.findByText("check the logs")).toBeInTheDocument();
    expect(
      (await screen.findAllByText(/Vis is waiting for an update/)).length,
    ).toBeGreaterThan(0);
  });
  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: with a CI watch on screen,
  // still read "Vis is thinking (iter 30)... 10m 1s" while the panel under it was
  // filling in and offering an Interrupt — a hang, in the one place the answer was.
  it("names the live panel instead of saying Vis is thinking", async () => {
    renderSessionScreen({
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

  function activityScenario(settled = false) {
    const activity = {
      ...activityFixture,
      id: "live-activity-view",
      is_settled: settled,
      activity: {
        ...activityFixture.activity,
        anchor: { evaluation_id: "evaluation-1", iteration: 41, form_index: 0 },
      },
    };
    const filedRow = {
      id: "activity-turn",
      user_request: "inspect the run",
      status: "running",
      created_at: Date.now(),
      iterations: [
        {
          id: "iteration-41",
          position: 41,
          forms: [{ source: "inspect_run()", result_summary: "done" }],
          attachments: [
            {
              index: 0,
              iteration_id: "iteration-41",
              view_id: "activity-view",
              classification: "activity",
              activity_anchor: {
                evaluation_id: "evaluation-1",
                iteration: 41,
                form_index: 0,
              },
              kind: "file",
              media_type: "application/vnd.vis.live+ndjson",
              filename: "activity.live.ndjson",
            },
          ],
        },
      ],
    };
    return { activity, filedRow };
  }

  // Regression, issue td-65cdf6: while the optimistic row overlapped its persisted
  // receipt, iOS painted Activity in both rows and placed the anchored copy between
  // PYTHON and RESULT. Android exposed only the detached copy from its older bundle.
  it("keeps one live Activity after its filed Python result during handoff", async () => {
    const { activity, filedRow } = activityScenario();

    renderSessionScreen({
      client: {
        cachedLiveTurn: () => ({
          turn: {
            id: "activity-turn",
            request: "inspect the run",
            answer: "",
            status: "running",
            startedAt: Date.now(),
            iterations: filedRow.iterations,
          },
          seq: 42,
        }),
        transcript: () => Promise.resolve([filedRow]),
        liveViews: () => Promise.resolve([activity]),
      },
    });

    await screen.findByText(/RUN_TESTS · suite/);
    expect(screen.getAllByRole("button", { name: "Expand execution trace" })).toHaveLength(1);
    fireEvent.click(screen.getByRole("button", { name: "Expand execution trace" }));
    const panelLabel = await screen.findByText("ACTIVITY");
    const python = screen.getAllByText("inspect_run()")[0]!;
    const result = screen.getAllByText("done")[0]!;
    expect(screen.getAllByText("ACTIVITY")).toHaveLength(1);
    expect(screen.queryByText("Loading Activity…")).toBeNull();
    expect(
      python.compareDocumentPosition(result) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).not.toBe(0);
    expect(
      result.compareDocumentPosition(panelLabel) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).not.toBe(0);
  });

  // Regression, issue td-65cdf6: a settled Activity already owned by its filed
  // receipt was also reused in the next optimistic row when anchor coordinates reset.
  it("does not reuse a filed Activity in the next optimistic row", async () => {
    const { activity, filedRow } = activityScenario(true);
    filedRow.status = "completed";
    const nextRow = {
      id: "next-turn",
      request: "continue",
      answer: "",
      status: "running" as const,
      startedAt: Date.now(),
      iterations: filedRow.iterations.map((iteration) => ({
        ...iteration,
        id: "next-iteration-41",
        forms: [{ source: "next_step()", result_summary: "next done" }],
        attachments: [],
      })),
    };

    renderSessionScreen({
      client: {
        cachedLiveTurn: () => ({ turn: nextRow, seq: 43 }),
        transcript: () => Promise.resolve([filedRow]),
        liveViews: () => Promise.resolve([activity]),
      },
    });

    await screen.findByText(/RUN_TESTS · suite/);
    const receipts = screen.getAllByRole("button", { name: "Expand execution trace" });
    expect(receipts).toHaveLength(2);
    receipts.forEach((receipt) => fireEvent.click(receipt));
    const filedPython = await screen.findByText("inspect_run()");
    const nextPython = await screen.findByText("next_step()");
    const panel = screen.getAllByText("ACTIVITY")[0]!;
    expect(screen.getAllByText("ACTIVITY")).toHaveLength(1);
    expect(
      filedPython.compareDocumentPosition(panel) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).not.toBe(0);
    expect(
      panel.compareDocumentPosition(nextPython) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).not.toBe(0);
  });
  it("keeps a genuinely unanchored Activity in the detached fallback", async () => {
    const activity = {
      ...activityFixture,
      id: "unanchored-activity",
      activity: {
        ...activityFixture.activity,
        anchor: { evaluation_id: "evaluation-2", iteration: 99, form_index: 0 },
      },
    };

    renderSessionScreen({
      client: {
        transcript: () => Promise.resolve([]),
        liveViews: () => Promise.resolve([activity]),
      },
    });

    expect(await screen.findByText("ACTIVITY")).toBeInTheDocument();
  });
});

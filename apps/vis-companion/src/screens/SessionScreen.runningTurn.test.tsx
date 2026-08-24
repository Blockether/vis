// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { screen } from "@testing-library/react";

import { renderSessionScreen } from "./session-screen-harness";
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

  // Regression, issue td-65cdf6: a production-position Activity could paint both
  // at its Python anchor and again in the detached band during row handoff.
  it("keeps one live Activity in its filed Python slot during handoff", async () => {
    const activity = {
      ...activityFixture,
      id: "activity-view",
      activity: {
        ...activityFixture.activity,
        anchor: { evaluation_id: "evaluation-1", iteration: 41, form_index: 0 },
      },
    };
    const filedRow = {
      id: "activity-turn",
      user_request: "inspect the run",
      status: "completed",
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

    renderSessionScreen({
      client: {
        transcript: () => Promise.resolve([filedRow]),
        liveViews: () => Promise.resolve([activity]),
      },
    });

    const panelLabel = await screen.findByText("ACTIVITY");
    const python = screen.getByText("inspect_run()");
    const result = screen.getByText("done");
    expect(screen.getAllByText("ACTIVITY")).toHaveLength(1);
    expect(screen.queryByText("Loading Activity…")).toBeNull();
    expect(
      python.compareDocumentPosition(panelLabel) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).not.toBe(0);
    expect(
      panelLabel.compareDocumentPosition(result) & Node.DOCUMENT_POSITION_FOLLOWING,
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

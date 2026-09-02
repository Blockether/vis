// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { fireEvent, screen } from "@testing-library/react";

import {
  renderSessionScreen,
  sessionFixture,
  subscriptionHub,
} from "./session-screen-harness";
import type { SseEvent } from "../lib/types";

const HISTORY = 9;

function completedRows() {
  return Array.from({ length: HISTORY }, (_unused, index) => ({
    turn_id: `old-${index}`,
    request: `older question ${index}`,
    status: "completed",
    created_at: 1_700_000_000_000 + index,
    content: [{ id: `b${index}`, type: "prose", markdown: `older answer ${index}` }],
    iterations: [],
  }));
}

/** Every history question currently mounted, oldest first. */
function paintedQuestions(): string[] {
  return Array.from({ length: HISTORY }, (_unused, index) => `older question ${index}`)
    .filter((question) => screen.queryByText(question) !== null);
}

afterEach(() => {
  vi.useRealTimers();
});

// Regression, reported from a phone: "about five or six seconds after I send a
// message the screen flickers, as if it remounted". A running turn is ALSO
// persisted as a bare 'running' row, which the 5s reconcile read back; the row is
// never painted — the running-turn bubble owns it — but it still counted against
// the render window, so the oldest turn on screen was unmounted, its markdown
// re-parsed, and the transcript lost that turn's pixels under a reader pinned to
// the end.
describe("the transcript while the turn you just sent starts", () => {
  it("keeps every painted turn when the running placeholder row lands", async () => {
    vi.useFakeTimers({ shouldAdvanceTime: true });
    const events = subscriptionHub();
    const rows = completedRows();
    const idle = sessionFixture({ id: "s1", status: "idle", live: false } as never);
    const live = sessionFixture({
      id: "s1",
      status: "running",
      live: true,
      current_turn_id: "gw-1",
      running_request: "run the tests",
    } as never);
    const placeholder = {
      turn_id: "gw-1",
      request: "run the tests",
      status: "running",
      created_at: 1_700_000_100_000,
      content: [],
      iterations: [],
    };
    let started = false;

    renderSessionScreen({
      session: idle,
      client: {
        cachedTranscript: () => rows,
        transcript: () => Promise.resolve(rows),
        session: () => Promise.resolve(started ? live : idle),
        // What the gateway answers once it has persisted the running turn.
        transcriptIfMoved: () =>
          Promise.resolve(started ? [...rows, placeholder] : null),
        submitTurn: () => Promise.resolve({ turn_id: "gw-1", status: "running" }),
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
    await vi.advanceTimersByTimeAsync(50);
    started = true;
    events.emit({
      type: "turn.started",
      turn_id: "gw-1",
      seq: 1,
      request: "run the tests",
      started_at: Date.now(),
    } as unknown as SseEvent);
    await vi.advanceTimersByTimeAsync(150);

    const painted = paintedQuestions();
    expect(painted.length).toBeGreaterThan(1);
    const oldest = screen.getByText(painted[0]);

    // The reconcile tick, with the placeholder row now in the transcript.
    await vi.advanceTimersByTimeAsync(5_300);

    expect(paintedQuestions()).toEqual(painted);
    // The same nodes, not re-created ones: a remounted turn re-parses its
    // markdown and repaints the screen the reader is looking at.
    expect(screen.getByText(painted[0])).toBe(oldest);
    // And the turn being sent is still painted exactly once.
    expect(screen.getAllByText("run the tests")).toHaveLength(1);
  });
});

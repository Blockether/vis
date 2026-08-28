// @vitest-environment jsdom
import { act, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import type { SseEvent } from "../lib/types";
import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

/** Let every poll, repaint and effect that fits inside `ms` happen. */
const settle = async (ms = 0) => {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
};

// The read this list owns is the fleet window — the one WITHOUT a `root=`, since a
// project's page is a read of its own (`GatewayClient.listProjectPage`).
const listReads = (requests: { path: string }[]) =>
  requests.filter(
    (request) => request.path.startsWith("/v1/sessions?") && !request.path.includes("root="),
  ).length;

/**
 * A hub whose fleet stream is delivering: the frames the gateway sends on
 * `GET /v1/events?scope=fleet`, and the streaming state the list paces itself by.
 */
function fleetHub() {
  let deliver: ((event: SseEvent) => void) | null = null;
  let report: ((streaming: boolean) => void) | null = null;
  return {
    hub: {
      subscribeFleet(listener: (event: SseEvent) => void) {
        deliver = listener;
        return () => {
          deliver = null;
        };
      },
      subscribeFleetState(listener: (streaming: boolean) => void) {
        report = listener;
        listener(true);
        return () => {
          report = null;
        };
      },
    },
    emit: async (event: SseEvent) => {
      await act(async () => {
        deliver?.(event);
      });
    },
    /** The stream dropped (or came back): the list's safety net changes cadence. */
    streaming: async (live: boolean) => {
      await act(async () => {
        report?.(live);
      });
    },
  };
}

let restore = () => {};

// Regression, user report in this Vis session (paraphrased: "the companion is slow,
// it keeps refreshing and struggling to see over the network"): the ONLY way this list
// learned that a run had started, parked on a human or ended was to re-read its whole
// window every five seconds — a payload whose ETag any one active session invalidates,
// so a phone paid the window over and over to discover a single boolean.
describe("a session list carried by the fleet stream", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    restore();
    vi.useRealTimers();
  });

  const oneRow = (fleet: ReturnType<typeof fleetHub>) =>
    renderSessionsScreen({
      machines: [{ sessions: [listSession({ id: "s1", title: "First" })] }],
      subscriptions: fleet.hub as never,
    });

  it("paints a run the stream announced without reading the window again", async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);
    expect(screen.getByText("First")).toBeTruthy();
    expect(screen.queryByText("LIVE")).toBeNull();
    const read = listReads(view.requests);

    await fleet.emit({
      type: "session.status",
      session_id: "s1",
      is_live: true,
      is_awaiting_input: false,
      current_turn_id: "t1",
    });

    expect(screen.getByText("LIVE")).toBeTruthy();
    expect(listReads(view.requests)).toBe(read);
  });

  // Regression, Vis session 448b3266-8836-4115-9cf5-6ed0679aa2f9: a settled fleet
  // frame painted NEW from metadata alone, before the finished transcript was warm.
  it("reads the settled row before replacing LIVE with its finished state", async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);

    await fleet.emit({
      type: "session.status",
      session_id: "s1",
      is_live: true,
      is_awaiting_input: false,
      current_turn_id: "t1",
    });
    const read = listReads(view.requests);

    await fleet.emit({
      type: "session.status",
      session_id: "s1",
      is_live: false,
      is_awaiting_input: false,
      current_turn_id: null,
    });
    await settle(200);

    expect(listReads(view.requests)).toBeGreaterThan(read);
  });

  it("takes a row's new title from the frame alone", async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    await fleet.emit({
      type: "session.title_updated",
      session_id: "s1",
      title: "Renamed by the engine",
    });

    expect(screen.getByText("Renamed by the engine")).toBeTruthy();
    expect(listReads(view.requests)).toBe(read);
  });

  // A frame about a session this window does not hold is news about MEMBERSHIP, and
  // where that row belongs is the gateway's arithmetic, never this device's.
  it("re-reads the window for a session it does not hold", async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    await fleet.emit({
      type: "session.status",
      session_id: "somewhere-else",
      is_live: true,
      is_awaiting_input: false,
      current_turn_id: "t9",
    });
    await settle(200);

    expect(listReads(view.requests)).toBeGreaterThan(read);
  });

  it("slows its safety net while the stream delivers, and speeds back up when it drops", async () => {
    const fleet = fleetHub();
    const view = oneRow(fleet);
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    // The five-second reachability poll is what the stream replaces.
    await settle(5_000);
    expect(listReads(view.requests)).toBe(read);

    // The net is still there, just slack.
    await settle(25_000);
    expect(listReads(view.requests)).toBe(read + 1);

    // No stream, no slack: a dead gateway must not keep looking alive for half a minute.
    await fleet.streaming(false);
    await settle(5_000);
    expect(listReads(view.requests)).toBe(read + 2);
  });
});

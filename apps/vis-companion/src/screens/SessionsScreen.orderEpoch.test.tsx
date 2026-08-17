// @vitest-environment jsdom
import { act, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

// Regression, user report (paraphrased: "the non-determinism of sorting in the app
// drives me mad — I click something and suddenly it is the freshest thing and goes
// to the top; the list keeps jumping around like mad"). The ordering key is content
// time now and no band lifts a running session any more, so this device cannot move
// a row — but a turn landing on ANOTHER machine still reordered the rows under the
// reading thumb on the next ten-second poll, and a session created elsewhere was
// inserted above what was being read.

let restore = () => {};

const rowOrder = () =>
  Array.from(document.querySelectorAll<HTMLElement>("[data-session-id]")).map(
    (row) => row.dataset.sessionId,
  );

const at = (hour: number) => new Date(Date.UTC(2024, 4, 1, hour, 0, 0)).toISOString();

const row = (id: string, hour: number) =>
  listSession({
    id,
    title: `Session ${id}`,
    modified_at: at(hour),
    workspace: { root: "/Users/dev/project" },
  });

describe("the order the reader is looking at", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    restore();
    vi.useRealTimers();
  });

  /** Let every poll, repaint and effect that fits inside `ms` happen. */
  const settle = async (ms = 0) => {
    await act(async () => {
      await vi.advanceTimersByTimeAsync(ms);
    });
  };

  it("does not move a row a turn on another machine just made the freshest", async () => {
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", sessions: [row("a1", 12), row("a2", 11), row("a3", 10)] }],
    });
    restore = view.restore;
    await settle(50);
    expect(rowOrder()).toEqual(["a1", "a2", "a3"]);

    // A turn lands on the deepest row: the gateway is right to answer it first.
    view.setRows(0, [row("a3", 18), row("a1", 12), row("a2", 11)]);
    await settle(10_000);

    // The rows are the reader's, in the reader's order, and nothing is waiting:
    // every row the answer promoted is already on screen.
    expect(rowOrder()).toEqual(["a1", "a2", "a3"]);
    expect(screen.queryByRole("button", { name: /newer session/ })).toBeNull();
  });

  it("holds a session created elsewhere behind a count, and lands it on the tap", async () => {
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", sessions: [row("a1", 12), row("a2", 11)] }],
    });
    restore = view.restore;
    await settle(50);
    expect(rowOrder()).toEqual(["a1", "a2"]);

    view.setRows(0, [row("new-1", 20), row("new-2", 19), row("a1", 12), row("a2", 11)]);
    await settle(10_000);

    // Two arrivals fresher than everything on screen: counted above the list, not
    // inserted under the thumb.
    expect(rowOrder()).toEqual(["a1", "a2"]);
    const pill = screen.getByRole("button", { name: "Show 2 newer sessions" });
    expect(pill.textContent).toContain("2 newer sessions");

    await act(async () => {
      pill.click();
    });
    expect(rowOrder()).toEqual(["new-1", "new-2", "a1", "a2"]);
    expect(screen.queryByRole("button", { name: /newer session/ })).toBeNull();
  });

  it("appends a row deeper than everything held, because that is the next page", async () => {
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", sessions: [row("a1", 12), row("a2", 11)] }],
    });
    restore = view.restore;
    await settle(50);

    view.setRows(0, [row("a1", 12), row("a2", 11), row("older", 4)]);
    await settle(10_000);

    expect(rowOrder()).toEqual(["a1", "a2", "older"]);
    expect(screen.queryByRole("button", { name: /newer session/ })).toBeNull();
  });

  it("comes back to what is current after a real absence", async () => {
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", sessions: [row("a1", 12), row("a2", 11)] }],
    });
    restore = view.restore;
    await settle(50);

    view.setRows(0, [row("new-1", 20), row("a1", 12), row("a2", 11)]);
    await settle(10_000);
    expect(rowOrder()).toEqual(["a1", "a2"]);

    // Away for two minutes, then foreground again: a glance at a notification is not
    // this (`WakeInfo.awayMs`), and the list a reader comes back to is the current one.
    window.dispatchEvent(new Event("pagehide"));
    await settle(120_000);
    await act(async () => {
      window.dispatchEvent(new Event("pageshow"));
      await vi.advanceTimersByTimeAsync(1_000);
    });

    expect(rowOrder()).toEqual(["new-1", "a1", "a2"]);
    expect(screen.queryByRole("button", { name: /newer session/ })).toBeNull();
  });
});

// @vitest-environment jsdom
import { act, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

// The fleet effects are the seam: `syncBadge` is keyed on the machines array itself, so
// it runs again exactly when — and only when — that array is replaced.
const badge = vi.hoisted(() => ({
  syncBadge: vi.fn(() => Promise.resolve()),
  reassertBadge: vi.fn(() => Promise.resolve()),
}));
vi.mock("../lib/badge", () => badge);

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};

// Regression, user report (paraphrased: "entering the session list makes it flicker, and
// it keeps fetching page after page of sessions"): every poll patched its machine even
// when the gateway answered with the rows already on screen, so the fleet array was
// replaced ten seconds apart forever — and with it the scope filter, the sort, the
// project grouping and the pager built from it, under a reader who was only reading.
describe("a poll that changes nothing leaves the list alone", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    badge.syncBadge.mockClear();
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

  const listReads = (requests: { path: string }[]) =>
    requests.filter((request) => request.path.startsWith("/v1/sessions?")).length;

  it("keeps the fleet it already has when the poll answers with the same rows", async () => {
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", sessions: [listSession({ id: "a1", title: "First" })] }],
    });
    restore = view.restore;
    await settle(50);
    expect(screen.getByText("First")).toBeTruthy();

    const painted = badge.syncBadge.mock.calls.length;
    const read = listReads(view.requests);

    await settle(10_000);

    // The poll really ran...
    expect(listReads(view.requests)).toBeGreaterThan(read);
    // ...and said nothing new, so the fleet the whole list is derived from is the
    // very array it was before.
    expect(badge.syncBadge.mock.calls.length).toBe(painted);
    expect(screen.getByText("First")).toBeTruthy();
  });
});

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

import { forgetListScroll, parkedListScroll, rememberListScroll } from "../lib/list-scroll";

/** Let every poll, repaint and effect that fits inside `ms` happen. */
const settle = async (ms = 0) => {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
};

// A project's page is a read of ITS own now (`GatewayClient.listProjectPage`), so
// the FLEET read — the one this poll owns — is the one without a `root=`.
const listReads = (requests: { path: string }[]) =>
  requests.filter(
    (request) =>
      request.path.startsWith("/v1/sessions?") && !request.path.includes("root="),
  ).length;
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

  it("keeps the fleet it already has when the poll answers with the same rows", async () => {
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", sessions: [listSession({ id: "a1", title: "First" })] }],
    });
    restore = view.restore;
    await settle(50);
    expect(screen.getByText("First")).toBeTruthy();

    const painted = badge.syncBadge.mock.calls.length;
    const read = listReads(view.requests);

    await settle(4_900);
    expect(listReads(view.requests)).toBe(read);
    await settle(100);

    // The five-second reachability poll really ran...
    expect(listReads(view.requests)).toBeGreaterThan(read);
    // ...and said nothing new, so the fleet the whole list is derived from is the
    // very array it was before.
    expect(badge.syncBadge.mock.calls.length).toBe(painted);
    expect(screen.getByText("First")).toBeTruthy();
  });

  it("keeps polling while an iOS webview reports the document hidden", async () => {
    vi.spyOn(document, "visibilityState", "get").mockReturnValue("hidden");
    const view = renderSessionsScreen({
      machines: [{ label: "RBI", sessions: [listSession({ id: "a1", title: "First" })] }],
    });
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    await settle(5_000);

    expect(listReads(view.requests)).toBeGreaterThan(read);
  });
});

// Regression, user report (paraphrased: "I come back to the app in a session view, and
// leaving it flickers while the session list loads — we already know that list, so hold
// it while I am reading the session"): the fleet's FIRST read was gated on this screen
// being visible, so a relaunch straight into a transcript left the list mounted on
// nothing but its skeleton, and its rows only started arriving on the frame the reader
// pressed Back on.
describe("the list parked behind an open session", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    restore();
    forgetListScroll();
    vi.useRealTimers();
  });

  const parkedList = () =>
    renderSessionsScreen({
      isVisible: false,
      machines: [{ label: "alpha", sessions: [listSession({ id: "a1", title: "First" })] }],
    });

  it("reads its rows while nobody is looking at it", async () => {
    const view = parkedList();
    restore = view.restore;

    await settle(50);

    expect(listReads(view.requests)).toBe(1);
    // What the reader arrives on is the list itself, never its loading state.
    expect(screen.getByText("First")).toBeTruthy();
    expect(screen.queryByLabelText("Loading sessions")).toBeNull();
  });

  it("reads once and leaves the poll to the screen on the glass", async () => {
    const view = parkedList();
    restore = view.restore;
    await settle(50);
    expect(listReads(view.requests)).toBe(1);

    await settle(30_000);

    expect(listReads(view.requests)).toBe(1);
  });

  it("asks for nothing when it already has rows to paint", async () => {
    const view = renderSessionsScreen({
      machines: [{ label: "alpha", sessions: [listSession({ id: "a1", title: "First" })] }],
    });
    restore = view.restore;
    await settle(50);
    const read = listReads(view.requests);

    view.setVisible(false);
    await settle(50);

    expect(listReads(view.requests)).toBe(read);
  });

  it("keeps the reading position it has nowhere to put back yet", async () => {
    rememberListScroll({ top: 900, anchor: { id: "a1", offset: 0 } });
    const view = parkedList();
    restore = view.restore;

    await settle(50);

    // The rows landed off the glass, where the scroller has no height to restore into.
    // Spending the mark there costs the reader their place on the frame they come back on.
    expect(parkedListScroll()?.top).toBe(900);
  });
});

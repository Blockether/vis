// @vitest-environment jsdom
import { screen } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// Regression, user report (paraphrased: switching between gateways flickered the
// project list and then the numbers in it): the counts were a tally of the session
// windows this device had paged in, so a project of 400 read `1` until the whole
// list had drained, and re-derived itself on every gateway switch.
describe("what a project header counts", () => {
  it("gets rows and stable gateway totals in one list request", async () => {
    const overview = {
      projects: [
        {
          root: "/Users/dev/project",
          project_id: "p-a",
          name: "project",
          session_count: 400,
          live_count: 3,
          awaiting_count: 1,
          last_activity_ms: 1,
        },
      ],
      project_count: 1,
      session_count: 400,
      live_count: 3,
      awaiting_count: 1,
    };
    const view = renderSessionsScreen({
      machines: [
        {
          label: "alpha",
          sessions: [listSession({ id: "a1", title: "First" })],
          routes: {
            "/v1/sessions": {
              sessions: [listSession({ id: "a1", title: "First" })],
              total: 1,
              has_more: false,
              overview,
            },
          },
        },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");

    // One row is on screen; the header still says what the project holds.
    expect(await screen.findByText("400 sessions")).toBeTruthy();
    expect(screen.getAllByText(/2 live/).length).toBeGreaterThan(0);
    expect(screen.getAllByText(/1 needs input/).length).toBeGreaterThan(0);
    // One list read, and a project's own page is the only other (`listProjectPage`).
    expect(
      view.requests.filter(
        ({ path }) => path.startsWith("/v1/sessions?") && !path.includes("root="),
      ),
    ).toHaveLength(1);
    expect(view.requests.some(({ path }) => path === "/v1/projects/overview")).toBe(false);
  });
});

// Regression, measured against a 1192-session machine: opening the list drained the
// WHOLE fleet — twelve serial windows per machine per poll, ~315 KB of rows re-cut
// into a page of ten — and every project header counted the part that had landed so
// far. The head window is all this device reads now, the totals ride beside it, and a
// project's own page is asked for by the group that paints it.
describe("a fleet far deeper than one window", () => {
  it("costs one list read per machine, and counts what the gateway holds", async () => {
    const deep = (prefix: string, count: number, perProject: number) =>
      Array.from({ length: count }, (_, index) =>
        listSession({
          id: `${prefix}-${index}`,
          title: `${prefix} ${index}`,
          workspace: { root: `/Users/dev/${prefix}-p${Math.floor(index / perProject)}` },
          modified_at: new Date(Date.UTC(2024, 4, 1, 0, 0, count - index)).toISOString(),
        }),
      );
    const view = renderSessionsScreen({
      machines: [
        { label: "alpha", sessions: deep("alpha", 1200, 150) },
        { label: "beta", sessions: deep("beta", 30, 30) },
      ],
    });
    restore = view.restore;
    await screen.findByText("alpha 0");

    // Every project count is the gateway's own, not a tally of the rows that landed.
    expect(screen.getAllByText("150 sessions").length).toBeGreaterThan(0);
    // Every project the machine holds has a band, whether or not a row of it was
    // in the window: eight of alpha's, however deep the last one sits.
    expect(view.getByLabelText("Expand alpha-p7")).toBeTruthy();

    // ONE fleet read per machine, for the head window alone — no `after`, no walk.
    const fleetReads = view.requests.filter(
      ({ path }) => path.startsWith("/v1/sessions?") && !path.includes("root="),
    );
    expect(fleetReads).toHaveLength(2);
    expect(fleetReads.every(({ path }) => !path.includes("after="))).toBe(true);
    expect(new Set(fleetReads.map(({ machine }) => machine)).size).toBe(2);
  });
});

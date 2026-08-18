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
  it("reports what the GATEWAY holds, not the rows this device has paged in", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          label: "alpha",
          sessions: [listSession({ id: "a1", title: "First" })],
          routes: {
            "/v1/projects/overview": {
              projects: [
                {
                  root: "/Users/dev/project",
                  project_id: "p-a",
                  name: "project",
                  session_count: 400,
                  live_count: 3,
                  awaiting_count: 0,
                  last_activity_ms: 1,
                },
              ],
              project_count: 1,
              session_count: 400,
              live_count: 3,
              awaiting_count: 0,
            },
          },
        },
      ],
    });
    restore = view.restore;
    await screen.findByText("First");

    // One row is on screen; the header still says what the project holds.
    expect(await screen.findByText("400 sessions")).toBeTruthy();
    expect(screen.getAllByText(/3 live/).length).toBeGreaterThan(0);
  });
});

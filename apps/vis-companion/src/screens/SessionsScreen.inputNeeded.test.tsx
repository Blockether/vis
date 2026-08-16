// @vitest-environment jsdom
import { screen } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// A run PARKED on an unanswered human-input request is still LIVE and still
// streams nothing: the list painted it as a session getting on with the job,
// and the one place the operator counts what needs them said nothing at all.
describe("a session waiting on a human", () => {
  it("says INPUT NEEDED, and leaves a plain running row alone", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: "s1", title: "Parked", live: true, is_awaiting_input: true }),
            listSession({ id: "s2", title: "Working", live: true }),
          ],
        },
      ],
    });
    restore = view.restore;

    expect(await screen.findByText("INPUT NEEDED")).toBeInTheDocument();
    expect(screen.getByText("LIVE")).toBeInTheDocument();
  });

  it("finds the parked row by what it is waiting for", async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: "s1", title: "Parked", live: true, is_awaiting_input: true }),
            listSession({ id: "s2", title: "Working", live: true }),
          ],
        },
      ],
      query: "input needed",
    });
    restore = view.restore;

    expect(await screen.findByText("Parked")).toBeInTheDocument();
    expect(screen.queryByText("Working")).not.toBeInTheDocument();
  });
});

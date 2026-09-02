// @vitest-environment jsdom
import { screen, within } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// A run PARKED on an unanswered human-input request is still LIVE and still streams
// nothing: the list painted it as a session getting on with the job. The DEMAND outranks
// liveness in the row's own mark — and it says so IN ITS PROJECT. It had a pinned band of
// its own above the list until the reader asked for it to go (paraphrased: every session
// belongs to a project, so we do not need special elements for this).
describe("a session waiting on a human", () => {
  const row = (title: string) =>
    screen.getByText(title).closest("[data-session-id]") as HTMLElement;

  it("marks the parked row in its project, and leaves a plain running row alone", async () => {
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

    await screen.findByText("Parked");
    expect(within(row("Parked")).getByText("INPUT NEEDED")).toBeInTheDocument();
    expect(within(row("Working")).getByText("LIVE")).toBeInTheDocument();
    // Said ONCE, where the session lives. No second band repeats the same row above the
    // list it is already in.
    expect(screen.queryByLabelText("Sessions waiting on you")).toBeNull();
    expect(screen.getAllByText("INPUT NEEDED")).toHaveLength(1);
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

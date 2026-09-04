// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => {
  restore();
  window.innerHeight = 768;
});

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

  // Regression, user report (phone, a project of 115 sessions): the header counted
  // `1 needs input` while no row on the page said INPUT NEEDED — the parked session
  // sat deep in the project, outside the window, and the gateway's `awaiting` strip
  // beside that window was never read.
  it("pins a parked row above the page, however deep in the project it sits", async () => {
    window.innerHeight = 844;
    const at = (rank: number) => new Date(Date.UTC(2024, 4, 1, 10, 0, rank)).toISOString();
    // Forty rows at fifteen a page; the parked one is the oldest, on page three.
    const rows = Array.from({ length: 40 }, (_, index) =>
      listSession({
        id: `s${index}`,
        title: `alpha ${String(index).padStart(2, "0")}`,
        workspace: { root: "/Users/dev/alpha" },
        modified_at: at(40 - index),
        ...(index === 39 ? { live: true, is_awaiting_input: true } : {}),
      }),
    );
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    restore = view.restore;

    await screen.findByText("alpha 39");
    expect(within(row("alpha 39")).getByText("INPUT NEEDED")).toBeInTheDocument();
    expect(screen.getAllByText("INPUT NEEDED")).toHaveLength(1);
    const shown = () => view.queryAllByText(/^alpha \d\d$/).map((node) => node.textContent);
    // Above the page, not in place of a row of it.
    expect(shown()[0]).toBe("alpha 39");
    expect(shown()).toHaveLength(16);

    // On its own page it is painted ONCE, where the order puts it.
    fireEvent.click(view.getByLabelText("Page 3"));
    await waitFor(() => expect(shown()).toHaveLength(10));
    expect(shown().filter((title) => title === "alpha 39")).toHaveLength(1);
    expect(screen.getAllByText("INPUT NEEDED")).toHaveLength(1);
  });
});

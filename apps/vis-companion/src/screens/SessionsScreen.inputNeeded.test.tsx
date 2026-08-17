// @vitest-environment jsdom
import { screen, within } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

let restore = () => {};
afterEach(() => restore());

// A run PARKED on an unanswered human-input request is still LIVE and still
// streams nothing: the list painted it as a session getting on with the job,
// and the one place the operator counts what needs them said nothing at all.
describe("a session waiting on a human", () => {
  it("pins the parked row in its own band, and leaves a plain running row alone", async () => {
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

    const band = await screen.findByLabelText("Sessions waiting on you");
    expect(within(band).getByText("Parked")).toBeInTheDocument();
    expect(within(band).getByText("INPUT NEEDED")).toBeInTheDocument();
    expect(within(band).queryByText("Working")).not.toBeInTheDocument();
    expect(screen.getByText("LIVE")).toBeInTheDocument();
  });

  // Regression, user report (paraphrased: "the list jumps around while I read it"): the
  // gateway used to LIFT a parked run to the top of the ordering, which moved every row
  // under the reader and pushed another session out of the window. With the lift gone,
  // the parked run sits at its content time — which in a long fleet is a page this
  // device has not read yet — so the demand has to reach the screen beside the window.
  it("keeps a session parked deep in the fleet on screen while its page is unread", async () => {
    const rows = Array.from({ length: 120 }, (_, index) =>
      listSession({ id: `s${index}`, title: `Session ${index}` }),
    );
    rows[119] = listSession({
      id: "deep",
      title: "Parked deep",
      live: true,
      is_awaiting_input: true,
    });
    const view = renderSessionsScreen({
      machines: [{ sessions: rows, holdsPages: true }],
    });
    restore = view.restore;

    const band = await screen.findByLabelText("Sessions waiting on you");
    expect(within(band).getByText("Parked deep")).toBeInTheDocument();
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

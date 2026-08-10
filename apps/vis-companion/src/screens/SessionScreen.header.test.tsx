// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";

// The session header must tell the operator when a session is parked in a DRAFT
// workspace (an isolated agent clone) and NAME it, while showing nothing of the
// kind on the project itself.
describe("session header draft indicator", () => {
  it("names the draft a session is parked in", async () => {
    renderSessionScreen({
      session: sessionFixture({
        workspace: {
          root: "/Users/x/.vis/drafts/vis/moss",
          repo_root: "/Users/x/vis",
          label: "moss",
          is_draft: true,
        },
      }),
    });

    expect(await screen.findByText(/draft moss/)).toBeInTheDocument();
  });

  it("says nothing of the kind for a session on the project itself", () => {
    renderSessionScreen({
      session: sessionFixture({
        workspace: { root: "/Users/x/vis", label: "vis", is_draft: false },
      }),
    });

    expect(screen.queryByText(/draft/i)).not.toBeInTheDocument();
  });
});

// Regression, user report (a tablet showed a full band of composer chrome under
// the open artifacts sheet): the sheet covered the transcript and left the
// composer standing under it — chrome for a message nobody is writing, and on a
// tablet the biggest thing on a screen that is not about it.
describe("the composer under an open artifacts sheet", () => {
  it("is hidden while the sheet is open, and comes back when it closes", async () => {
    const user = userEvent.setup();
    renderSessionScreen({
      client: {
        sessionArtifacts: () =>
          Promise.resolve([
            {
              index: 0,
              turn: 1,
              iteration_id: "i1",
              kind: "image",
              media_type: "image/png",
              filename: "chart.png",
              version: 1,
              size: 128,
            },
          ]),
      },
    });

    const composer = document.querySelector("footer");
    expect(composer).not.toBeNull();
    expect(composer).not.toHaveClass("hidden");

    await user.click(await screen.findByRole("button", { name: /artifact/i }));
    expect(composer).toHaveClass("hidden");

    await user.click(screen.getByRole("button", { name: /close/i }));
    expect(composer).not.toHaveClass("hidden");
  });
});

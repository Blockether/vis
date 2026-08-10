// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { screen, waitFor } from "@testing-library/react";

import { renderSessionScreen } from "./session-screen-harness";

// Regression, session 617d3b77-8522-4866-b4b4-01cc8253bf1a: an image-heavy traced
// answer stayed hidden behind "Loading session…" while the off-screen history
// ramped in — the screen waited for the WHOLE visible window to hydrate instead
// of the first painted turns.
//
// The scroll ARITHMETIC this screen performs on its transcript — following the
// end, restoring a parked reader, telling its own correction from a gesture —
// lives in `lib/reading-position.ts` and is pinned there against real figures.
// jsdom lays nothing out (`scrollHeight` is 0), so a mounted screen can prove
// what it SHOWS, never how far it scrolled.
describe("opening a session", () => {
  it("shows the transcript instead of the loading sheet once turns arrive", async () => {
    renderSessionScreen({
      client: {
        transcript: () =>
          Promise.resolve([
            {
              id: "t1",
              user_request: "Rename the machine tag",
              status: "completed",
              iterations: [],
            },
          ]),
      },
    });

    expect(await screen.findByText("Rename the machine tag")).toBeInTheDocument();
    await waitFor(() =>
      expect(screen.queryByText(/Loading session/)).not.toBeInTheDocument(),
    );
  });
});

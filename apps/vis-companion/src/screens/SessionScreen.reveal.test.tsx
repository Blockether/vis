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
// Regression, session 976f705e-fd80-4787-adc6-1ae8388fdaa2: returning to a cached
// session still covered ready-to-paint rows with the cold-load sheet on every visit.
describe("returning to a cached session", () => {
  it("paints the cached transcript without showing the loading sheet", () => {
    renderSessionScreen({
      client: {
        cachedTranscript: () => [
          {
            id: "cached-turn",
            user_request: "Already in memory",
            status: "completed",
            iterations: [],
          },
        ],
      },
    });

    expect(screen.getByText("Already in memory")).toBeInTheDocument();
    expect(screen.queryByLabelText("Loading session")).not.toBeInTheDocument();
  });

  it("treats a cached empty transcript as ready rather than cold", () => {
    renderSessionScreen({ client: { cachedTranscript: () => [] } });

    expect(screen.queryByLabelText("Loading session")).not.toBeInTheDocument();
  });
});

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

    expect(screen.getByLabelText("Loading session")).toBeInTheDocument();
    expect(await screen.findByText("Rename the machine tag")).toBeInTheDocument();
    await waitFor(() =>
      expect(screen.queryByText(/Loading session/)).not.toBeInTheDocument(),
    );
  });

  // Regression, user report: a response shorter than the transcript viewport stayed
  // against the header and left most of the phone as an empty band above the composer.
  // The transcript's minimum viewport height must give that spare height to its TOP,
  // keeping the newest response beside the composer where subsequent chunks arrive.
  it("bottom-aligns a short response instead of leaving a blank lower viewport", async () => {
    renderSessionScreen({
      client: {
        transcript: () =>
          Promise.resolve([
            {
              id: "t-short",
              user_request: "Give me the short answer",
              status: "completed",
              iterations: [],
            },
          ]),
      },
    });

    expect(await screen.findByText("Give me the short answer")).toBeInTheDocument();
    const viewport = screen.getByRole("region", { name: "Transcript" });
    expect(viewport.firstElementChild).toHaveClass(
      "flex",
      "flex-col",
      "justify-end",
    );
  });
});

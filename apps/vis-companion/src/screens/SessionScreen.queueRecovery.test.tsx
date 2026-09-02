// @vitest-environment jsdom
import { cleanup, fireEvent, screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { renderSessionScreen } from "./session-screen-harness";

afterEach(cleanup);

describe("a queued turn after a failed request", () => {
  // Regression, Vis session 57dfea5e-0c2d-4190-a82c-0e1992e352c3: reopening the
  // session recovered the queued row but not queue.paused, so the app offered no
  // working way to continue that message after the provider recovered.
  it("recovers the paused marker with the backlog and continues it", async () => {
    const resumeQueue = vi.fn().mockResolvedValue(undefined);
    renderSessionScreen({
      client: {
        cachedQueuedTurns: () => [
          {
            turnId: "waiting",
            request: "Run this after recovery",
            preview: "Run this after recovery",
            attachments: [],
          },
        ],
        cachedQueuePaused: () => ({ reason: "turn_failed", held: 1 }),
        resumeQueue,
      },
    });

    expect(await screen.findByText("Run this after recovery")).toBeTruthy();
    expect(screen.getByText("1 held · turn failed")).toBeTruthy();

    fireEvent.click(screen.getByRole("button", { name: "Continue queue" }));
    await waitFor(() => expect(resumeQueue).toHaveBeenCalledWith("s1"));
  });
});

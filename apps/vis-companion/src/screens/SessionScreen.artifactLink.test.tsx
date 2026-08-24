// @vitest-environment jsdom
import { screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { describe, expect, it } from "vitest";

import { renderSessionScreen } from "./session-screen-harness";

const ATTACHMENT_ID = "8e3a587d-232c-497d-a290-7d16cfcf0e02";
const row = {
  id: "turn-1",
  user_request: "Show me the preview",
  status: "completed",
  created_at: Date.now(),
  content: [
    {
      id: "answer",
      type: "prose",
      markdown: `[Zobacz podgląd mobilny](attachment://${ATTACHMENT_ID})`,
    },
  ],
  iterations: [
    {
      id: "iteration-1",
      attachments: [
        {
          index: 0,
          iteration_id: "iteration-1",
          attachment_id: ATTACHMENT_ID,
          kind: "image",
          filename: "mobile-preview.png",
          media_type: "image/png",
          size: 1024,
        },
      ],
    },
  ],
};

// User report: tapping a preview link in an answer invoked an unsupported browser
// scheme, so the artifacts surface and the requested preview both stayed closed.
describe("an attachment link in an answer", () => {
  it("opens that artifact directly in the session's artifacts surface", async () => {
    renderSessionScreen({
      client: {
        cachedTranscript: () => [row],
        transcript: () => Promise.resolve([row]),
        sessionArtifacts: () => Promise.resolve([]),
      },
    });

    await userEvent.click(
      await screen.findByRole("link", { name: "Zobacz podgląd mobilny" }),
    );

    expect(
      screen.getByRole("region", { name: "Artifacts produced by the model" }),
    ).toBeInTheDocument();
    expect(
      screen.getByRole("dialog", { name: "mobile-preview.png" }),
    ).toBeInTheDocument();
  });
});

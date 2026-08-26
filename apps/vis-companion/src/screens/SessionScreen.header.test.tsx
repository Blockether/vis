// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { fireEvent, screen, waitFor, within } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";


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

  // Regression, user report: opening an artifact hid the whole composer, including
  // the microphone that an armed voice conversation still needs while its document
  // is being read. The only way to comment was to close the document first.
  it("keeps the voice microphone on the artifact surface", async () => {
    const user = userEvent.setup();
    const capabilities = {
      version: 1,
      features: {
        voice: { enabled: true, model: { status: "ready" } },
        attachments: { max_files: 8, media_types: ["image/*"] },
      },
    };
    renderSessionScreen({
      client: {
        cachedCapabilities: () => capabilities,
        capabilities: () => Promise.resolve(capabilities),
        sessionArtifacts: () =>
          Promise.resolve([
            {
              index: 0,
              turn: 1,
              iteration_id: "i1",
              kind: "document",
              media_type: "text/markdown",
              filename: "notes.md",
              version: 1,
              size: 128,
            },
          ]),
      },
    });

    fireEvent.contextMenu(
      screen.getByRole("button", { name: /dictate message/i }),
    );
    const voiceMic = await screen.findByRole("button", {
      name: /start voice utterance/i,
    });
    expect(voiceMic).toBeInTheDocument();

    await user.click(await screen.findByRole("button", { name: /artifact/i }));
    const surface = screen.getByRole("region", {
      name: "Artifacts produced by the model",
    });
    await user.click(
      within(surface).getByRole("button", { name: /^open notes\.md/i }),
    );
    expect(await screen.findByRole("dialog", { name: "notes.md" })).toBeVisible();
    const overlayMic = within(surface).getByRole("button", {
      name: /start voice utterance/i,
    });
    expect(overlayMic).toBeVisible();
    expect(overlayMic).toHaveClass("size-11");
  });
});

// A session id leaves this screen to be pasted somewhere that knows nothing
// about it, so the chip copies the MARKED form — a bare UUID could be any id at
// all, while `vis_session_id#<uuid>` says which kind of thing it names.
describe("the session id chip", () => {
  it("puts the marked id on the clipboard, not the bare uuid", async () => {
    const written: string[] = [];
    Object.defineProperty(navigator, "clipboard", {
      configurable: true,
      value: { writeText: (text: string) => void written.push(text) },
    });
    renderSessionScreen({
      session: sessionFixture({ id: "123e4567-e89b-12d3-a456-426614174000" }),
    });

    fireEvent.click(
      await screen.findByRole("button", { name: "Copy session id" }),
    );

    await waitFor(() =>
      expect(written).toEqual([
        "vis_session_id#123e4567-e89b-12d3-a456-426614174000",
      ]),
    );
  });
});

// Regression, user report from a phone (paraphrased: on iOS the heading's
// elements stand at a different height than everywhere else, and the boxes of
// the session screen's chrome change): this band cleared the notch with padding
// on the SAME box that spells `min-h-13`, and a min-height is a BORDER-BOX
// minimum — so the inset was subtracted from the band instead of standing over
// it. Measured at 390px with a 59px top inset the row collapsed to the 46px the
// title block happened to need, taking `BackButton` — which stretches to that
// row — from 51px to 46px, while the same header off a notch kept its full
// height. With the floor gone the heading also FOLLOWED its content, so anything
// that changed the title block moved the whole band on a phone and nowhere else.
// `DialogHeader isUnderNotch` clears a notch the same way.
describe("the session heading under a notch", () => {
  it("stands the notch strip ABOVE its own row, never out of it", async () => {
    renderSessionScreen({ session: sessionFixture({ id: "notched" }) });

    const band = (
      await screen.findByRole("button", { name: "Back to sessions" })
    ).closest("header")!;
    const worn = band.className.split(/\s+/).filter(Boolean);

    expect(worn).toContain("min-h-13");
    expect(worn).toContain("pt-[env(safe-area-inset-top)]");
    expect(worn).toContain("box-content");
  });
});

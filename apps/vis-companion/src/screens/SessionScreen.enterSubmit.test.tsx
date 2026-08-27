// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { renderSessionScreen } from "./session-screen-harness";

/** What this screen is looking at: a trackpad's pointer, or a thumb's. */
function pointer(kind: "fine" | "coarse") {
  window.matchMedia = ((query: string) => ({
    matches: query.includes("pointer: fine") ? kind === "fine" : false,
    media: query,
    onchange: null,
    addListener: () => {},
    removeListener: () => {},
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => false,
  })) as never;
}

afterEach(() => {
  vi.restoreAllMocks();
});

// Regression, user report (paraphrased: in the app Enter should send the message
// and Shift+Enter should make the new line): whether Return submitted was decided
// by the PLATFORM, so every native build typed a new line on Return even with a
// keyboard folded onto the tablet, and the send button was the only way out.
describe("Return in the composer", () => {
  it("sends where a fine pointer says a keyboard came with it", async () => {
    pointer("fine");
    const submitTurn = vi.fn(() => Promise.resolve(null));
    renderSessionScreen({ client: { submitTurn } });
    const composer = await screen.findByLabelText("Message Vis");

    fireEvent.change(composer, { target: { value: "Second paragraph" } });
    fireEvent.keyDown(composer, { key: "Enter", shiftKey: true });

    expect(submitTurn).not.toHaveBeenCalled();

    fireEvent.keyDown(composer, { key: "Enter" });

    await waitFor(() => expect(submitTurn).toHaveBeenCalledTimes(1));
  });

  it("types the new line under a thumb, which has no Shift to hold", async () => {
    pointer("coarse");
    const submitTurn = vi.fn(() => Promise.resolve(null));
    renderSessionScreen({ client: { submitTurn } });
    const composer = await screen.findByLabelText("Message Vis");

    fireEvent.change(composer, { target: { value: "First paragraph" } });
    const typed = fireEvent.keyDown(composer, { key: "Enter" });

    expect(typed).toBe(true);
    expect(submitTurn).not.toHaveBeenCalled();
  });
});

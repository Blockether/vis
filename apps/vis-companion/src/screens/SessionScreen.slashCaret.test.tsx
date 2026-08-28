// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { act, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";

// Regression, iOS keyboard on slash-command tap: completing a command set the
// prompt but left the native selection and the caret state where the original
// "/" was — inside the freshly written word. The iOS virtual keyboard then sat
// mid-word, fired autocorrect, and inserted at the wrong spot.
describe("slash-command completion caret", () => {
  it("parks the caret at the end of the completed command", async () => {
    const user = userEvent.setup();
    renderSessionScreen();

    const composer = screen.getByLabelText("Message Vis") as HTMLTextAreaElement;
    await user.type(composer, "/relo");
    await user.click(await screen.findByText("/reload"));

    // The caret is parked inside a frame the completion asks for.
    await act(async () => {
      await new Promise((resolve) => requestAnimationFrame(() => resolve(null)));
    });

    expect(composer.value).toBe("/reload ");
    expect(composer.selectionStart).toBe(composer.value.length);
    expect(composer.selectionEnd).toBe(composer.value.length);
  });
});

// Regression: the screen fetched a gateway-global slash palette instead of the
// active session's project-relative one.
describe("session-scoped slash discovery", () => {
  it("asks for the palette of the session on screen", async () => {
    const slashes = vi.fn((sid: string) => {
      void sid;
      return Promise.resolve([]);
    });
    renderSessionScreen({ client: { slashes } });

    await waitFor(() => expect(slashes).toHaveBeenCalled());
    expect(slashes.mock.calls[0]?.[0]).toBe("s1");
  });
});

// Skills keep their canonical namespace while remaining discoverable both by
// unprefixed search and in the complete slash palette.
describe("skill slash search", () => {
  it("shows a prefixed skill by name and in the complete palette", async () => {
    const user = userEvent.setup();
    renderSessionScreen({
      session: sessionFixture({ id: "skill-search" }),
      client: {
        slashes: () =>
          Promise.resolve([
            { name: "/skill:create-extension", doc: "Create an extension" },
          ]),
      },
    });

    const composer = screen.getByLabelText("Message Vis");
    await user.type(composer, "/create-ext");

    expect(await screen.findByText("/skill:create-extension")).toBeTruthy();

    await user.clear(composer);
    await user.type(composer, "/");
    expect(await screen.findByText("/skill:create-extension")).toBeTruthy();
  });
});

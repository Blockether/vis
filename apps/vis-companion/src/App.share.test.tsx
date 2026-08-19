// @vitest-environment jsdom
import { fireEvent, screen } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { renderApp } from "./app-harness";
import {
  peekPendingShare,
  receiveSharedText,
  resetShareIntakeForTests,
} from "./lib/share-intake";
import { listSession } from "./screens/sessions-screen-harness";

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
  resetShareIntakeForTests();
});

const settle = (ms = 0) => new Promise((resolve) => setTimeout(resolve, ms));

const fleet = () => [
  { label: "laptop", sessions: [listSession({ id: "s1", title: "Session one" })] },
];

// Regression, cross-validated on the Android emulator: a voice memo shared from
// the system sheet went straight into whichever session happened to be open, and
// the list never got to offer the choice. The screen that is mounted is not a
// destination — only a human naming one is.
describe("a share that arrives with no destination", () => {
  it("does not fall into the session that happens to be open", async () => {
    const view = renderApp({ machines: fleet() });
    restore = view.restore;

    fireEvent.click(await screen.findByText("Session one"));
    const composer = (await screen.findByLabelText(
      "Message Vis",
    )) as HTMLTextAreaElement;
    await settle(50);

    receiveSharedText({ text: "look at this" });
    await settle(50);

    expect(composer.value).toBe("");
    expect(peekPendingShare()?.text).toBe("look at this");
    view.unmount();
  });

  it("lands in the session the human then picks", async () => {
    const view = renderApp({ machines: fleet() });
    restore = view.restore;

    await screen.findByText("Session one");
    receiveSharedText({ text: "look at this" });
    // The list is the chooser, and it says what it is holding.
    expect(await screen.findByText("Sharing")).toBeInTheDocument();

    fireEvent.click(screen.getByText("Session one"));
    const composer = (await screen.findByLabelText(
      "Message Vis",
    )) as HTMLTextAreaElement;
    await settle(50);

    expect(composer.value).toContain("look at this");
    expect(peekPendingShare()).toBeNull();
    view.unmount();
  });
});

// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from "@testing-library/react";
import { afterEach, describe, expect, it } from "vitest";

import { renderApp } from "./app-harness";
import { listSession } from "./screens/sessions-screen-harness";

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
});

// `DESK_RAIL` as `fit-rows.ts` spells it — a wide window under a fine pointer.
const onADesk = () => {
  const previous = window.matchMedia;
  window.matchMedia = ((query: string) => ({
    matches: query.includes("pointer: fine"),
    media: query,
    onchange: null,
    addListener: () => {},
    removeListener: () => {},
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => false,
  })) as never;
  return () => {
    window.matchMedia = previous;
  };
};

const fleet = () => [
  {
    label: "laptop",
    sessions: [
      listSession({
        id: "one",
        title: "Alpha one",
        workspace: { root: "/Users/dev/alpha" },
        modified_at: "2024-05-01T11:00:00Z",
      }),
    ],
  },
];

// Regression, user report (desktop review: the list was a 1400px table with a second
// fleet index beside it, and opening a session replaced the whole window with the
// transcript): a desk is a SPLIT — the list stands in a sidebar, the conversation
// fills the rest, and nothing navigates away from the list to read one.
describe("a desk keeps the list beside the conversation", () => {
  it("stands the list in a sidebar and centres an empty pane until a session opens", async () => {
    const restoreDensity = onADesk();
    const view = renderApp({ machines: fleet() });
    restore = () => {
      view.restore();
      restoreDensity();
    };
    await screen.findByText("Alpha one");

    const main = view.baseElement.querySelector("main") as HTMLElement;
    const sidebar = main.firstElementChild as HTMLElement;
    expect(within(sidebar).getByRole("region", { name: "Sessions" })).toBeTruthy();
    expect(sidebar.className).toContain("w-80");
    expect(screen.getByRole("region", { name: "No session open" })).toBeTruthy();
    // The shell's own bar stays over both columns.
    expect(screen.getByRole("button", { name: "Open preferences" })).toBeTruthy();
    view.unmount();
  });

  it("opens a session beside the list, with no way back because nothing was left", async () => {
    const restoreDensity = onADesk();
    const view = renderApp({ machines: fleet() });
    restore = () => {
      view.restore();
      restoreDensity();
    };
    await screen.findByText("Alpha one");
    fireEvent.click(screen.getByText("Alpha one"));
    await screen.findByLabelText("Message Vis");

    // The list did not leave; the empty pane did.
    expect(screen.getByRole("region", { name: "Sessions" })).toBeTruthy();
    expect(screen.queryByRole("region", { name: "No session open" })).toBeNull();
    expect(screen.queryByRole("button", { name: "Back to sessions" })).toBeNull();
    // The shell's bar is still over both columns.
    expect(screen.getByRole("button", { name: "Open preferences" })).toBeTruthy();
    view.unmount();
  });

  it("still hands a phone the whole screen and a way back", async () => {
    const view = renderApp({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("Alpha one");
    expect(screen.queryByRole("region", { name: "No session open" })).toBeNull();
    fireEvent.click(screen.getByText("Alpha one"));
    await screen.findByLabelText("Message Vis");
    expect(screen.getByRole("button", { name: "Back to sessions" })).toBeTruthy();
    await waitFor(() =>
      expect(screen.queryByRole("button", { name: "Open preferences" })).toBeNull(),
    );
    view.unmount();
  });
});

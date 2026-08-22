// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import type * as FleetModule from "./lib/fleet";

// The passes this device could still take over the rows on their way to the
// screen. If the list a reader sees is the gateway's ANSWER, none of them runs.
const counters = vi.hoisted(() => ({ ordered: 0 }));
vi.mock("./lib/fleet", async (importOriginal) => {
  const actual = (await importOriginal()) as typeof FleetModule;
  return {
    ...actual,
    sessionOrder: (...args: Parameters<typeof actual.sessionOrder>) => {
      counters.ordered += 1;
      return actual.sessionOrder(...args);
    },
  };
});

import { renderApp } from "./app-harness";
import { listSession } from "./screens/sessions-screen-harness";

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
  counters.ordered = 0;
});

const rowOrder = () =>
  [...document.querySelectorAll("[data-session-id]")].map((row) =>
    row.getAttribute("data-session-id"),
  );

const settle = (ms = 0) => new Promise((resolve) => setTimeout(resolve, ms));

const readsOfTheList = (requests: string[]) =>
  requests.filter((href) => href.includes("/v1/sessions?"));

// One project whose LAST row is an empty session: no title of its own, no turn
// ever taken. Every device is answered that row last — except the one holding
// words for it, and only that device knows.
const fleet = () => [
  {
    sessions: [
      listSession({
        id: "one",
        title: "Alpha one",
        workspace: { root: "/Users/dev/alpha" },
        modified_at: "2024-05-01T11:00:00Z",
      }),
      listSession({
        id: "two",
        title: "Alpha two",
        workspace: { root: "/Users/dev/alpha" },
        modified_at: "2024-05-01T10:00:00Z",
      }),
      listSession({
        id: "blank",
        title: "",
        turn_count: 0,
        workspace: { root: "/Users/dev/alpha" },
        modified_at: "2024-05-01T09:00:00Z",
      }),
    ],
  },
];

// The acceptance of "let the gateway own the list": a row this device — and only
// this device — has news about still arrives in its place from the gateway. The
// device says WHAT it holds (`dirty=`), never WHERE the row goes.
describe("a session holding unsent words", () => {
  it("comes back in the gateway's dirty band, with nothing reordered here", async () => {
    const view = renderApp({ machines: fleet() });
    restore = view.restore;
    await screen.findByText("Alpha one");
    expect(rowOrder()).toEqual(["one", "two", "blank"]);

    fireEvent.click(screen.getByText("Untitled session"));
    const composer = (await screen.findByLabelText(
      "Message Vis",
    )) as HTMLTextAreaElement;
    // The stored draft message is read asynchronously; a keystroke before it
    // lands would be overwritten by the read.
    await settle(50);
    fireEvent.change(composer, { target: { value: "half a thought" } });
    await settle();
    fireEvent.click(screen.getByRole("button", { name: "Back to sessions" }));

    await waitFor(() => expect(rowOrder()).toEqual(["blank", "one", "two"]));
    // What travelled is the fact, and the fact is the id.
    expect(readsOfTheList(view.requests).at(-1)).toContain("dirty=blank");
    // Not one pass over the rows on this side: no filter, no band, no sort.
    expect(counters.ordered).toBe(0);
    view.unmount();
  });
});

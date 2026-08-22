// @vitest-environment jsdom
import { fireEvent, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

// The list's fleet-wide pass over the rows, counted where it actually runs: once
// per machine in scope, every time the sessions list recomputes what it shows.
const counters = vi.hoisted(() => ({ passes: 0 }));
vi.mock("./lib/fleet", async (importOriginal) => {
  const actual = await importOriginal<typeof import("./lib/fleet")>();
  return {
    ...actual,
    withSearchHits: (...args: Parameters<typeof actual.withSearchHits>) => {
      counters.passes += 1;
      return actual.withSearchHits(...args);
    },
  };
});

import { renderApp } from "./app-harness";
import { listSession } from "./screens/sessions-screen-harness";

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
});

// A fleet the list has to do real work over: one machine, four projects, and a
// first session with no title of its own, so what a composer holds is the only
// thing that can name its row.
const fleet = () => [
  {
    label: "laptop",
    sessions: Array.from({ length: 12 }, (_, index) =>
      listSession({
        id: `s${index}`,
        title: index === 0 ? "" : `Session ${index}`,
        workspace: { root: `/Users/dev/project-${index % 4}` },
      }),
    ),
  },
];

const settle = (ms = 0) => new Promise((resolve) => setTimeout(resolve, ms));

const openFirstSession = async () => {
  const view = renderApp({ machines: fleet() });
  restore = view.restore;
  fireEvent.click(await screen.findByText("Untitled session"));
  const composer = (await screen.findByLabelText(
    "Message Vis",
  )) as HTMLTextAreaElement;
  // The stored draft message is read asynchronously and nothing is recorded
  // until it lands, so let it land before a keystroke is measured.
  await settle(50);
  return { view, composer };
};

const type = async (composer: HTMLTextAreaElement, text: string) => {
  for (let index = 1; index <= text.length; index += 1) {
    fireEvent.change(composer, { target: { value: text.slice(0, index) } });
    await settle();
  }
};

// Regression, reported from the phone ("writing in the input of the companion
// app hangs for half a second, many times, on iOS"): the sessions list stays
// MOUNTED behind an open transcript, and every keystroke in the composer wrote
// the draft message through the store that list subscribes to. Each write
// published a fresh snapshot object, so a character re-ran the fleet-wide
// published a fresh snapshot object, so a character re-ran the fleet-wide pass
// over every machine and every session, and re-rendered every project group, for
// a screen the reader cannot see.
describe("typing in the composer", () => {
  it("does not re-run the sessions list behind it", async () => {
    const { view, composer } = await openFirstSession();

    // The first character is real news: this session now holds unsent work, so
    // its row is dirty. Everything after it tells the list nothing new.
    await type(composer, "h");
    const before = counters.passes;
    await type(composer, "hello there");

    expect(composer.value).toBe("hello there");
    expect(counters.passes - before).toBe(0);
    view.unmount();
  });

  it("hands the list the words on the way out", async () => {
    const { view, composer } = await openFirstSession();
    await type(composer, "half a thought");

    fireEvent.click(screen.getByRole("button", { name: "Back to sessions" }));

    // Leaving persists the message, and that is when the list is told: the row
    // of a session with no title of its own is named by what is waiting in it.
    expect(await screen.findByText("half a thought")).toBeTruthy();
    expect(screen.getByText("dirty")).toBeTruthy();
    view.unmount();
  });
});

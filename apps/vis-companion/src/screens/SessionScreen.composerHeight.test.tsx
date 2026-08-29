// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { act, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";

// jsdom lays nothing out, so the composer's geometry is handed over here: the
// shipped box is `h-8 py-2 text-ui` — 32px around one 16px line between 8px
// paddings — and it may grow to `max-h-20`, 80px. A character is ~6px wide at
// that step, which is what decides where a line wraps.
const BOX = 32;
const LINE = 16;
const PADDING = 16;
const CEILING = 80;
const CHARACTER = 6;

const layout = { width: 216 };

/** The box the browser would report for the height this element carries. */
function boxHeight(element: HTMLTextAreaElement): number {
  const inline = Number.parseFloat(element.style.height);
  return Number.isNaN(inline)
    ? BOX
    : Math.min(CEILING, Math.max(BOX, inline));
}

function installLayout(): void {
  const perLine = () => Math.max(1, Math.floor(layout.width / CHARACTER));
  const define = (name: string, get: (self: HTMLTextAreaElement) => number) =>
    Object.defineProperty(HTMLTextAreaElement.prototype, name, {
      configurable: true,
      get(this: HTMLTextAreaElement) {
        return get(this);
      },
    });
  define("clientWidth", () => layout.width);
  define("clientHeight", boxHeight);
  define("scrollHeight", (element) => {
    // A textarea measures its PLACEHOLDER when it holds no value, and the
    // composer's placeholder wraps to two lines on a phone.
    const text = element.value || element.placeholder;
    const lines = Math.max(1, Math.ceil(text.length / perLine()));
    return Math.max(boxHeight(element), lines * LINE + PADDING);
  });
}

/** ResizeObserver, reduced to "fire the callbacks watching this element". */
function installObserver(): (element: Element) => void {
  const watchers: { target: Element; run: () => void }[] = [];
  vi.stubGlobal(
    "ResizeObserver",
    class {
      private readonly callback: () => void;
      constructor(callback: () => void) {
        this.callback = callback;
      }
      observe(target: Element) {
        watchers.push({ target, run: () => this.callback() });
      }
      unobserve() {}
      disconnect() {}
      takeRecords() {
        return [];
      }
    },
  );
  return (element) => {
    for (const watcher of watchers) {
      if (watcher.target === element) watcher.run();
    }
  };
}

afterEach(() => {
  layout.width = 216;
  for (const name of ["clientWidth", "clientHeight", "scrollHeight"]) {
    Reflect.deleteProperty(HTMLTextAreaElement.prototype, name);
  }
  vi.unstubAllGlobals();
});

// Regression, composer clipped its own second line: the box was measured only
// when the PROMPT changed, so anything that rewrapped the same text under it —
// a rotation, a split view, the transcript's scrollbar arriving mid-turn, the
// mic button mounting with the capabilities answer — left the composer one line
// tall around two lines, showing the line just typed cut in half inside its
// bottom padding until the next keystroke happened to grow it.
describe("composer height", () => {
  it("grows with the text and comes back down when it is deleted", async () => {
    const user = userEvent.setup();
    installLayout();
    installObserver();
    renderSessionScreen({ session: sessionFixture({ id: "typed" }) });

    const composer = screen.getByLabelText("Message Vis") as HTMLTextAreaElement;
    await user.type(composer, "a".repeat(30));
    expect(composer.style.height).toBe("");

    await user.type(composer, "a".repeat(10));
    expect(composer.style.height).toBe("48px");

    await user.type(composer, "{backspace}".repeat(10));
    expect(composer.style.height).toBe("32px");
  });

  it("refits when the box narrows under text that did not change", async () => {
    const user = userEvent.setup();
    installLayout();
    const resize = installObserver();
    renderSessionScreen({ session: sessionFixture({ id: "narrowed" }) });

    const composer = screen.getByLabelText("Message Vis") as HTMLTextAreaElement;
    await user.type(composer, "a".repeat(30));
    expect(composer.style.height).toBe("");

    layout.width = 150;
    act(() => resize(composer));
    expect(composer.style.height).toBe("48px");

    layout.width = 320;
    act(() => resize(composer));
    expect(composer.style.height).toBe("32px");
  });

  it("keeps an empty composer at its own height, placeholder and all", () => {
    // Narrow enough that the placeholder itself wraps: an empty box measures
    // that text, and sizing to it would grow the composer around words nobody
    // typed.
    layout.width = 120;
    installLayout();
    const resize = installObserver();
    renderSessionScreen({ session: sessionFixture({ id: "empty" }) });

    const composer = screen.getByLabelText("Message Vis") as HTMLTextAreaElement;
    expect(composer.value).toBe("");
    expect(composer.scrollHeight).toBeGreaterThan(composer.clientHeight);

    act(() => resize(composer));
    expect(composer.style.height).toBe("");
  });

  // Regression, user report ("it goes outside of the input"): while a turn ran the
  // composer read "Message Vis — queues behind the running turn" — 43 characters in
  // a box that holds 36, so the placeholder wrapped to a second line that the box,
  // which deliberately never grows around text nobody typed, then clipped.
  it("says a message queues in the one line the composer keeps", () => {
    installLayout();
    installObserver();
    renderSessionScreen({
      session: sessionFixture({ id: "busy", status: "running" }),
      client: {
        cachedRunningTurn: () => ({
          turn: {
            id: "t1",
            request: "check the logs",
            answer: "",
            iterations: [],
            startedAt: Date.now(),
            status: "running" as const,
          },
          seq: 1,
        }),
      },
    });

    const composer = screen.getByLabelText("Message Vis") as HTMLTextAreaElement;
    expect(composer.placeholder).toContain("queues");
    expect(composer.scrollHeight).toBe(composer.clientHeight);
  });
});

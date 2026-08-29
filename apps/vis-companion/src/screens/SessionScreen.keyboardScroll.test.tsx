// @vitest-environment jsdom
import { act, fireEvent, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";
import { noteReaderGesture } from "../lib/reader-gesture";
import { flushParked } from "../lib/parked";

// Regression, user report ("sometimes I tap somewhere in the middle of the
// screen and the whole thing shoots off"): a tap outside the composer is what
// takes the iOS keyboard DOWN, the shell grows back by the keyboard's height,
// and a reader parked in history was moved by that entire height — measured on
// an iPhone 17 Pro simulator, 274 px of a 568 px screen, in one frame, for a
// tap that asked for nothing. Raising the keyboard moved them the same way.

/** The transcript's own height; it never changes here — only the shell does. */
const TRANSCRIPT = 46_000;
/** The scroller with no keyboard, and what the keyboard takes off its bottom. */
const SHELL = 800;
const KEYBOARD = 274;

function transcript() {
  return [1, 2, 3].map((position) => ({
    turn_id: `t${position}`,
    request: `question ${position}`,
    status: "completed",
    iterations: [
      {
        position,
        id: `i${position}`,
        assistant_prose: `answer ${position}`,
        forms: [],
      },
    ],
  }));
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

/**
 * The geometry a browser would have measured, with the shell's height in one
 * place and every write to the scroller recorded: the report is about pixels
 * that moved on their own, so the test watches the writes, not just the end
 * state.
 */
function measure(
  viewport: HTMLElement,
  shell: { height: number },
  moves: number[],
): void {
  let top = 0;
  Object.defineProperty(viewport, "scrollHeight", {
    configurable: true,
    get: () => TRANSCRIPT,
  });
  Object.defineProperty(viewport, "clientHeight", {
    configurable: true,
    get: () => shell.height,
  });
  Object.defineProperty(viewport, "scrollTop", {
    configurable: true,
    get: () => top,
    set: (value: number) => {
      top = value;
      moves.push(value);
    },
  });
}

/** The window the keyboard leaves behind, which is what `shellViewportHeight` reads. */
function shellIs(height: number): void {
  Object.defineProperty(window, "innerHeight", {
    configurable: true,
    value: height,
  });
}

/** Frames run by hand, so a "keyboard" is exactly as long as it says. */
function installFrames() {
  const frames: FrameRequestCallback[] = [];
  vi.stubGlobal("requestAnimationFrame", (callback: FrameRequestCallback) => {
    frames.push(callback);
    return frames.length;
  });
  vi.stubGlobal("cancelAnimationFrame", () => {});
  return async () => {
    const due = frames.splice(0);
    await act(async () => {
      for (const callback of due) callback(0);
    });
  };
}

/** Is the "↓ Latest" offer on screen? */
function latestOffered(): boolean {
  return !!screen.queryByRole("button", { name: /Latest/ });
}

describe("the keyboard against the reader's place", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    flushParked();
    Reflect.deleteProperty(window, "innerHeight");
  });

  it("re-pins a reader who is following the end", async () => {
    const paint = installFrames();
    const shell = { height: SHELL };
    shellIs(SHELL);
    const resize = installObserver();
    renderSessionScreen({
      session: sessionFixture({ id: "following" }),
      client: { transcript: () => Promise.resolve(transcript()) },
    });
    await act(async () => {});
    const viewport = screen.getByRole("region", { name: "Transcript" });
    const moves: number[] = [];
    measure(viewport, shell, moves);
    await paint();

    // One settle with nothing moving: the screen learns the geometry it would
    // have measured on the phone before a keyboard was anywhere near it.
    act(() => resize(viewport));
    moves.length = 0;

    // Nobody has touched the scroller: the end is still the anchor, and the
    // keyboard is about to take 274 px of the newest turn with it.
    shell.height = SHELL - KEYBOARD;
    shellIs(SHELL - KEYBOARD);
    act(() => resize(viewport));

    expect(viewport.scrollTop).toBe(TRANSCRIPT - (SHELL - KEYBOARD));
  });

  it("leaves a reader in history exactly where they are", async () => {
    const paint = installFrames();
    const shell = { height: SHELL };
    shellIs(SHELL);
    const resize = installObserver();
    renderSessionScreen({
      session: sessionFixture({ id: "parked" }),
      client: { transcript: () => Promise.resolve(transcript()) },
    });
    await act(async () => {});
    const viewport = screen.getByRole("region", { name: "Transcript" });
    const moves: number[] = [];
    measure(viewport, shell, moves);
    await paint();

    // One settle with nothing moving: the screen learns the geometry it would
    // have measured on the phone before a keyboard was anywhere near it.
    act(() => resize(viewport));
    moves.length = 0;

    // The reader drags back into history and stops reading the end.
    noteReaderGesture();
    viewport.scrollTop = 20_000;
    fireEvent.scroll(viewport);
    await paint();
    moves.length = 0;

    // Tapping the composer raises the keyboard: the scroller loses its bottom
    // 274 px, and every line the reader is looking at keeps its exact y.
    shell.height = SHELL - KEYBOARD;
    shellIs(SHELL - KEYBOARD);
    act(() => resize(viewport));
    expect(viewport.scrollTop).toBe(20_000);

    // Tapping anywhere outside the composer takes it down again — the tap in
    // the report. Same answer: nothing moves.
    shell.height = SHELL;
    shellIs(SHELL);
    act(() => resize(viewport));
    expect(viewport.scrollTop).toBe(20_000);

    expect(moves).toEqual([]);
  });
  // Regression, user report ("I tap the input on iOS and the Latest pill appears —
  // I am writing a new message, that makes no sense"): nothing scrolled. The
  // keyboard took 274 px off the scroller's bottom, the newest turn stayed exactly
  // where it was, and the offer measured that lost edge as distance the reader had
  // chosen to keep — over the composer they had just tapped to write in.
  it("makes no offer to a reader who taps the composer at the end", async () => {
    const paint = installFrames();
    const shell = { height: SHELL };
    shellIs(SHELL);
    const resize = installObserver();
    renderSessionScreen({
      session: sessionFixture({ id: "writing" }),
      client: { transcript: () => Promise.resolve(transcript()) },
    });
    await act(async () => {});
    const viewport = screen.getByRole("region", { name: "Transcript" });
    const moves: number[] = [];
    measure(viewport, shell, moves);
    await paint();
    act(() => resize(viewport));

    // They are at the newest turn, and a ten-pixel nudge drops follow there: an
    // upward move is "hold this line" however small, so the screen stops chasing
    // and never re-arms while they sit still. The end is on screen; nothing is
    // offered, because there is nowhere to go.
    viewport.scrollTop = TRANSCRIPT - SHELL;
    fireEvent.scroll(viewport);
    await paint();
    viewport.scrollTop -= 10;
    fireEvent.scroll(viewport);
    await paint();
    expect(latestOffered()).toBe(false);

    // Now they tap the composer and the keyboard slides up over the bottom 274 px.
    act(() => (screen.getByLabelText("Message Vis") as HTMLTextAreaElement).focus());
    shell.height = SHELL - KEYBOARD;
    shellIs(SHELL - KEYBOARD);
    act(() => resize(viewport));
    await paint();

    expect(latestOffered()).toBe(false);
    expect(viewport.scrollTop).toBe(TRANSCRIPT - (SHELL - KEYBOARD));
  });
});

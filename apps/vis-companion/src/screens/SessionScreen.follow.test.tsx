// @vitest-environment jsdom
import { act, fireEvent, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";
import { noteReaderGesture } from "../lib/reader-gesture";
import { flushParked } from "../lib/parked";
import { parkedReadingPosition } from "../lib/reading-position";

// Regression, user report ("it was live, I even scrolled all the way down, and
// it never remembered that I want the new things — it kept putting me back
// somewhere in the middle of the live turn"): a turn being written grows the
// transcript every flush, so a reader dragging down is measured against an end
// that moved while they were reaching for it. On an iPhone 17 Pro simulator,
// six hard drags at a streaming session ended 512 px above the end, the follow
// never re-engaged, "↓ Latest" stayed offered, and every later flush widened
// the gap the reader had just closed by hand.

/** The scroller, with the transcript's height in one mutable place. */
const SHELL = 800;
/** What one flush of a live turn adds while the finger is on the glass. */
const FLUSH = 300;

function transcript() {
  return [1, 2, 3].map((position) => ({
    id: `t${position}`,
    user_request: `question ${position}`,
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

function measure(
  viewport: HTMLElement,
  live: { height: number },
  moves: number[],
): void {
  let top = 0;
  Object.defineProperty(viewport, "scrollHeight", {
    configurable: true,
    get: () => live.height,
  });
  Object.defineProperty(viewport, "clientHeight", {
    configurable: true,
    get: () => SHELL,
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

/** Frames run by hand: `handleScroll` batches its measurement into one. */
function installFrames() {
  const frames: FrameRequestCallback[] = [];
  vi.stubGlobal("requestAnimationFrame", (callback: FrameRequestCallback) => {
    frames.push(callback);
    return frames.length;
  });
  vi.stubGlobal("cancelAnimationFrame", () => {});
  return async () => {
    for (let round = 0; round < 4; round += 1) {
      const due = frames.splice(0);
      if (!due.length) return;
      await act(async () => {
        for (const callback of due) callback(0);
      });
    }
  };
}

/** ResizeObserver reduced to firing every callback watching one element. */
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

/** Is the "↓ Latest" offer on screen? */
function latestOffered(): boolean {
  return !!screen.queryByRole("button", { name: /Latest/ });
}

describe("a reader reaching the end of a turn that is still being written", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    flushParked();
  });

  async function readerDrags({
    sid,
    steps,
    step,
    growth,
    startAbove,
  }: {
    sid: string;
    steps: number;
    step: number;
    growth: number;
    startAbove: number;
  }) {
    const paint = installFrames();
    const live = { height: 46_000 };
    renderSessionScreen({
      session: sessionFixture({ id: sid, status: "running" }),
      client: { transcript: () => Promise.resolve(transcript()) },
    });
    await act(async () => {});
    const viewport = screen.getByRole("region", { name: "Transcript" });
    const moves: number[] = [];
    measure(viewport, live, moves);
    await paint();

    // The reader is in history, a good way above the newest turn.
    noteReaderGesture();
    viewport.scrollTop = live.height - SHELL - startAbove;
    fireEvent.scroll(viewport);
    await paint();

    // Now they drag DOWN, and the turn keeps being written underneath them:
    // every step of the gesture adds a flush of new height below the fold.
    for (let taken = 0; taken < steps; taken += 1) {
      noteReaderGesture();
      viewport.scrollTop = viewport.scrollTop + step;
      live.height += growth;
      fireEvent.scroll(viewport);
      await paint();
    }
    return { viewport, live, moves };
  }

  it("treats the end they were reaching for as the end", async () => {
    const { viewport, live } = await readerDrags({
      sid: "chasing",
      steps: 7,
      step: 700,
      growth: FLUSH,
      startAbove: 3_000,
    });

    // They cannot physically close the last 200 px: each 700 px of drag is
    // eaten by 300 px of new transcript. They still ARRIVED.
    expect(live.height - viewport.scrollTop - SHELL).toBeGreaterThan(64);
    expect(latestOffered()).toBe(false);
    // Their place is the newest turn, so nothing is parked to reopen into.
    expect(parkedReadingPosition("chasing")).toBe(null);
  });

  it("leaves a reader who stayed in history where they are", async () => {
    const { viewport, live } = await readerDrags({
      sid: "reading",
      steps: 3,
      step: 700,
      growth: FLUSH,
      startAbove: 12_000,
    });

    const gap = live.height - viewport.scrollTop - SHELL;
    expect(gap).toBeGreaterThan(SHELL);
    expect(latestOffered()).toBe(true);
    expect(parkedReadingPosition("reading")).toBe(gap);
  });

  // Regression, session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25: every large
  // tool/result block arriving at the end moved the transcript by that block's
  // full height. Measured in WebKit, one 261 px update moved scrollTop 259 px in
  // a 489 px viewport; a tool-heavy turn repeatedly replaced what was on screen.
  it("stops following before a large live batch yanks the visible page", async () => {
    const paint = installFrames();
    const resize = installObserver();
    const live = { height: 46_000 };
    // No gesture from an earlier test may own this test's scroller.
    const now = Date.now();
    vi.spyOn(Date, "now").mockReturnValue(now + 1_000);
    renderSessionScreen({
      session: sessionFixture({ id: "bursty", status: "running" }),
      client: {
        cachedTranscript: () => transcript(),
        transcript: () => Promise.resolve(transcript()),
      },
    });
    await act(async () => {});
    const viewport = screen.getByRole("region", { name: "Transcript" });
    const content = viewport.firstElementChild!;
    const moves: number[] = [];
    measure(viewport, live, moves);
    await paint();

    viewport.scrollTop = live.height - SHELL;
    fireEvent.scroll(viewport);
    await paint();
    moves.length = 0;

    // A line-sized stream flush remains a follow: reading current prose should
    // still reveal its next line without an offer to jump.
    live.height += 40;
    act(() => resize(content));
    expect(viewport.scrollTop).toBe(live.height - SHELL);
    expect(latestOffered()).toBe(false);

    // A card-sized batch is different: keep the current line on the glass and
    // let Latest offer the new block instead of moving half a screen at once.
    const before = viewport.scrollTop;
    live.height += SHELL / 2;
    act(() => resize(content));
    expect(viewport.scrollTop).toBe(before);
    expect(latestOffered()).toBe(true);
  });
});

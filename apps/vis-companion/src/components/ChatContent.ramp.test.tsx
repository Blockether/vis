// @vitest-environment jsdom
import { act, fireEvent, render } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { TranscriptIteration } from "../lib/types";

const { IterationTrace } = await import("./ChatContent");

// Regression, user report ("scrolling up, the python blocks are still white"):
// pressing "Load earlier" on a big session left the transcript filling in for
// SEVEN seconds — measured on device, 20 000 nodes in 74 mounting frames of
// 30-200 ms — so a reader scrolling up chased bare paper. A ramp step costs
// ~70 ms whatever its size (one reconcile, one style pass, one paint) and only
// ~0.05 ms per node it mounts, so the step size decides how many times that
// 70 ms is paid. The old controller aimed each step at a 6 ms budget it never
// measured and halved on any frame over 32 ms — which EVERY step overruns — so
// it collapsed to its floor and paid the fixed cost hundreds of times.
function iteration(position: number): TranscriptIteration {
  return {
    position,
    id: `i${position}`,
    thinking: `thought ${position}`,
    assistant_prose: `step ${position}`,
    forms: [],
    attachments: [],
  } as unknown as TranscriptIteration;
}

const client = {
  base: "http://gateway.example.com",
  retainAttachment: () => () => {},
  attachmentUrl: () => Promise.resolve(null),
} as never;

/** Every frame a mounting step lands in costs this — what the device measured. */
const FRAME_MS = 70;

/**
 * Mount a trace of `count` iterations and pump animation frames until it stops
 * asking for them, answering how many frames the backfill took.
 */
function rampFrames(
  count: number,
  { unfold = false }: { unfold?: boolean } = {},
): { frames: number; segments: number } {
  const queue: FrameRequestCallback[] = [];
  let clock = 0;
  vi.stubGlobal("requestAnimationFrame", (cb: FrameRequestCallback) => {
    queue.push(cb);
    return queue.length;
  });
  vi.stubGlobal("cancelAnimationFrame", () => {});
  vi.spyOn(performance, "now").mockImplementation(() => clock);

  const iterations = Array.from({ length: count }, (_, index) =>
    iteration(index),
  );
  const view = render(
    <IterationTrace iterations={iterations} live={false} client={client} sid="s1" />,
  );
  const rail = () => view.container.firstElementChild?.children.length ?? 0;

  const pump = () => {
    let frames = 0;
    let previous = -1;
    while (queue.length > 0 && frames < 2000) {
      const tick = queue.shift();
      if (!tick) break;
      clock += FRAME_MS;
      act(() => {
        tick(clock);
      });
      frames += 1;
      // The backfill is over once a whole frame mounted nothing new.
      if (rail() === previous && queue.length === 0) break;
      previous = rail();
    }
    return frames;
  };

  let frames = pump();
  if (unfold) {
    // The rule is the only button a folded trace paints.
    const rule = view.container.querySelector("button");
    if (!rule) throw new Error("a folded trace painted no rule to press");
    fireEvent.click(rule);
    frames += pump();
  }
  const segments = rail();
  view.unmount();
  return { frames, segments };
}

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("a trace backfilling the turns a reader is scrolling into", () => {
  // Regression, user report ("this session is so big it will not load"): one turn
  // of a real session held 1,116 iterations, and the trace painted every one of
  // them — measured in Chromium at 393x852, 107,090 px and 23,806 DOM nodes for
  // that turn alone, 180 screens a reader had to drag through. The ramp above
  // decides how FAST the trace mounts; only a fold decides how MUCH of it exists.
  it("stops at the fold instead of mounting a whole turn nobody scrolled to", () => {
    const { frames, segments } = rampFrames(300);

    // The last 24 steps, plus the rule that says how many are behind them.
    expect(segments).toBe(25);
    expect(frames).toBeLessThanOrEqual(20);
  });

  it("hands the rest back in a handful of frames, not one frame per handful", () => {
    const { frames, segments } = rampFrames(300, { unfold: true });

    // A step that triples until it hurts reaches all 300 segments in well
    // under twenty paid frames. The old floor-bound controller needed one
    // frame per two segments.
    expect(segments).toBe(300);
    expect(frames).toBeLessThanOrEqual(20);
  }, 20_000); // The frame-count assertion owns performance, not jsdom wall time.

  it("keeps the first paint small, so opening a session is not the whole turn", () => {
    const queue: FrameRequestCallback[] = [];
    vi.stubGlobal("requestAnimationFrame", (cb: FrameRequestCallback) => {
      queue.push(cb);
      return queue.length;
    });
    vi.stubGlobal("cancelAnimationFrame", () => {});

    const iterations = Array.from({ length: 200 }, (_, index) =>
      iteration(index),
    );
    const view = render(
      <IterationTrace
        iterations={iterations}
        live={false}
        client={client}
        sid="s1"
      />,
    );

    expect(view.container.firstElementChild?.children.length).toBe(8);
    view.unmount();
  });
  // Regression, user report ("it scrolls by itself, God knows where"): the ramp
  // counted the segments it had mounted FROM THE END, so every segment a
  // running turn streamed slid that window down by one and dropped the oldest
  // one on screen. Content above the reader left the scroller for a frame and
  // came back, with the screen's anchor corrector chasing it both ways —
  // measured on an iPhone 17 Pro simulator as a -294 px write followed by
  // +294 px 39 ms later, on a transcript nobody was touching.
  it("never takes back a segment it has already shown", () => {
    const queue: FrameRequestCallback[] = [];
    vi.stubGlobal("requestAnimationFrame", (cb: FrameRequestCallback) => {
      queue.push(cb);
      return queue.length;
    });
    vi.stubGlobal("cancelAnimationFrame", () => {});

    const iterations = Array.from({ length: 40 }, (_, index) =>
      iteration(index),
    );
    const view = render(
      <IterationTrace iterations={iterations} live client={client} sid="s1" />,
    );
    const shown = () =>
      [...(view.container.firstElementChild?.children ?? [])].map(
        (node) => node.textContent ?? "",
      );

    const before = shown();
    expect(before.length).toBe(8);

    // One flush of a turn still being written: a segment at the END.
    view.rerender(
      <IterationTrace
        iterations={[...iterations, iteration(40)]}
        live
        client={client}
        sid="s1"
      />,
    );

    const after = shown();
    expect(after.slice(0, before.length)).toEqual(before);
    expect(after.length).toBe(before.length + 1);
    view.unmount();
  });
});

// @vitest-environment jsdom
import { render } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import type {
  IterationAttachment,
  TranscriptIteration,
} from "../lib/types";

// Regression, user report ("scrolling a big live session hangs on iOS"): one
// streaming delta rebuilds the turn's `iterations` array to grow its tail, so
// `buildSegments` returned a fresh object for every segment and the memo
// boundary — which compared identity — re-rendered the WHOLE trace on every
// flush, ~7 times a second. Measured on a 60-iteration turn: 63 segment renders
// per flush against 1. The settled part of a trace cannot change; only the
// iteration being streamed can.
//
// The rail is the seam that proves it: each segment builds its own attachment
// page, so a segment that re-rendered asks `pageBySize` for one again.
const railPages = vi.fn();
vi.mock("../lib/artifacts", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../lib/artifacts")>();
  return {
    ...original,
    pageBySize: (...args: Parameters<typeof original.pageBySize>) => {
      railPages();
      return original.pageBySize(...args);
    },
  };
});

const { IterationTrace } = await import("./ChatContent");

function attachment(position: number): IterationAttachment {
  return {
    index: 0,
    iteration_id: `i${position}`,
    name: `notes-${position}.txt`,
    size: 12,
    mime: "text/plain",
  } as unknown as IterationAttachment;
}

function iteration(position: number, thinking: string): TranscriptIteration {
  return {
    position,
    id: `i${position}`,
    thinking,
    assistant_prose: `step ${position}`,
    forms: [],
    attachments: [attachment(position)],
  } as unknown as TranscriptIteration;
}

/** The settled iterations keep their identity across a flush, as the reducer leaves them. */
const settled = [iteration(0, "first"), iteration(1, "second")];

/** What `updateLiveIteration` hands the screen for one `content.block.delta`. */
function streamed(tick: number): TranscriptIteration[] {
  return [...settled, iteration(2, `thinking${".".repeat(tick)}`)];
}

const client = {
  base: "http://gateway.example.com",
  retainAttachment: () => () => {},
  attachmentUrl: () => Promise.resolve(null),
} as never;

describe("a trace while its turn streams", () => {
  it("re-renders only the iteration being streamed", () => {
    const view = render(
      <IterationTrace
        iterations={streamed(1)}
        live
        whole
        client={client}
        sid="s1"
      />,
    );
    railPages.mockClear();

    view.rerender(
      <IterationTrace
        iterations={streamed(2)}
        live
        whole
        client={client}
        sid="s1"
      />,
    );

    expect(railPages).toHaveBeenCalledTimes(1);
  });

  it("re-renders a settled segment when its own iteration changes", () => {
    const view = render(
      <IterationTrace
        iterations={streamed(1)}
        live
        whole
        client={client}
        sid="s1"
      />,
    );
    railPages.mockClear();

    view.rerender(
      <IterationTrace
        iterations={[iteration(0, "first"), settled[1], iteration(2, "x")]}
        live
        whole
        client={client}
        sid="s1"
      />,
    );

    expect(railPages).toHaveBeenCalledTimes(2);
  });
});

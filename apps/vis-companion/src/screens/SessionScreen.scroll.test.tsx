// @vitest-environment jsdom
import { act } from "react";
import { fireEvent, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";
import { flushParked } from "../lib/parked";

// Regression, user report ("scrolling a big live session hangs on iOS"): the
// transcript marks the reader's place from its scroll handler — once per
// animation frame for as long as a finger is on the glass — and every one of
// those marks was serialized and handed to `sessionStorage` there and then. A
// synchronous store write inside the gesture is a dropped frame on a phone, and
// the longer the session the more of the budget was already spent.

const READING_POSITIONS = "vis.readingPositions";

/** The geometry a browser would have measured for a long transcript. */
function measured(viewport: HTMLElement): void {
  Object.defineProperty(viewport, "scrollHeight", {
    value: 46_000,
    configurable: true,
  });
  Object.defineProperty(viewport, "clientHeight", {
    value: 800,
    configurable: true,
  });
}

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

describe("scrolling a transcript", () => {
  afterEach(() => {
    vi.restoreAllMocks();
    flushParked();
  });

  it("marks the reader's place without a store write per frame", async () => {
    // Frames are driven by hand so the gesture is exactly as long as it says.
    const frames: FrameRequestCallback[] = [];
    vi.stubGlobal("requestAnimationFrame", (callback: FrameRequestCallback) => {
      frames.push(callback);
      return frames.length;
    });
    vi.stubGlobal("cancelAnimationFrame", () => {});
    const paint = async () => {
      const due = frames.splice(0);
      await act(async () => {
        for (const callback of due) callback(0);
      });
    };

    renderSessionScreen({
      session: sessionFixture({ id: "s1" }),
      client: { transcript: () => Promise.resolve(transcript()) },
    });
    await act(async () => {});
    const viewport = screen.getByRole("region", { name: "Transcript" });
    measured(viewport);
    await paint();

    const writes = vi.spyOn(globalThis.sessionStorage, "setItem");
    for (let frame = 0; frame < 30; frame += 1) {
      viewport.scrollTop = 20_000 + frame * 40;
      fireEvent.scroll(viewport);
      await paint();
    }

    expect(
      writes.mock.calls.filter(([key]) => key === READING_POSITIONS),
    ).toHaveLength(0);

    // And the place is still there for the way back in.
    fireEvent(window, new Event("pagehide"));
    expect(
      JSON.parse(sessionStorage.getItem(READING_POSITIONS) ?? "null"),
    ).toEqual({ s1: 46_000 - (20_000 + 29 * 40) - 800 });
  });
});

// @vitest-environment jsdom
import { act, fireEvent, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { noteReaderGesture } from "../lib/reader-gesture";
import { renderSessionScreen, sessionFixture } from "./session-screen-harness";

const SHELL = 800;
const TURN_PX = 10_000;
const TURNS = 30;

function transcript() {
  return Array.from({ length: TURNS }, (_unused, index) => {
    const position = index + 1;
    return {
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
    };
  });
}

function mountedTurns(viewport: HTMLElement): number {
  return (viewport.textContent?.match(/question \d+/g) ?? []).length;
}

/** Model the transcript geometry that React has mounted for this frame. */
function measure(viewport: HTMLElement): void {
  let top = 0;
  const height = () => mountedTurns(viewport) * TURN_PX + SHELL;
  Object.defineProperty(viewport, "scrollHeight", {
    configurable: true,
    get: height,
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
    },
  });
  viewport.scrollTo = ((options: ScrollToOptions) => {
    top = Math.max(0, Math.min(options.top ?? top, height() - SHELL));
  }) as HTMLElement["scrollTo"];
}

function installFrames() {
  const frames: FrameRequestCallback[] = [];
  vi.stubGlobal("requestAnimationFrame", (callback: FrameRequestCallback) => {
    frames.push(callback);
    return frames.length;
  });
  vi.stubGlobal("cancelAnimationFrame", () => {});
  return async () => {
    for (let round = 0; round < 120; round += 1) {
      const due = frames.splice(0);
      if (!due.length) return;
      await act(async () => {
        for (const callback of due) callback(0);
      });
    }
  };
}

function fromEnd(viewport: HTMLElement): number {
  return viewport.scrollHeight - viewport.scrollTop - SHELL;
}

async function openSession() {
  const paint = installFrames();
  const view = renderSessionScreen({
    session: sessionFixture({ id: "always-latest", status: "running" }),
    client: { transcript: () => Promise.resolve(transcript()) },
  });
  await act(async () => {});
  const viewport = screen.getByRole("region", { name: "Transcript" });
  measure(viewport);
  await paint();
  return { view, viewport, paint };
}

describe("opening an existing session", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
  });

  // Regression, Vis session cbcef612-ea68-4044-b4c6-31c02be374bd: moving
  // through existing sessions reopened each transcript at its remembered offset,
  // leaving the newest answer slightly above the viewport bottom.
  it("opens at the newest turn after leaving from history", async () => {
    const first = await openSession();

    noteReaderGesture();
    first.viewport.scrollTop = first.viewport.scrollHeight - SHELL - 40_000;
    fireEvent.scroll(first.viewport);
    await first.paint();
    expect(fromEnd(first.viewport)).toBe(40_000);
    first.view.unmount();

    const second = await openSession();

    expect(fromEnd(second.viewport)).toBe(0);
    second.view.unmount();
  });
});

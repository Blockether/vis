// @vitest-environment jsdom
import { act, fireEvent, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { renderSessionScreen, sessionFixture } from "./session-screen-harness";
import { noteReaderGesture } from "../lib/reader-gesture";
import { flushParked } from "../lib/parked";
import {
  parkedReadingPosition,
  rememberReadingPosition,
} from "../lib/reading-position";

// Regression, user report (paraphrased: "I am in the live view on iOS. I press
// Latest, it scrolls down nicely — but when I leave the session and come back I
// am at the BEGINNING of the live view instead of at the very bottom, picking up
// the new things"): a reading place is a DISTANCE FROM THE END, and every visit
// rebuilds the same INITIAL_VISIBLE_TURNS window however far back the reader had
// pulled the history in. Measured in the shipped app (WebKit, iPhone 14, a
// 25-turn session): two taps on "Load earlier" grew the transcript to 159 160 px,
// the reader parked 91 349 px above its end, and re-entering rebuilt a 74 555 px
// window — `applyReadingPosition` clamped the impossible distance to scrollTop 0,
// the session's FIRST turn, 73 921 px from its newest one, follow off and
// "Latest" offered. Nothing ever revised that place, so every later visit landed
// on the first turn again.

const SHELL = 800;
/** What one turn of this transcript measures once it is on screen. */
const TURN_PX = 10_000;
const TURNS = 30;

function transcript(count: number = TURNS) {
  return Array.from({ length: count }, (_unused, index) => {
    const position = index + 1;
    return {
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
    };
  });
}

/** How many turns are mounted right now — the transcript's height in rows. */
function mountedTurns(viewport: HTMLElement): number {
  return (viewport.textContent?.match(/question \d+/g) ?? []).length;
}

/**
 * The geometry a phone would have measured.
 *
 * The scroller is exactly as tall as the turns standing in it, so revealing more
 * history moves its END further from its top — which is the whole reason a place
 * taken deeper in the history cannot be honoured by the window a fresh visit
 * builds. And a mounted turn is not a PAINTED one: deferred markdown, syntax
 * highlighting and `content-visibility` land their pixels frames after React
 * commits, so this height follows the rows that have been painted, one per
 * frame. Measured in the shipped app (WebKit, iPhone 14, an eight-turn window):
 * 96 -> 1 898 -> 15 884 -> ... -> 65 976 px across sixteen frames, most of them
 * after the last row had already mounted.
 *
 * Returns the step that paints one more row.
 */
function measure(viewport: HTMLElement): () => void {
  let top = 0;
  let painted = 0;
  const height = () => painted * TURN_PX + SHELL;
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
  // The screen pins to the newest turn with `scrollTo`, and jsdom has no layout
  // to answer it with — so model what a browser does: clamp to the bottom.
  viewport.scrollTo = ((options: ScrollToOptions) => {
    top = Math.max(0, Math.min(options.top ?? top, height() - SHELL));
  }) as HTMLElement["scrollTo"];
  return () => {
    painted = Math.min(mountedTurns(viewport), painted + 1);
  };
}

/**
 * Frames run by hand: the opening ramp hydrates one chunk per frame, and the
 * transcript paints one more of its rows before each of them.
 */
function installFrames(beforeFrame: () => void) {
  const frames: FrameRequestCallback[] = [];
  vi.stubGlobal("requestAnimationFrame", (callback: FrameRequestCallback) => {
    frames.push(callback);
    return frames.length;
  });
  vi.stubGlobal("cancelAnimationFrame", () => {});
  return async (rounds = 120) => {
    for (let round = 0; round < rounds; round += 1) {
      const due = frames.splice(0);
      if (!due.length) return;
      beforeFrame();
      await act(async () => {
        for (const callback of due) callback(0);
      });
    }
  };
}

/** How far the scroller sits above the newest turn. */
function fromEnd(viewport: HTMLElement): number {
  return viewport.scrollHeight - viewport.scrollTop - SHELL;
}

describe("reopening a session someone was reading", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
    flushParked();
  });

  async function reopen(
    sid: string,
    parked: number | null,
    {
      turnCount = TURNS,
      client = {},
      duringOpening,
    }: {
      turnCount?: number;
      client?: Record<string, unknown>;
      duringOpening?: (viewport: HTMLElement, paintOneRow: () => void) => void;
    } = {},
  ) {
    let paintOneRow = () => {};
    const paint = installFrames(() => paintOneRow());
    if (parked !== null) rememberReadingPosition(sid, parked);
    const view = renderSessionScreen({
      session: sessionFixture({ id: sid, status: "running" }),
      client: {
        transcript: () => Promise.resolve(transcript(turnCount)),
        ...client,
      },
    });
    await act(async () => {});
    const viewport = screen.getByRole("region", { name: "Transcript" });
    paintOneRow = measure(viewport);
    duringOpening?.(viewport, paintOneRow);
    await paint();
    return { view, viewport, paint };
  }

  it("reveals the history the place needs instead of clamping to the top", async () => {
    // 120 000 px above the end is twelve turns back — deeper than the eight a
    // fresh visit mounts, and exactly the case that used to land on turn one.
    const { viewport } = await reopen("deep", 120_000);

    expect(fromEnd(viewport)).toBe(120_000);
    expect(mountedTurns(viewport)).toBeGreaterThan(8);
  });

  it("keeps a place the transcript has not finished painting", async () => {
    // Measured live: reopening this session mounted its whole eight-turn window
    // and only THEN grew from 96 px to 65 976 px over sixteen frames. A place
    // 6 000 px above the end fits that transcript with room to spare — but not
    // on the frame its rows mounted, and answering there dropped the reader at
    // the newest turn and threw the place away with it.
    const { viewport } = await reopen("painting", 6_000, { turnCount: 8 });

    expect(fromEnd(viewport)).toBe(6_000);
    expect(parkedReadingPosition("painting")).toBe(6_000);
  });

  it("keeps the place while the gateway's answer is still on its way", async () => {
    // Opening paints the CACHE first — one turn, on the machine this was
    // measured on — and the gateway's own answer lands frames later. A place is
    // impossible against a single cached turn by definition, so answering there
    // threw the reader's place away before their transcript had arrived, and
    // every re-entry after that landed at the newest turn.
    let deliver = () => {};
    const answered = new Promise((resolve) => {
      deliver = () => resolve(transcript(TURNS));
    });
    const { viewport, paint } = await reopen("cached", 40_000, {
      client: {
        cachedTranscript: () => transcript(1),
        transcriptIfMoved: () => answered,
      },
    });

    deliver();
    await act(async () => {});
    await paint();

    expect(fromEnd(viewport)).toBe(40_000);
    expect(parkedReadingPosition("cached")).toBe(40_000);
  });

  it("does not let the opening's own scrolls erase the place", async () => {
    // Measured live (WebKit, iPhone 14): two scroll events landed on a 3 226 px
    // transcript — the opening ramp's own, before the effect that honours a
    // place had so much as read it — and both measured "at the end", which
    // erases the place. From then on the session opened at its newest turn
    // however far back the reader had been.
    const { viewport } = await reopen("opening", 40_000, {
      duringOpening: (scroller, paintOneRow) => {
        paintOneRow();
        scroller.scrollTop = scroller.scrollHeight - SHELL;
        fireEvent.scroll(scroller);
      },
    });

    expect(parkedReadingPosition("opening")).toBe(40_000);
    expect(fromEnd(viewport)).toBe(40_000);
  });
  it("opens at the newest turn, and forgets a place this session cannot hold", async () => {
    // Further back than the whole transcript: whatever was parked no longer
    // addresses anything, and the top is the one place the reader never was.
    const { viewport } = await reopen("gone", (TURNS + 5) * TURN_PX);

    expect(fromEnd(viewport)).toBeLessThanOrEqual(64);
    expect(parkedReadingPosition("gone")).toBe(null);
  });

  it("keeps the place when the screen is torn down before it opens", async () => {
    // Measured live: React's development double-invoke tears the screen down the
    // instant it mounts, and the leave-mark ran against 708 px of transcript
    // nothing had painted yet — a scroller standing at its own end, which parks
    // NOTHING. The place was gone before the effect that honours it had run, and
    // the same teardown happens whenever someone taps a session and leaves again
    // while its transcript is still arriving.
    rememberReadingPosition("torn", 40_000);
    installFrames(() => {});
    const view = renderSessionScreen({
      session: sessionFixture({ id: "torn", status: "running" }),
      client: { transcript: () => Promise.resolve(transcript()) },
    });
    await act(async () => {});
    // Nothing painted: the scroller is its own viewport tall, so it measures as
    // being at the end of a transcript it has not shown a single row of.
    measure(screen.getByRole("region", { name: "Transcript" }));
    view.unmount();

    expect(parkedReadingPosition("torn")).toBe(40_000);
  });
  it("parks nothing when the reader leaves from the newest turn", async () => {
    const { view, viewport, paint } = await reopen("live", null);

    // They read back through the history, so a place is marked from the scroll.
    noteReaderGesture();
    viewport.scrollTop = viewport.scrollHeight - SHELL - 50_000;
    fireEvent.scroll(viewport);
    await paint();
    expect(parkedReadingPosition("live")).toBe(50_000);

    // Then they ride the live turn back down: the growth is carried by the
    // follow, which moves the scroller without a scroll event of its own. The
    // last event still says 50 000 px, and the reader is at the newest turn.
    viewport.scrollTop = viewport.scrollHeight - SHELL;
    view.unmount();

    expect(parkedReadingPosition("live")).toBe(null);
  });
});

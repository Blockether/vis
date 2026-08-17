// @vitest-environment jsdom
import { act, render } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { AssistantMessage } from "./ChatContent";
import type { TranscriptTurn } from "../lib/types";

// Regression, reported as "scrolling a big session is slow, and it flickers":
// a long transcript is ONE paint tree, so every scroll frame costs the whole
// mounted DOM (measured on the iOS simulator, 30 turns / 64 517 nodes: 82 ms a
// frame, and 74 ms for an inert clone of the same DOM — nothing React does
// touches that). Skipping the turns WebKit is not painting is the fix, and the
// attempt before this one was reverted because it GUESSED the skipped height:
// every first reveal corrected the guess, and a correction above the reader
// moves the text under the thumb.
//
// So the invariant is not "no containment", it is "no size that nobody
// measured" — and, because a skipped turn is not laid out and can no longer
// report anything, "nothing changes under a skip without dropping it first".
// And, because arming a box CONTAINS it, a third: "arming changes nothing" —
// the box is a formatting context before the skip goes on, or it grows by its
// own last child's bottom margin at the moment it is armed.
// Arming on first sight broke the second half on the simulator: a 24-turn
// transcript that stands 443 315 px measured 100 701 px, frozen at the height
// it had before its code blocks landed.
const QUIET_MS = 400;

// The shared box observer and the frame it flushes on are made at the first
// measurement and kept for the module's lifetime, so both stand-ins are in
// place before anything renders. A test that needs a FIGURE hands over its own
// geometry — jsdom lays nothing out.
const watchers: { target: Element; fire: () => void }[] = [];

vi.stubGlobal(
  "ResizeObserver",
  class {
    private readonly callback: (entries: { target: Element }[]) => void;
    constructor(callback: (entries: { target: Element }[]) => void) {
      this.callback = callback;
    }
    observe(target: Element) {
      watchers.push({ target, fire: () => this.callback([{ target }]) });
    }
    unobserve(target: Element) {
      const at = watchers.findIndex((watcher) => watcher.target === target);
      if (at >= 0) watchers.splice(at, 1);
    }
    disconnect() {}
    takeRecords() {
      return [];
    }
  },
);

const frames: FrameRequestCallback[] = [];

vi.stubGlobal("requestAnimationFrame", (callback: FrameRequestCallback) => {
  frames.push(callback);
  return frames.length;
});

/** Run the frames queued so far; work they queue waits for the next flush. */
function flushFrames(): void {
  const due = frames.splice(0);
  act(() => {
    for (const callback of due) callback(0);
  });
}

/** Let the quiet period pass, then run the look it scheduled. */
function waitOut(ms = QUIET_MS + 10): void {
  act(() => {
    vi.advanceTimersByTime(ms);
  });
  flushFrames();
}

function resized(target: Element): void {
  for (const watcher of watchers) {
    if (watcher.target === target) watcher.fire();
  }
}

/** The box the browser would report for this turn. */
function layout(box: HTMLElement, width: number, height: number): void {
  Object.defineProperty(box, "offsetWidth", {
    configurable: true,
    value: width,
  });
  box.getBoundingClientRect = () =>
    ({ width, height, top: 0, left: 0, right: width, bottom: height }) as DOMRect;
}

const turn: TranscriptTurn = {
  id: "turn-1",
  status: "completed",
  iterations: [
    { id: "iteration-1", forms: [{ op: "shell", result_summary: "ok" }] },
  ],
} as TranscriptTurn;

function mount(streaming = false) {
  const view = render(<AssistantMessage turn={turn} streaming={streaming} />);
  const box = view.container.querySelector("article") as HTMLElement;
  return { view, box };
}

beforeEach(() => {
  vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
});

afterEach(() => {
  vi.useRealTimers();
  frames.length = 0;
  watchers.length = 0;
});

describe("a finished turn is skipped at its own measured height", () => {
  // Regression, reported as "the part above a new request jumps like anything,
  // in even steps": arming implies `contain:layout`, so a turn that was not
  // already a formatting context grows by its last child's bottom margin the
  // moment it is armed — a turn ending in the notice card that a failed,
  // interrupted or cancelled turn carries measured 7 620.06 px armed against
  // 7 612.06 px unarmed in WebKit. The skip read those 8 px as content landing,
  // dropped, and re-armed a quiet period later, stepping the whole transcript
  // below it 8 px every ~533 ms for as long as that turn stayed on screen.
  // jsdom lays nothing out, so what is pinned here is the precondition itself.
  it("puts the skip on a box arming it cannot resize", () => {
    const { box } = mount();

    // The look this mount queued is drained before the check: the module keeps
    // ONE frame in flight, and a test that leaves it queued makes every later
    // one measure nothing.
    flushFrames();

    expect(box.className.split(/\s+/u)).toContain("flow-root");
  });

  it("skips nothing during the commit that rendered the turn", () => {
    const { box } = mount();
    layout(box, 390, 4321.5);

    // Reading geometry inside that commit would force one full transcript
    // layout per turn, which is the cost this is buying back.
    flushFrames();

    expect(box.style.contentVisibility).toBe("");
  });

  it("arms the skip at the height the turn held still at", () => {
    const { box } = mount();
    layout(box, 390, 4321.5);
    flushFrames();

    waitOut();

    expect(box.style.contentVisibility).toBe("auto");
    expect(box.style.containIntrinsicSize).toBe("auto 4321.5px");
  });

  it("keeps waiting while the turn is still growing", () => {
    const { box } = mount();
    layout(box, 390, 1000);
    flushFrames();

    // The prose is up; the code blocks and pictures are still landing.
    act(() => {
      vi.advanceTimersByTime(200);
    });
    layout(box, 390, 4000);
    resized(box);
    flushFrames();

    // The look scheduled before it grew lands first, and 200 ms of stillness is
    // not stillness.
    waitOut(250);

    expect(box.style.contentVisibility).toBe("");

    waitOut();

    expect(box.style.containIntrinsicSize).toBe("auto 4000px");
  });

  it("never skips the turn that is streaming", () => {
    const { box } = mount(true);
    layout(box, 390, 4321);
    flushFrames();

    waitOut();

    expect(box.style.contentVisibility).toBe("");
    expect(box.style.containIntrinsicSize).toBe("");
  });

  it("arms a live turn once it finishes", () => {
    const { view, box } = mount(true);
    layout(box, 390, 900);

    act(() => {
      view.rerender(<AssistantMessage turn={turn} streaming={false} />);
    });
    flushFrames();
    waitOut();

    expect(box.style.containIntrinsicSize).toBe("auto 900px");
  });

  it("declares nothing for a turn it could not measure", () => {
    const { box } = mount();

    // No geometry handed over: jsdom reports a zero box, which means "not laid
    // out", never "empty" — and a zero-height skip is the worst guess of all.
    flushFrames();
    waitOut();

    expect(box.style.contentVisibility).toBe("");
    expect(box.style.containIntrinsicSize).toBe("");
  });

  it("refuses the placeholder height a skipped turn reports", () => {
    const { box } = mount();
    layout(box, 390, 4321);
    flushFrames();
    waitOut();

    // WebKit is skipping it now, so every metric it reports is the placeholder
    // that was written for it. Learning from that is how a measurement decays
    // back into a guess — and how a reveal corrects the reader's scroll.
    box.checkVisibility = () => false;
    layout(box, 390, 800);
    resized(box);
    flushFrames();

    expect(box.style.containIntrinsicSize).toBe("auto 4321px");
  });

  it("drops the skip when the width changes under it", () => {
    const { box } = mount();
    layout(box, 390, 4321);
    flushFrames();
    waitOut();

    // A rotation (or a split view) makes the remembered height a guess again:
    // the skip goes, the turn lays itself out at the new width, and the next
    // measurement is taken for real.
    box.checkVisibility = () => false;
    layout(box, 844, 4321);
    resized(box);
    flushFrames();

    expect(box.style.contentVisibility).toBe("");
    expect(box.style.containIntrinsicSize).toBe("");

    box.checkVisibility = () => true;
    layout(box, 844, 2600);
    flushFrames();
    waitOut();

    expect(box.style.containIntrinsicSize).toBe("auto 2600px");
  });

  it("drops the skip when content lands under it", async () => {
    const { box } = mount();
    layout(box, 390, 4321);
    flushFrames();
    waitOut();
    expect(box.style.contentVisibility).toBe("auto");

    // Deferred markdown arriving into a subtree nobody is laying out: no
    // resize can report it, so the mutation itself has to.
    box.checkVisibility = () => false;
    box.append(document.createElement("span"));
    await act(async () => {});

    expect(box.style.contentVisibility).toBe("");

    box.checkVisibility = () => true;
    layout(box, 390, 5000);
    flushFrames();
    waitOut();

    expect(box.style.containIntrinsicSize).toBe("auto 5000px");
  });

  it("drops the skip when a picture under it finishes loading", () => {
    const { box } = mount();
    const picture = document.createElement("img");
    box.append(picture);
    layout(box, 390, 4321);
    flushFrames();
    waitOut();
    expect(box.style.contentVisibility).toBe("auto");

    // A picture that lands changes no DOM at all — it just takes more room.
    picture.dispatchEvent(new Event("load"));

    expect(box.style.contentVisibility).toBe("");
  });
});

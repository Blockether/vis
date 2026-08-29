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

// The neighbourhood observer, made once per scroller and kept for the module's
// lifetime like the box observer above. A test moves the reader by hand: jsdom
// scrolls nothing and intersects nothing.
const neighbours: {
  target: Element;
  root: Element | null;
  rootMargin: string;
  report: (near: boolean) => void;
}[] = [];

vi.stubGlobal(
  "IntersectionObserver",
  class {
    private readonly callback: (
      entries: { target: Element; isIntersecting: boolean }[],
    ) => void;
    private readonly options: { root?: Element | null; rootMargin?: string };
    constructor(
      callback: (
        entries: { target: Element; isIntersecting: boolean }[],
      ) => void,
      options: { root?: Element | null; rootMargin?: string } = {},
    ) {
      this.callback = callback;
      this.options = options;
    }
    observe(target: Element) {
      neighbours.push({
        target,
        root: this.options.root ?? null,
        rootMargin: this.options.rootMargin ?? "",
        report: (near: boolean) =>
          this.callback([{ target, isIntersecting: near }]),
      });
    }
    unobserve(target: Element) {
      const at = neighbours.findIndex(
        (neighbour) => neighbour.target === target,
      );
      if (at >= 0) neighbours.splice(at, 1);
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

/** Move the reader: this turn is (or is no longer) inside the warm band. */
function nearby(target: Element, near: boolean): void {
  for (const neighbour of neighbours) {
    if (neighbour.target === target) neighbour.report(near);
  }
  flushFrames();
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
  // The module keeps ONE frame in flight for the whole transcript, and clearing
  // the queue here cannot clear THAT: a test that ends with a look still queued
  // would leave every later test's measurement scheduled against a frame that
  // is never coming. Drain first, then clear.
  for (let pass = 0; pass < 5 && frames.length > 0; pass += 1) flushFrames();
  vi.useRealTimers();
  frames.length = 0;
  watchers.length = 0;
});

describe("a finished turn is skipped at its own measured size", () => {
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

  it("arms the skip at the width and height the turn held still at", () => {
    const { box } = mount();
    layout(box, 390, 4321.5);
    flushFrames();

    waitOut();

    expect(box.style.contentVisibility).toBe("auto");
    expect(box.style.containIntrinsicSize).toBe("auto 390px auto 4321.5px");
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

    expect(box.style.containIntrinsicSize).toBe("auto 390px auto 4000px");
  });

  it("never skips the turn that is streaming", () => {
    const { box } = mount(true);
    layout(box, 390, 4321);
    flushFrames();

    waitOut();

    expect(box.style.contentVisibility).toBe("");
    expect(box.style.containIntrinsicSize).toBe("");
  });

  it("arms a running turn once it finishes", () => {
    const { view, box } = mount(true);
    layout(box, 390, 900);

    act(() => {
      view.rerender(<AssistantMessage turn={turn} streaming={false} />);
    });
    flushFrames();
    waitOut();

    expect(box.style.containIntrinsicSize).toBe("auto 390px auto 900px");
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

    expect(box.style.containIntrinsicSize).toBe("auto 390px auto 4321px");
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

    expect(box.style.containIntrinsicSize).toBe("auto 844px auto 2600px");
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

    expect(box.style.containIntrinsicSize).toBe("auto 390px auto 5000px");
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

// Regression, user report ("scrolling up, the request and response before the
// one I am looking at only load then, and there is a flicker"): a finished turn
// was armed wherever it stood, so the turn one pixel above the viewport was
// skipped — not laid out at all — and the frame that revealed it had to lay it
// out and rasterize it before it could paint anything. What a reader scrolling
// up saw was the placeholder: a correctly sized box of bare paper, and the turn
// a frame or two later.
describe("the turn beside the one being read is never skipped", () => {
  it("measures the band against the scroller, not the window", () => {
    const scroller = document.createElement("div");
    scroller.style.overflowY = "auto";
    const column = document.createElement("div");
    scroller.append(column);
    document.body.append(scroller);

    const view = render(<AssistantMessage turn={turn} />, {
      container: column,
    });
    const box = view.container.querySelector("article") as HTMLElement;
    flushFrames();

    // A root margin only ever expands the ROOT's own rect. Rooted at the
    // window, this band would still be clipped by the scroller — the turn
    // would count as near only once it was already on screen, which is the
    // reveal this exists to get ahead of.
    const watching = neighbours.find((neighbour) => neighbour.target === box);
    expect(watching?.root).toBe(scroller);
    expect(watching?.rootMargin).toBe("100%");

    view.unmount();
    scroller.remove();
  });

  it("refuses to arm the turn the reader is about to reach", () => {
    const { box } = mount();
    layout(box, 390, 4321);
    flushFrames();

    // A screenful below and coming up: this turn is next.
    nearby(box, true);
    waitOut();

    // It then holds still for another whole quiet period, which for a turn
    // anywhere else is the entirety of what arming waits for.
    resized(box);
    waitOut();

    expect(box.style.contentVisibility).toBe("");
    expect(box.style.containIntrinsicSize).toBe("");
  });

  it("gives the skip back a screen before the reader arrives", () => {
    const { box } = mount();
    layout(box, 390, 4321);
    flushFrames();
    waitOut();
    expect(box.style.contentVisibility).toBe("auto");

    nearby(box, true);

    // Laid out now, on a frame nobody is reading, instead of on the frame that
    // puts it under the reader's eyes.
    expect(box.style.contentVisibility).toBe("");
    expect(box.style.containIntrinsicSize).toBe("");
  });

  it("arms again once the reader has left it a screen behind", () => {
    const { box } = mount();
    layout(box, 390, 4321);
    flushFrames();
    nearby(box, true);
    waitOut();
    expect(box.style.contentVisibility).toBe("");

    // Far again — and armed from a fresh measurement, never from the size it
    // was carrying when the reader walked past it.
    layout(box, 390, 5000);
    nearby(box, false);
    waitOut();

    expect(box.style.containIntrinsicSize).toBe("auto 390px auto 5000px");
  });

  it("holds the whole turn before the one on screen, however tall it is", () => {
    const view = render(
      <>
        <AssistantMessage turn={turn} />
        <AssistantMessage turn={{ ...turn, id: "turn-2" }} />
      </>,
    );
    const [above, reading] = [
      ...view.container.querySelectorAll("article"),
    ] as HTMLElement[];
    layout(above, 390, 21778);
    layout(reading, 390, 10694);
    flushFrames();
    waitOut();
    expect(above.style.contentVisibility).toBe("auto");

    // Measured in the browser, one turn of a 30-turn session stands 16 000 to
    // 22 000 px in a 708 px viewport: the reader is inside the turn below this
    // one, so its near edge is a screen and more away while its body is one
    // flick up. The band alone would leave it skipped; being the turn NEXT to
    // the one on screen is what keeps it laid out.
    nearby(reading, true);

    expect(above.style.contentVisibility).toBe("");
    expect(reading.style.contentVisibility).toBe("");
  });
});

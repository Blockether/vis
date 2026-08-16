// @vitest-environment jsdom
import { useRef, useState } from "react";
import { act, render } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import { drag, fireTouch, pullDown } from "./pull-to-search.fixture";
import {
  PULL_HINT_PX,
  PULL_OPEN_PX,
  pullMove,
  pullStart,
  usePullToSearch,
  type PullPhase,
} from "./pull-to-search";

const AT = { x: 180, y: 120 };
const watching = { from: AT, phase: "none" as PullPhase };
const down = (by: number) => ({ x: AT.x, y: AT.y + by });

describe("reading a pull at the top of the list", () => {
  it("watches a single finger on a list that is parked at its top", () => {
    expect(pullStart({ scrollTop: 0 }, 1, AT)).toEqual(watching);
  });

  it("refuses a list that is scrolled, because that pull is a scroll", () => {
    expect(pullStart({ scrollTop: 240 }, 1, AT)).toBeNull();
  });

  it("refuses a second finger, because a pinch is not a question about search", () => {
    expect(pullStart({ scrollTop: 0 }, 2, AT)).toBeNull();
  });

  it("hints once the pull is deliberate and arms once a lift would open it", () => {
    expect(pullMove(watching, 1, down(PULL_HINT_PX - 1))?.phase).toBe("none");
    expect(pullMove(watching, 1, down(PULL_HINT_PX))?.phase).toBe("pulling");
    expect(pullMove(watching, 1, down(PULL_OPEN_PX - 1))?.phase).toBe("pulling");
    expect(pullMove(watching, 1, down(PULL_OPEN_PX))?.phase).toBe("armed");
  });

  it("disarms when the finger comes back up, so the lift can be taken back", () => {
    const armed = pullMove(watching, 1, down(PULL_OPEN_PX))!;
    expect(armed.phase).toBe("armed");
    expect(pullMove(armed, 1, down(PULL_HINT_PX))?.phase).toBe("pulling");
  });

  it("leaves the gesture when the finger scrolls into the list instead", () => {
    expect(pullMove(watching, 1, down(-40))).toBeNull();
  });

  it("gives a sideways drag back to the row it started on", () => {
    expect(pullMove(watching, 1, { x: AT.x + 60, y: AT.y + 8 })).toBeNull();
  });

  it("keeps a pull that merely wanders, since no thumb travels straight", () => {
    expect(pullMove(watching, 1, { x: AT.x + 10, y: AT.y + PULL_OPEN_PX })?.phase).toBe("armed");
  });
});

/** A scroller with the gesture on it, reporting every phase it is told about. */
function PulledList({
  onSearch,
  seen = [],
}: {
  onSearch: (() => void) | null;
  seen?: PullPhase[];
}) {
  const ref = useRef<HTMLDivElement | null>(null);
  const [phase, setPhase] = useState<PullPhase>("none");
  usePullToSearch(ref, (next) => {
    seen.push(next);
    setPhase(next);
  }, onSearch);
  return (
    <div ref={ref} data-testid="list" data-phase={phase} />
  );
}

describe("pulling the sessions list down", () => {
  it("opens the search on the lift that ends an armed pull", () => {
    const onSearch = vi.fn();
    const seen: PullPhase[] = [];
    const { getByTestId } = render(<PulledList onSearch={onSearch} seen={seen} />);

    act(() => pullDown(getByTestId("list"), PULL_OPEN_PX + 20));

    expect(onSearch).toHaveBeenCalledTimes(1);
    expect(seen).toEqual(["pulling", "armed", "none"]);
    expect(getByTestId("list").dataset.phase).toBe("none");
  });

  it("opens nothing for a pull that never reached the threshold", () => {
    const onSearch = vi.fn();
    const { getByTestId } = render(<PulledList onSearch={onSearch} />);

    act(() => pullDown(getByTestId("list"), PULL_OPEN_PX - 10));

    expect(onSearch).not.toHaveBeenCalled();
  });

  it("hints while the finger is down and takes the hint back when it lifts", () => {
    const { getByTestId } = render(<PulledList onSearch={() => {}} />);
    const list = getByTestId("list");

    act(() => {
      fireTouch(list, "touchstart", [AT]);
      fireTouch(list, "touchmove", [down(PULL_OPEN_PX)]);
    });
    expect(list.dataset.phase).toBe("armed");

    act(() => fireTouch(list, "touchend", []));
    expect(list.dataset.phase).toBe("none");
  });

  it("opens nothing when the browser takes the drag away instead of releasing it", () => {
    const onSearch = vi.fn();
    const { getByTestId } = render(<PulledList onSearch={onSearch} />);

    act(() => pullDown(getByTestId("list"), PULL_OPEN_PX + 20, "cancel"));

    expect(onSearch).not.toHaveBeenCalled();
  });

  it("leaves a scrolled list alone", () => {
    const onSearch = vi.fn();
    const seen: PullPhase[] = [];
    const { getByTestId } = render(<PulledList onSearch={onSearch} seen={seen} />);
    // jsdom lays nothing out, so the scroller is simply told where it is parked.
    getByTestId("list").scrollTop = 800;

    act(() => pullDown(getByTestId("list"), PULL_OPEN_PX + 20));

    expect(onSearch).not.toHaveBeenCalled();
    expect(seen).toEqual([]);
  });

  it("says nothing at all while the search page is already the screen", () => {
    const seen: PullPhase[] = [];
    const { getByTestId } = render(<PulledList onSearch={null} seen={seen} />);

    act(() => drag(getByTestId("list"), AT, [down(PULL_OPEN_PX + 40)]));

    expect(seen).toEqual([]);
    expect(getByTestId("list").dataset.phase).toBe("none");
  });
});

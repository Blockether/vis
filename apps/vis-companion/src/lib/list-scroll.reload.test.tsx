// @vitest-environment jsdom
import { useRef } from "react";
import { render } from "@testing-library/react";
import { fireEvent } from "@testing-library/dom";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { useListScrollPark } from "./list-scroll";

// Regression, user report ("I'm at the bottom, I reload, and I'm not at the
// bottom"): the sessions list parked its place ONLY in a module variable, and
// only in the layout cleanup that an unmount runs. A reload unmounts nothing and
// throws the module away, so the one gesture a reader repeats all day was the
// one that lost their place — on a screen whose docstring promised to keep it.

/** A list whose scroller is real enough to be measured, with rows to anchor on. */
function List({ onReaderScrolled = () => {} }: { onReaderScrolled?: () => void }) {
  const ref = useRef<HTMLDivElement | null>(null);
  useListScrollPark(ref, onReaderScrolled);
  return (
    <div ref={ref} data-testid="list">
      {["s1", "s2", "s3"].map((id) => (
        <article key={id} data-session-id={id}>
          {id}
        </article>
      ))}
    </div>
  );
}

/**
 * jsdom lays nothing out, so the scroller is given the geometry a browser would
 * have measured: 1200px down a 5000px list in an 800px window.
 */
function scrolledToRow(element: HTMLElement, top: number): void {
  element.scrollTop = top;
  Object.defineProperty(element, "scrollHeight", { value: 5000, configurable: true });
  Object.defineProperty(element, "clientHeight", { value: 800, configurable: true });
  element.getBoundingClientRect = () => new DOMRect(0, 0, 390, 800);
  for (const row of Array.from(element.querySelectorAll<HTMLElement>("[data-session-id]"))) {
    row.getBoundingClientRect = () => new DOMRect(0, -20, 390, 64);
  }
}

/** A fresh JavaScript context on the same tab: exactly what reload leaves behind. */
async function afterReload() {
  vi.resetModules();
  return import("./list-scroll");
}

describe("the sessions list across a reload", () => {
  beforeEach(() => {
    sessionStorage.clear();
    vi.resetModules();
  });

  it("parks the reader's place when the page goes away without unmounting", async () => {
    const { getByTestId } = render(<List />);
    scrolledToRow(getByTestId("list"), 1200);

    fireEvent(window, new Event("pagehide"));

    expect((await afterReload()).parkedListScroll()).toEqual({
      top: 1200,
      anchor: { id: "s1", offset: -20 },
    });
  });

  it("parks it when the app is backgrounded and may never come back", async () => {
    const { getByTestId } = render(<List />);
    scrolledToRow(getByTestId("list"), 900);

    vi.spyOn(document, "visibilityState", "get").mockReturnValue("hidden");
    fireEvent(document, new Event("visibilitychange"));

    expect((await afterReload()).parkedListScroll()?.top).toBe(900);
  });

  it("still parks it on the unmount that opening a session performs", async () => {
    const { getByTestId, unmount } = render(<List />);
    scrolledToRow(getByTestId("list"), 640);

    unmount();

    expect((await afterReload()).parkedListScroll()?.top).toBe(640);
  });

  it("drops the mark the moment the reader takes over with a finger", async () => {
    const onReaderScrolled = vi.fn();
    const { getByTestId } = render(<List onReaderScrolled={onReaderScrolled} />);
    const list = getByTestId("list");
    scrolledToRow(list, 1200);
    fireEvent(window, new Event("pagehide"));

    fireEvent.touchStart(list);

    expect(onReaderScrolled).toHaveBeenCalled();
    expect((await afterReload()).parkedListScroll()).toBeNull();
  });

  it("keeps nothing for a list that was already at the top", async () => {
    const { getByTestId } = render(<List />);
    scrolledToRow(getByTestId("list"), 0);

    fireEvent(window, new Event("pagehide"));

    expect((await afterReload()).parkedListScroll()).toBeNull();
  });
});

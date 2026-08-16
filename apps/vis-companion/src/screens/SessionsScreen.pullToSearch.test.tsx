// @vitest-environment jsdom
import { act, waitFor } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";
import { PULL_OPEN_PX } from "../lib/pull-to-search";
import { fireTouch, pullDown } from "../lib/pull-to-search.fixture";

// The search page's only door used to be one glass in the far top corner of the app
// bar — the corner a thumb reading the list cannot reach. On a phone the list itself
// is now the door: pull it past its own top and let go.

const AT = { x: 180, y: 120 };
const down = (by: number) => ({ x: AT.x, y: AT.y + by });

/** The list's scroller, once the fleet has painted a row into it. */
async function listOf(view: ReturnType<typeof renderSessionsScreen>) {
  await waitFor(() => {
    expect(view.container.querySelector("[data-session-id]")).not.toBeNull();
  });
  const viewport = view.container.querySelector<HTMLElement>(".overflow-y-auto");
  expect(viewport).not.toBeNull();
  return viewport!;
}

function fleet(onSearch: (() => void) | null) {
  return renderSessionsScreen({
    machines: [{ label: "alpha", sessions: [listSession({ id: "s1", title: "A session" })] }],
    onSearch,
  });
}

describe("pulling the sessions list down", () => {
  it("opens the search page when the pull is released", async () => {
    const onSearch = vi.fn();
    const view = fleet(onSearch);
    try {
      const list = await listOf(view);
      act(() => pullDown(list, PULL_OPEN_PX + 20));

      expect(onSearch).toHaveBeenCalledTimes(1);
    } finally {
      view.restore();
      view.unmount();
    }
  });

  it("says what the lift will do while the finger is still down", async () => {
    const view = fleet(() => {});
    try {
      const list = await listOf(view);
      const hint = () => view.container.querySelector<HTMLElement>(".pointer-events-none.absolute")!;
      expect(hint().className).toContain("opacity-0");

      act(() => {
        fireTouch(list, "touchstart", [AT]);
        fireTouch(list, "touchmove", [down(20)]);
      });
      expect(hint().textContent).toBe("Pull to search");
      expect(hint().className).toContain("opacity-100");

      act(() => fireTouch(list, "touchmove", [down(PULL_OPEN_PX)]));
      expect(hint().textContent).toBe("Release to search");
      expect(hint().className).toContain("text-accent");

      act(() => fireTouch(list, "touchend", []));
      expect(hint().className).toContain("opacity-0");
    } finally {
      view.restore();
      view.unmount();
    }
  });

  it("leaves a list the reader has scrolled alone", async () => {
    const onSearch = vi.fn();
    const view = fleet(onSearch);
    try {
      const list = await listOf(view);
      // jsdom lays nothing out, so the scroller is simply told where it is parked.
      list.scrollTop = 400;

      act(() => pullDown(list, PULL_OPEN_PX + 20));

      expect(onSearch).not.toHaveBeenCalled();
      expect(view.queryByText("Release to search")).toBeNull();
    } finally {
      view.restore();
      view.unmount();
    }
  });

  it("promises nothing while the search page is already the screen", async () => {
    const view = fleet(null);
    try {
      const list = await listOf(view);
      act(() => pullDown(list, PULL_OPEN_PX + 20));

      expect(view.queryByText("Release to search")).toBeNull();
    } finally {
      view.restore();
      view.unmount();
    }
  });
});

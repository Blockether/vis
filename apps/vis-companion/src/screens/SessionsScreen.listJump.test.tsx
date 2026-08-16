// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { act, waitFor } from "@testing-library/react";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

// Regression: with two gateways paired, the sessions list jumped under the reader
// while the fleet was still loading. Every machine is its own round trip and a
// machine with more history than one window patches its rows again per page, so
// projects kept appearing ABOVE the section the reader was looking at and pushed
// it down the glass, with nothing putting the reading position back.

const ROW = 100;

function fleetOf(count: number, prefix: string, perProject: number) {
  return Array.from({ length: count }, (_, index) =>
    listSession({
      id: `${prefix}-${index}`,
      title: `${prefix} session ${index}`,
      workspace: { root: `/Users/dev/${prefix}-p${Math.floor(index / perProject)}` },
      modified_at: new Date(Date.UTC(2024, 4, 1, 0, count - index)).toISOString(),
    }),
  );
}

/** Give jsdom a layout: every listed row is `ROW` tall, in DOM order. */
function measure(viewport: HTMLElement): () => void {
  const real = Element.prototype.getBoundingClientRect;
  const rect = (top: number, height: number) =>
    ({ top, bottom: top + height, left: 0, right: 0, width: 0, height, x: 0, y: top }) as DOMRect;
  Element.prototype.getBoundingClientRect = function (this: Element): DOMRect {
    if (this === viewport) return rect(0, 600);
    const rows = Array.from(document.querySelectorAll("[data-session-id]"));
    const index = rows.indexOf(this);
    return index < 0 ? rect(0, 0) : rect(index * ROW - viewport.scrollTop, ROW);
  };
  return () => {
    Element.prototype.getBoundingClientRect = real;
  };
}

let unmeasure: (() => void) | null = null;

afterEach(() => {
  unmeasure?.();
  unmeasure = null;
});

const ids = () =>
  Array.from(document.querySelectorAll<HTMLElement>("[data-session-id]")).map(
    (row) => row.dataset.sessionId,
  );

describe("the sessions list while a fleet loads", () => {
  it("keeps the row under the top edge when a later page of another machine lands", async () => {
    const view = renderSessionsScreen({
      machines: [
        { label: "alpha", sessions: fleetOf(150, "alpha", 10), holdsPages: true },
        { label: "beta", sessions: fleetOf(3, "beta", 10) },
      ],
    });
    try {
      // Alpha's first window and the whole of beta are on screen; alpha's later
      // pages — five more projects — are still in flight.
      await waitFor(() => {
        expect(ids()).toContain("beta-0");
        expect(ids()).toContain("alpha-99");
      });
      expect(ids()).not.toContain("alpha-149");
      const viewport = view.container.querySelector<HTMLElement>(".overflow-y-auto");
      expect(viewport).not.toBeNull();
      unmeasure = measure(viewport!);

      // The reader is parked with one of beta's rows under the top edge.
      const before = ids().indexOf("beta-0");
      expect(before).toBeGreaterThan(0);
      act(() => {
        viewport!.scrollTop = before * ROW;
      });

      // Alpha's remaining projects land ABOVE everything beta owns.
      view.releasePages();
      await waitFor(() => {
        expect(ids()).toContain("alpha-149");
      });

      const after = ids().indexOf("beta-0");
      expect(after).toBeGreaterThan(before);
      // The parked row is still under the top edge: the list did not move.
      expect(after * ROW - viewport!.scrollTop).toBe(0);
    } finally {
      view.restore();
      view.unmount();
    }
  });
});

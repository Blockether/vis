// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { act, waitFor } from "@testing-library/react";

import { listSession, renderSessionsScreen } from "./sessions-screen-harness";

// Regression: with two gateways paired, the sessions list jumped under the reader
// while the fleet was still loading. Every machine is its own round trip, so a
// machine that answers late lands its whole section ABOVE the one the reader is
// looking at and pushes it down the glass, with nothing putting the reading
// position back.

const ROW = 100;
/** What a project's own header takes, once folding leaves some of them alone. */
const HEADER = 48;

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

/**
 * Give jsdom a layout: in DOM order, a project header takes `HEADER` and every
 * listed row takes `ROW`. Headers are laid out too because a project that arrives
 * FOLDED still takes its band — which is exactly what pushes a reader down now.
 */
function measure(viewport: HTMLElement): () => void {
  const real = Element.prototype.getBoundingClientRect;
  const rect = (top: number, height: number) =>
    ({ top, bottom: top + height, left: 0, right: 0, width: 0, height, x: 0, y: top }) as DOMRect;
  const boxes = () => {
    const laid = new Map<Element, { top: number; height: number }>();
    let cursor = 0;
    for (const node of document.querySelectorAll('section[aria-label$=" sessions"], [data-session-id]')) {
      const height = node.hasAttribute("data-session-id") ? ROW : HEADER;
      laid.set(node, { top: cursor, height });
      cursor += height;
    }
    return laid;
  };
  Element.prototype.getBoundingClientRect = function (this: Element): DOMRect {
    if (this === viewport) return rect(0, 600);
    const box = boxes().get(this);
    return box ? rect(box.top - viewport.scrollTop, box.height) : rect(0, 0);
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
  it("keeps the row under the top edge when a late machine lands its section", async () => {
    const view = renderSessionsScreen({
      machines: [
        { label: "alpha", sessions: fleetOf(150, "alpha", 10), holdsList: true },
        { label: "beta", sessions: fleetOf(3, "beta", 10) },
      ],
    });
    try {
      // Beta has spoken and alpha has not: only beta's rows are on the glass, and
      // alpha's projects — every one of them, counted beside its window — are still
      // in flight.
      await waitFor(() => {
        expect(ids()).toContain("beta-0");
      });
      expect(ids()).not.toContain("alpha-0");
      expect(view.queryByLabelText("Expand alpha-p9")).toBeNull();
      const viewport = view.container.querySelector<HTMLElement>(".overflow-y-auto");
      expect(viewport).not.toBeNull();
      unmeasure = measure(viewport!);
      const laidTop = (id: string) =>
        document
          .querySelector<HTMLElement>(`[data-session-id="${id}"]`)!
          .getBoundingClientRect().top + viewport!.scrollTop;

      // The reader is parked with one of beta's rows under the top edge.
      const before = laidTop("beta-0");
      expect(before).toBeGreaterThan(0);
      act(() => {
        viewport!.scrollTop = before;
      });

      // Alpha answers, and everything it owns lands ABOVE everything beta owns.
      view.releasePages();
      await waitFor(() => {
        expect(view.getByLabelText("Expand alpha-p9")).toBeTruthy();
      });

      expect(laidTop("beta-0")).toBeGreaterThan(before);
      // The parked row is still under the top edge: the list did not move.
      expect(
        document.querySelector<HTMLElement>('[data-session-id="beta-0"]')!.getBoundingClientRect()
          .top,
      ).toBe(0);
    } finally {
      view.restore();
      view.unmount();
    }
  });
});

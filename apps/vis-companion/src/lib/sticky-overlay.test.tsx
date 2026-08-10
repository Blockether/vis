// @vitest-environment jsdom
import { act } from "react";
import { createRoot, type Root } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { ExpandableImage } from "../components/ImageViewer";
import {
  claimOverlayHandover,
  dropOverlayHandovers,
  offerOverlayHandover,
} from "./sticky-overlay";

globalThis.IS_REACT_ACT_ENVIRONMENT = true;

let root: Root;
let host: HTMLDivElement;

beforeEach(() => {
  host = document.createElement("div");
  document.body.append(host);
  root = createRoot(host);
  HTMLCanvasElement.prototype.getContext = vi.fn(() => null) as never;
});

afterEach(() => {
  act(() => root.unmount());
  host.remove();
  dropOverlayHandovers();
});

/**
 * The screen's own swap: while the answer streams the picture lives in the LIVE
 * bubble; the moment the turn settles the same picture is rendered by a settled
 * transcript row instead — a different subtree, so the live one unmounts.
 */
const picture = (
  <ExpandableImage src="blob:picture" alt="chart.png" className="size-8" />
);
const paint = (settled: boolean) =>
  act(() =>
    root.render(
      settled ? (
        <section key="settled">{picture}</section>
      ) : (
        <div key="live">{picture}</div>
      ),
    ),
  );

const viewer = () => document.querySelector('[role="dialog"]');

// Regression, user report: with an artifact open full screen from a still-live
// answer, issuing the answer closed the viewer and dumped the reader back onto
// the final answer.
describe("an artifact opened from a live answer", () => {
  it("stays open when the turn settles under it", () => {
    paint(false);
    act(() =>
      document
        .querySelector<HTMLButtonElement>(
          'button[aria-label="Open chart.png full screen"]',
        )
        ?.click(),
    );
    expect(viewer()).not.toBeNull();

    paint(true);
    expect(viewer()).not.toBeNull();
  });

  it("is handed over only to the row that replaces it, and only at once", () => {
    // Leaving the session drops every outstanding offer, so re-entering it never
    // re-opens what the reader walked away from.
    offerOverlayHandover("image:blob:picture");
    dropOverlayHandovers();
    expect(claimOverlayHandover("image:blob:picture")).toBe(false);

    // And an offer nobody claimed inside the swap window is dead of old age.
    vi.useFakeTimers();
    try {
      vi.setSystemTime(new Date("2025-01-01T00:00:00Z"));
      offerOverlayHandover("image:blob:picture");
      vi.setSystemTime(new Date("2025-01-01T00:00:05Z"));
      expect(claimOverlayHandover("image:blob:picture")).toBe(false);
    } finally {
      vi.useRealTimers();
    }
  });
});

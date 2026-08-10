// @vitest-environment jsdom
import { act } from "react";
import { createRoot, type Root } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { ExpandableImage } from "../components/ImageViewer";
import { dropOverlayHandovers } from "./sticky-overlay";

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

  it("does not re-open once the transcript that owned it is left", () => {
    paint(false);
    act(() =>
      document
        .querySelector<HTMLButtonElement>(
          'button[aria-label="Open chart.png full screen"]',
        )
        ?.click(),
    );
    expect(viewer()).not.toBeNull();

    dropOverlayHandovers();
    paint(true);
    expect(viewer()).toBeNull();
  });
});

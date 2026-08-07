// @vitest-environment jsdom
import { act } from "react";
import { createRoot, type Root } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import type { GatewayClient } from "../lib/gateway";
import { DocOverlay } from "./DocArtifact";

// The pen and the rasteriser are the browser's job; what is under test is the
// WIRING — that an artifact opened from the TRANSCRIPT reaches the annotator,
// and that drawing on a PDF saves under the same filename, which is the next
// version of that artifact.
vi.mock("../lib/pdf-annotate", () => ({
  renderPdfPage: vi.fn(async () => ({
    src: "data:image/png;base64,",
    pageCount: 3,
  })),
  stampPdfPage: vi.fn(async () => new Uint8Array([1, 2, 3])),
}));

vi.mock("./ImageViewer", () => ({
  ImageViewer: ({
    applyLabel,
    onApply,
  }: {
    applyLabel?: string;
    onApply?: (edited: Blob) => void | Promise<void>;
  }) => (
    <button
      type="button"
      aria-label="apply"
      onClick={() => void onApply?.(new Blob(["ink"]))}
    >
      {applyLabel}
    </button>
  ),
}));

globalThis.IS_REACT_ACT_ENVIRONMENT = true;

let root: Root;
let host: HTMLDivElement;

function gatewayStub(version: number) {
  return {
    saveArtifactBytes: vi.fn(async () => ({ version })),
  } as unknown as GatewayClient & {
    saveArtifactBytes: ReturnType<typeof vi.fn>;
  };
}

function press(selector: string) {
  const button = document.querySelector<HTMLButtonElement>(selector);
  if (!button) throw new Error(`no control at ${selector}`);
  act(() => {
    button.click();
  });
}

/** Let the component's own promises (fetch, render, save) settle. */
async function settle() {
  await act(async () => {
    await Promise.resolve();
    await new Promise((done) => setTimeout(done, 0));
  });
}

beforeEach(() => {
  host = document.createElement("div");
  document.body.append(host);
  root = createRoot(host);
});

afterEach(() => {
  act(() => root.unmount());
  host.remove();
  vi.unstubAllGlobals();
});

describe("an artifact opened from the transcript", () => {
  it("renders a markdown note as prose that can be commented on", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => new Response("# Release plan\n\nWe cut on Friday.")),
    );
    await act(async () => {
      root.render(
        <DocOverlay
          name="PLAN.md"
          mime="text/markdown"
          url="blob:note"
          failed={false}
          annotate={{ client: gatewayStub(2), sid: "s1", iterationId: "i1" }}
          onClose={() => undefined}
        />,
      );
    });
    await settle();
    expect(host.textContent).toContain("Select a passage to comment on it.");
    expect(host.querySelector("h1")?.textContent).toBe("Release plan");
  });

  it("draws on a PDF page and saves it as the next version", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => new Response(new Uint8Array([37, 80, 68, 70]))),
    );
    const client = gatewayStub(4);
    await act(async () => {
      root.render(
        <DocOverlay
          name="report.pdf"
          mime="application/pdf"
          url="blob:pdf"
          failed={false}
          annotate={{ client, sid: "s1", iterationId: "i1" }}
          onClose={() => undefined}
        />,
      );
    });

    press('button[aria-label="Draw on page 1"]');
    await settle();
    press('button[aria-label="apply"]');
    await settle();

    expect(client.saveArtifactBytes).toHaveBeenCalledWith(
      "s1",
      "i1",
      "report.pdf",
      "application/pdf",
      expect.anything(),
    );
    expect(host.textContent).toContain("Saved as v4");
  });

  it("stays a plain reader when the artifact cannot be marked up", async () => {
    await act(async () => {
      root.render(
        <DocOverlay
          name="report.pdf"
          mime="application/pdf"
          url="blob:pdf"
          failed={false}
          onClose={() => undefined}
        />,
      );
    });
    expect(document.querySelector('button[aria-label^="Draw on page"]')).toBe(
      null,
    );
  });
});

// @vitest-environment jsdom
import { act } from "react";
import { createRoot, type Root } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import type { GatewayClient } from "../lib/gateway";
import { DocOverlay } from "./DocArtifact";
import { AttachmentRail } from "./ChatContent";
import { resetAnnotationDraftCache } from "../lib/annotation-drafts";

// An opened document keeps unsaved remarks on the device (`lib/annotation-drafts`); these
// tests own what it holds, so its native half is a map they can empty.
const native = vi.hoisted(() => ({ store: new Map<string, string>() }));

vi.mock("@capacitor/preferences", () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({
      value: native.store.get(key) ?? null,
    }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
    remove: async ({ key }: { key: string }) => {
      native.store.delete(key);
    },
  },
}));

// The pen and rasteriser are the browser’s job; these tests hold the wiring from a
// transcript artifact through a visible PDF revision and back to the same filename.
const pdfMocks = vi.hoisted(() => ({
  renders: 0,
  /** A raster a test can hold open, to watch the band DURING a page turn. */
  hold: null as Promise<void> | null,
}));
vi.mock("../lib/pdf-annotate", () => ({
  renderPdfPage: vi.fn(async () => {
    if (pdfMocks.hold) await pdfMocks.hold;
    return {
      src: `data:image/png;base64,page-${++pdfMocks.renders}`,
      pageCount: 3,
    };
  }),
  stampPdfPage: vi.fn(async () => new Uint8Array([1, 2, 3])),
}));

vi.mock("./ImageViewer", () => ({
  ImageViewer: ({
    src,
    applyLabel,
    onApply,
  }: {
    src: string;
    applyLabel?: string;
    onApply?: (edited: Blob) => void | Promise<void>;
  }) => (
    <button
      type="button"
      data-source={src}
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
    saveArtifactText: vi.fn(async () => ({ version })),
  } as unknown as GatewayClient & {
    saveArtifactBytes: ReturnType<typeof vi.fn>;
    saveArtifactText: ReturnType<typeof vi.fn>;
  };
}

function press(selector: string) {
  const button = document.querySelector<HTMLButtonElement>(selector);
  if (!button) throw new Error(`no control at ${selector}`);
  act(() => {
    button.click();
  });
}

function pressText(label: string) {
  const button = Array.from(document.querySelectorAll("button")).find(
    (candidate) => candidate.textContent?.trim() === label,
  );
  if (!button) throw new Error(`no control named ${label}`);
  act(() => {
    button.click();
  });
}

/** React reads a controlled field through the native setter, never `value =`. */
function type(text: string) {
  const field = document.querySelector("textarea");
  if (!field) throw new Error("no field to type into");
  const setter = Object.getOwnPropertyDescriptor(
    window.HTMLTextAreaElement.prototype,
    "value",
  )!.set!;
  act(() => {
    setter.call(field, text);
    field.dispatchEvent(new Event("input", { bubbles: true }));
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
  native.store.clear();
  pdfMocks.renders = 0;
  pdfMocks.hold = null;
  globalThis.localStorage.clear();
  resetAnnotationDraftCache();
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
    expect(host.textContent).not.toContain("Tap a passage");
    expect(host.querySelector("h1")?.textContent).toBe("Release plan");
  });

  // Regression, user report ("why isn't the global save next to that whole close on
  // the header bar"): a note's Save was docked in a footer under the comments, so the
  // document's one verb and its one way out stood at opposite ends of the screen.
  it("saves a note from the band that names it, and the band reports the version", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => new Response("# Release plan\n\nWe cut on Friday.")),
    );
    const client = gatewayStub(7);
    await act(async () => {
      root.render(
        <DocOverlay
          name="PLAN.md"
          mime="text/markdown"
          url="blob:note"
          failed={false}
          annotate={{ client, sid: "s1", iterationId: "i1" }}
          onClose={() => undefined}
        />,
      );
    });
    await settle();

    const band = () => host.querySelector("header")!;
    const saves = () =>
      Array.from(host.querySelectorAll("button")).filter((button) =>
        /^Sav/.test(button.textContent ?? ""),
      );

    // There is ONE of it, it is in the band, and it has nothing to do yet.
    expect(saves()).toHaveLength(1);
    expect(saves()[0].closest("header")).toBe(band());
    expect(saves()[0].disabled).toBe(true);

    pressText("Comment all");
    type("Stale.");
    pressText("Add comment");
    expect(saves()[0].disabled).toBe(false);

    act(() => {
      saves()[0].click();
    });
    await settle();

    expect(client.saveArtifactText).toHaveBeenCalledWith(
      "s1",
      "i1",
      "PLAN.md",
      "text/markdown",
      expect.stringContaining("Stale."),
    );
    // What became of the document is the band's to report, under its name.
    expect(band().textContent).toContain("Saved as v7");
  });
  // Regression, user report: the PDF used the browser’s unconstrained viewer and a saved
  // drawing disappeared because the open reader kept showing the original bytes.
  it("fits a PDF page in the app and shows the saved drawing", async () => {
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
    await settle();

    expect(host.querySelector("iframe")).toBe(null);
    const page = host.querySelector<HTMLImageElement>(
      'img[aria-label="Page 1 of report.pdf"]',
    );
    expect(page?.src).toContain("page-1");

    press('button[aria-label="Annotate page 1"]');
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
    expect(page?.src).toContain("page-2");
  });

  // Reported: the pager and the pen stood in a strip of their own under the page, at the
  // far end of the screen from the ✕, with the position printed between them.
  it("carries the pager and the pen in the band that names the file", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => new Response(new Uint8Array([37, 80, 68, 70]))),
    );
    await act(async () => {
      root.render(
        <DocOverlay
          name="report.pdf"
          mime="application/pdf"
          url="blob:pdf"
          failed={false}
          annotate={{ client: gatewayStub(2), sid: "s1", iterationId: "i1" }}
          onClose={() => undefined}
        />,
      );
    });
    await settle();

    const band = host.querySelector("header")!;
    for (const label of ["Previous page", "Next page", "Annotate page 1"]) {
      expect(band.querySelector(`button[aria-label="${label}"]`)).not.toBe(
        null,
      );
    }
    // Where the reader stands is the band’s to report, under the filename.
    expect(band.textContent).toContain("Page 1 of 3");

    press('button[aria-label="Next page"]');
    await settle();
    expect(band.textContent).toContain("Page 2 of 3");
    expect(band.querySelector('button[aria-label="Annotate page 2"]')).not.toBe(
      null,
    );
  });

  // Regression, user report: clicking ‹ › flickered the very cells being clicked,
  // Annotate included — a page turn re-fetched the whole document and disabled all
  // three cells until the new raster arrived.
  it("turns the page without dimming the band or fetching the file again", async () => {
    const fetched = vi.fn(
      async () => new Response(new Uint8Array([37, 80, 68, 70])),
    );
    vi.stubGlobal("fetch", fetched);
    await act(async () => {
      root.render(
        <DocOverlay
          name="report.pdf"
          mime="application/pdf"
          url="blob:pdf"
          failed={false}
          annotate={{ client: gatewayStub(2), sid: "s1", iterationId: "i1" }}
          onClose={() => undefined}
        />,
      );
    });
    await settle();
    expect(fetched).toHaveBeenCalledTimes(1);

    // Hold the next raster open: what the band looks like DURING a turn is the report.
    let draw = () => {};
    pdfMocks.hold = new Promise<void>((done) => {
      draw = () => done();
    });
    press('button[aria-label="Next page"]');
    await settle();

    const band = host.querySelector("header")!;
    const cells = Array.from(band.querySelectorAll("button")).filter((cell) =>
      /page/i.test(cell.getAttribute("aria-label") ?? ""),
    );
    expect(cells).toHaveLength(3);
    expect(
      cells.filter((cell) => cell.disabled).map((cell) => cell.ariaLabel),
    ).toEqual([]);
    // The document is in memory: turning a page never goes back to the network.
    expect(fetched).toHaveBeenCalledTimes(1);

    draw();
    pdfMocks.hold = null;
    await settle();
    expect(band.textContent).toContain("Page 2 of 3");
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
    expect(document.querySelector('button[aria-label^="Annotate page"]')).toBe(
      null,
    );
  });
});

// A REMARK LIVES IN THE FILE, SO A CUT IS READ WITH THE REMARKS IT CARRIES.
//
// Asked once the transcript collapsed a revised note into one row: and stepping back
// into those versions, are the comments there too? Nothing pinned it. A remark is not
// metadata beside the artifact, it is a `## Comments` section INSIDE its bytes
// (`lib/markdown-annotations`), and the band's version cell hands the tile another cut
// to FETCH — so what comes back must be that cut's prose and that cut's remarks, never
// the newest ones painted over an older document.
describe("a document read back through its own versions", () => {
  const cut = (index: number, version: number, size: number) => ({
    filename: "PLAN.md",
    media_type: "text/markdown",
    size,
    iteration_id: "i1",
    index,
    version,
  });

  const bytes: Record<string, string> = {
    "blob:PLAN.md#0":
      "# Release plan\n\nWe cut on Friday.\n\n## Comments\n\n- **Whole document** — Ship the rail collapse first.\n",
    "blob:PLAN.md#1":
      "# Release plan\n\nWe cut on Monday.\n\n## Comments\n\n- **Whole document** — The band reads better now.\n",
  };

  it("opens an older cut with the comments that cut was saved with", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (url: string) => new Response(bytes[String(url)] ?? "")),
    );
    const asked: number[] = [];
    const client = {
      attachmentUrl: async (
        _sid: string,
        _iteration: string,
        index: number,
      ) => {
        asked.push(index);
        return `blob:PLAN.md#${index}`;
      },
      retainAttachment: () => () => {},
    } as unknown as GatewayClient;

    await act(async () => {
      root.render(
        <AttachmentRail
          client={client}
          sid="s1"
          attachments={[cut(0, 1, 12_700), cut(1, 2, 12_400)]}
        />,
      );
    });
    await settle();

    press('[aria-label="Open PLAN.md"]');
    await settle();
    // The one row opens on the NEWEST cut, and that cut's own remark is in the document.
    expect(document.body.textContent).toContain("We cut on Monday.");
    expect(document.body.textContent).toContain("The band reads better now.");

    press('[aria-label="Versions of PLAN.md"]');
    await settle();
    press('[aria-label="Read v1 of PLAN.md"]');
    await settle();

    // v1 is fetched by ITS OWN index, and arrives with its prose and its remark.
    expect(asked).toEqual([1, 0]);
    expect(document.body.textContent).toContain("We cut on Friday.");
    expect(document.body.textContent).toContain(
      "Ship the rail collapse first.",
    );
    expect(document.body.textContent).not.toContain(
      "The band reads better now.",
    );
    expect(
      document.querySelector('[aria-label="Versions of PLAN.md"]')?.textContent,
    ).toContain("v1 of 2");
  });
});

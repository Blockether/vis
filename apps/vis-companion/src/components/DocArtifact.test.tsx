// @vitest-environment jsdom
import { render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import {
  DocFrame,
  DocOverlay,
  DocPreview,
  DocStack,
  docSandbox,
  docStackSummary,
} from "./DocArtifact";

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, " ")
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, ">")
    .replace(/&lt;/g, "<")
    .replace(/&amp;/g, "&");

// An attached page is UNTRUSTED markup. It renders in an iframe, which is its
// own document with its own CSS scope, and the sandbox is what makes that a
// security boundary: a blob: URL inherits the app's origin, so `allow-same-origin`
// would hand the artifact the app's storage and the gateway's bearer token.
describe("sandboxing", () => {
  it("never grants an artifact the app origin", () => {
    expect(docSandbox("text/html")).not.toContain("allow-same-origin");
    expect(docSandbox("application/pdf")).not.toContain("allow-same-origin");
  });

  it("never lets an artifact act on the app around it", () => {
    for (const mime of [
      "text/html",
      "application/xhtml+xml",
      "application/pdf",
    ]) {
      expect(docSandbox(mime)).not.toContain("allow-top-navigation");
      expect(docSandbox(mime)).not.toContain("allow-popups");
    }
  });

  it("runs a page's own script and allows the browser PDF viewer", () => {
    expect(docSandbox("text/html")).toContain("allow-scripts");
    expect(docSandbox("application/xhtml+xml")).toContain("allow-scripts");
    expect(docSandbox("application/pdf")).toBe("allow-scripts");
  });

  it("paints the artifact inside a sandboxed frame", () => {
    const html = renderToStaticMarkup(
      <DocFrame url="blob:x" mime="text/html" name="page.html" />,
    );
    expect(html).toContain("<iframe");
    expect(html).toContain('sandbox="allow-scripts');
    expect(html).toContain('title="page.html"');
    expect(html).not.toContain("allow-same-origin");
  });
});

// Regression: a document artifact stood in the transcript twice — the `vis-doc`
// fence painted a card and the attachment rail painted a tile for the very same
// file — and the tile embedded the bytes, so a note taller than the turn that
// produced it had to be scrolled past to reach the next line of the
// conversation.
describe("DocPreview", () => {
  const preview = (mime = "application/pdf", name = "report.pdf") =>
    renderToStaticMarkup(
      <DocPreview
        name={name}
        mime={mime}
        sizeLabel="1.2 MB"
        url="blob:x"
        failed={false}
        onNeeded={() => undefined}
      />,
    );

  it("names the artifact without painting a byte of it", () => {
    const html = preview();
    expect(text(html)).toContain("report.pdf");
    expect(text(html)).toContain("1.2 MB");
    expect(html).not.toContain("<iframe");
    expect(html).not.toContain("60vh");
  });

  it("shows a note exactly as small as it shows a PDF", () => {
    const note = preview("text/markdown", "PLAN.md");
    expect(note).not.toContain("<iframe");
    expect(note).not.toContain("60vh");
    expect(text(note)).toContain("PLAN.md");
  });

  it("offers no draw, hide or new tab control", () => {
    const body = text(preview());
    expect(body).not.toContain("Draw");
    expect(body).not.toContain("Hide");
    expect(body).not.toContain("New tab");
  });

  // Reported ("I love 4"): the card carried the screen's only verb as a chip at its
  // far trailing edge, so the other nine tenths of it was paper a finger could land
  // on for nothing. The ROW is the button now, and it keeps a finger's 48px box while
  // only a cursor tightens it.
  it("is itself the control that opens the document", () => {
    const html = preview();
    expect(html).toContain('aria-label="Open report.pdf"');
    expect(html).toContain("min-h-12");
    expect(html).toContain("px-3");
    expect(html).toContain("w-full");
    // One press target in the row, and no second word beside it.
    expect(html.match(/<button/g)).toHaveLength(1);
    expect(text(html)).not.toContain("Open ");
  });

  // A row is a row of the stack: the frame, the paper and the edge belong to the
  // stack around it, or four documents are four boxes down one turn.
  it("carries no frame of its own", () => {
    const html = preview();
    expect(html).not.toContain("border-code-edge");
    expect(html).not.toContain("mt-2");
  });
});

// Reported ("I love 4"): every document artifact was its own framed card with its own
// kind chip and its own Open, so a step that wrote four files read as four settings
// panels stacked down the transcript instead of as one thing the step produced.
describe("DocStack", () => {
  const stack = (summary?: string) =>
    renderToStaticMarkup(<DocStack summary={summary}>rows</DocStack>);

  it("is the one frame the rows live in", () => {
    const html = stack();
    expect(html).toContain("border border-code-edge");
    expect(html).toContain("divide-y");
  });

  it("reports the group, and only when there is a group", () => {
    expect(text(stack("4 documents · 31.4KB"))).toContain(
      "4 documents · 31.4KB",
    );
    // A lone document is one row and no header band at all.
    expect(stack()).not.toContain("border-b");
  });

  it("claims a weight only when every document reported one", () => {
    expect(docStackSummary([{ size: 1024 }, { size: 2048 }])).toBe(
      "2 documents · 3.0KB",
    );
    expect(docStackSummary([{ size: 1024 }, {}])).toBe("2 documents");
    expect(docStackSummary([{ size: 1024 }])).toBe("1 document · 1.0KB");
  });
});

// Regression: the opened document was read in a letterbox — the frame sized
// itself to 60vh inside a padded scroller — and the session's composer strip
// still stood at the bottom of the screen over it.
describe("an opened document", () => {
  const overlay = () =>
    renderToStaticMarkup(
      <DocOverlay
        name="report.pdf"
        mime="application/pdf"
        sizeLabel="1.2 MB"
        url="blob:x"
        failed={false}
        onClose={() => undefined}
      />,
    );

  it("owns the whole viewport", () => {
    const markup = overlay();
    expect(markup).toContain("inset-0");
    expect(markup).toContain("h-full");
    expect(markup).not.toContain("h-[100dvh]");
    expect(markup).toContain("flex-1");
    expect(markup).not.toContain("60vh");
    expect(markup).toContain("w-full");
    expect(markup).toContain('aria-label="Close report.pdf"');
  });

  // One document per test: `useStickyOverlay` remembers an opened document BY NAME
  // across mounts, so a second test reusing the name opens on arrival.
  const openPreview = async (name: string) => {
    const view = render(
      <DocPreview
        name={name}
        mime="application/pdf"
        sizeLabel="1.2 MB"
        url="blob:x"
        failed={false}
        onNeeded={() => undefined}
      />,
    );
    await userEvent.click(screen.getByText(name));
    const close = document.querySelector(`[aria-label="Close ${name}"]`)!;
    expect(close).toBeTruthy();
    return { view, screenEl: close.closest("div[class*=inset-0]") as HTMLElement };
  };

  // Regression, user report ("the input should be ABOVE THE KEYBOARD"): this screen
  // hung off `document.body` at `100dvh`. The keyboard pins the app SHELL, not the
  // glass (`useVisualViewportShell`), so the annotator's composer sat under the keys
  // on iOS and on mobile web alike.
  it("mounts inside the viewport-pinned shell, so a keyboard cannot bury it", async () => {
    const shell = document.createElement("div");
    shell.setAttribute("data-viewport-shell", "");
    document.body.append(shell);

    const { view, screenEl } = await openPreview("shelled.pdf");
    expect(shell.contains(screenEl)).toBe(true);
    expect(view.container.contains(screenEl)).toBe(false);
    expect(screenEl.className).toContain("absolute");
    expect(screenEl.className).not.toContain("fixed");

    view.unmount();
    shell.remove();
  });

  // No shell yet — the layer still leaves the transcript, so the composer strip the
  // session screen pins to the bottom cannot paint on top of it.
  it("falls back to the body, fixed, when there is no shell", async () => {
    const { view, screenEl } = await openPreview("bodied.pdf");
    expect(view.container.contains(screenEl)).toBe(false);
    expect(screenEl.closest("body")).toBe(document.body);
    expect(screenEl.className).toContain("fixed");
    view.unmount();
  });

  it("fills its box instead of standing at 60vh", () => {
    const markup = renderToStaticMarkup(
      <DocFrame url="blob:x" mime="application/pdf" name="q3.pdf" />,
    );
    expect(markup).toContain("flex-1");
    expect(markup).not.toContain("60vh");
  });
});

// Regression, user report: "the model attached me a document, then I commented on
// it and saved, and suddenly instead of the original one I see two". The rail now
// paints ONE row per name (`collapseAttachmentVersions`), so the row itself has to
// say that the version moved — and the thread it stands for opens from the band of
// the document it opens, never from a second row in the transcript.
describe("a revised document", () => {
  const cuts = [
    {
      filename: "PLAN.md",
      index: 1,
      iteration_id: "i1",
      version: 2,
      size: 12_700,
    },
    {
      filename: "PLAN.md",
      index: 0,
      iteration_id: "i1",
      version: 1,
      size: 13_000,
    },
  ];

  it("says the version moved in the row the document already had", () => {
    const html = renderToStaticMarkup(
      <DocPreview
        name="PLAN.md"
        mime="text/markdown"
        sizeLabel="12.4KB"
        url="blob:x"
        failed={false}
        versions={cuts}
        onNeeded={() => undefined}
      />,
    );
    expect(text(html)).toContain("v2");
    // A revision is a LINE in the row, never a second row: one name, one chevron.
    expect(html.split('aria-label="Open PLAN.md"').length - 1).toBe(1);
  });

  it("leaves a file written once with nothing to report", () => {
    const html = renderToStaticMarkup(
      <DocPreview
        name="ONCE.md"
        mime="text/markdown"
        sizeLabel="1.0KB"
        url="blob:x"
        failed={false}
        versions={[cuts[1]]}
        onNeeded={() => undefined}
      />,
    );
    expect(text(html)).not.toContain("v1");
  });

  it("opens the thread from the BAND and reads the cut that is picked", async () => {
    const picked: number[] = [];
    render(
      <DocOverlay
        name="THREAD.md"
        mime="text/markdown"
        sizeLabel="12.4KB"
        url={null}
        failed={false}
        versions={cuts}
        shownAt={0}
        onPick={(at) => picked.push(at)}
        onClose={() => undefined}
      />,
    );
    const cell = screen.getByLabelText("Versions of THREAD.md");
    expect(cell.textContent).toContain("v2 of 2");

    await userEvent.click(cell);
    expect(screen.getByText("2 versions")).toBeTruthy();

    await userEvent.click(screen.getByLabelText("Read v1 of THREAD.md"));
    expect(picked).toEqual([1]);
    // Picking closes the thread again: the document is what the screen is for.
    expect(screen.queryByText("2 versions")).toBeNull();
  });

  it("gives a document with one cut no version cell at all", () => {
    render(
      <DocOverlay
        name="LONE.md"
        mime="text/markdown"
        sizeLabel="1.0KB"
        url={null}
        failed={false}
        versions={[cuts[1]]}
        onClose={() => undefined}
      />,
    );
    expect(screen.queryByLabelText("Versions of LONE.md")).toBeNull();
  });
});

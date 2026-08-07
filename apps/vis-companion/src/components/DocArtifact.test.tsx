import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import { DocFrame, DocOverlay, DocPreview, docSandbox } from "./DocArtifact";
import docArtifactSource from "./DocArtifact.tsx?raw";

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

  it("runs no script for a page and allows only the browser PDF viewer", () => {
    expect(docSandbox("text/html")).toBe("");
    expect(docSandbox("application/xhtml+xml")).toBe("");
    expect(docSandbox("application/pdf")).toBe("allow-scripts");
  });

  it("paints the artifact inside a sandboxed frame", () => {
    const html = renderToStaticMarkup(
      <DocFrame url="blob:x" mime="text/html" name="page.html" />,
    );
    expect(html).toContain("<iframe");
    expect(html).toContain('sandbox=""');
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

  // The one control the card carries is the app's own `Button` — a secondary
  // (`secondary`) at the header rhythm — not a chip face spelled out here. A compact
  // button keeps the 44px target on touch through its own invisible `::after`.
  it("carries an Open button from the shared vocabulary", () => {
    const html = preview();
    expect(text(html)).toContain("Open");
    expect(html).toContain("border-edge-strong");
    expect(html).toContain("after:-top-1.5");
    expect(html).not.toContain("bg-button");
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
    expect(markup).toContain("fixed inset-0");
    expect(markup).toContain("h-[100dvh]");
    expect(markup).toContain("flex-1");
    expect(markup).not.toContain("60vh");
    expect(markup).toContain("w-full");
    expect(markup).toContain('aria-label="Close report.pdf"');
  });

  it("is portalled to the document body, so the composer cannot cover it", () => {
    expect(docArtifactSource).toContain("createPortal(");
    expect(docArtifactSource).toContain("document.body,");
  });

  it("fills its box instead of standing at 60vh", () => {
    const markup = renderToStaticMarkup(
      <DocFrame url="blob:x" mime="application/pdf" name="q3.pdf" />,
    );
    expect(markup).toContain("flex-1");
    expect(markup).not.toContain("60vh");
  });
});

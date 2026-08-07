import { renderToStaticMarkup } from "react-dom/server";
import { describe, expect, it } from "vitest";
import {
  DocCard,
  DocFrame,
  DocOverlay,
  DocPreview,
  docSandbox,
  parseDocBlock,
} from "./DocArtifact";

/** The block `vis_attach` emits for a document artifact: five header lines, no payload. */
const fence = [
  "[Document: report.pdf PDF, 1.2 MB]",
  "/tmp/vis-python/doc-1/report.pdf",
  "application/pdf",
  "report.pdf",
  "1.2 MB",
].join("\n");

/** Visible text of a rendered chunk: tags out, entities back. */
const text = (html: string) =>
  html
    .replace(/<[^>]+>/g, " ")
    .replace(/&quot;/g, '"')
    .replace(/&#x27;/g, "'")
    .replace(/&gt;/g, ">")
    .replace(/&lt;/g, "<")
    .replace(/&amp;/g, "&");

describe("vis-doc fence", () => {
  it("splits the five header lines", () => {
    const artifact = parseDocBlock(fence);
    expect(artifact.summary).toBe("[Document: report.pdf PDF, 1.2 MB]");
    expect(artifact.path).toBe("/tmp/vis-python/doc-1/report.pdf");
    expect(artifact.mime).toBe("application/pdf");
    expect(artifact.name).toBe("report.pdf");
    expect(artifact.sizeLabel).toBe("1.2 MB");
  });

  it("falls back to the path basename when the name line is missing", () => {
    const artifact = parseDocBlock(
      ["[Document: page.html HTML, 2 KB]", "/tmp/page.html", "text/html"].join(
        "\n",
      ),
    );
    expect(artifact.name).toBe("page.html");
    expect(artifact.sizeLabel).toBe("");
  });

  it("survives a body that carries no header at all", () => {
    const artifact = parseDocBlock("");
    expect(artifact.path).toBe("");
    expect(artifact.name).toBe("document");
  });
});

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

describe("DocCard", () => {
  it("states what was produced and where it landed", () => {
    const body = text(renderToStaticMarkup(<DocCard body={fence} compact />));
    expect(body).toContain("PDF");
    expect(body).toContain("report.pdf");
    expect(body).toContain("application/pdf");
    expect(body).toContain("1.2 MB");
    expect(body).toContain("/tmp/vis-python/doc-1/report.pdf");
  });

  it("carries no frame of its own when a card already draws one", () => {
    const framed = renderToStaticMarkup(<DocCard body={fence} compact />);
    const bare = renderToStaticMarkup(
      <DocCard body={fence} compact frameless />,
    );
    expect(framed).toContain("border border-code-edge");
    expect(bare.startsWith('<div class="my-2 flex w-full')).toBe(true);
  });

  it("never embeds the artifact itself — the transcript ships descriptors only", () => {
    expect(
      renderToStaticMarkup(<DocCard body={fence} compact />),
    ).not.toContain("<iframe");
  });
});
// A document is READ where it sits: no capture strip, no collapse toggle and no
// new-tab escape hatch — the frame is the whole surface.
describe("DocPreview", () => {
  const preview = () =>
    renderToStaticMarkup(
      <DocPreview
        name="report.pdf"
        mime="application/pdf"
        sizeLabel="1.2 MB"
        url="blob:x"
        failed={false}
        onNeeded={() => undefined}
      />,
    );

  it("names the artifact and paints it in the sandboxed frame", () => {
    const html = preview();
    expect(text(html)).toContain("report.pdf");
    expect(text(html)).toContain("1.2 MB");
    expect(html).toContain("<iframe");
  });

  it("offers no draw, hide or new tab control", () => {
    const body = text(preview());
    expect(body).not.toContain("Draw");
    expect(body).not.toContain("Hide");
    expect(body).not.toContain("New tab");
  });

  // The one control the card carries wears the transcript's own chip face, the
  // same one `Copy` wears in a tool result.
  it("carries an Open chip shaped like the copy chip", () => {
    const html = preview();
    expect(text(html)).toContain("Open");
    expect(html).toContain("border-dialog-edge");
    expect(html).toContain("bg-button");
    expect(html).toContain("text-button-foreground");
  });

  it("opens the document over the whole viewport", () => {
    const markup = renderToStaticMarkup(
      <DocOverlay
        name="report.pdf"
        mime="application/pdf"
        sizeLabel="1.2 MB"
        url="blob:x"
        failed={false}
        onClose={() => undefined}
      />,
    );
    expect(markup).toContain("fixed inset-0");
    expect(markup).toContain("flex-1");
    expect(markup).not.toContain("60vh");
    expect(markup).toContain("w-full");
    expect(text(markup)).toContain("Close");
  });

  // A markdown note read by the app used to grow without bound inside the turn
  // that produced it, while the PDF beside it stood in a 60vh frame: one preview,
  // two heights.
  it("stands in the same bounded box whatever it holds", () => {
    expect(preview()).toContain("max-h-[60vh]");
    expect(
      renderToStaticMarkup(
        <DocPreview
          name="PLAN.md"
          mime="text/markdown"
          sizeLabel="6.4KB"
          url="blob:x"
          failed={false}
          onNeeded={() => undefined}
        />,
      ),
    ).toContain("max-h-[60vh]");
  });
});

// Regression: an opened attachment was read in a letterbox — the document frame
// sized itself to 60vh (capped at 34rem) inside a padded scroller, so a PDF or a
// log opened from the artifacts sheet left the app's paper above and below it
// instead of using the whole screen.
describe("an opened document", () => {
  it("fills its box instead of standing at 60vh", () => {
    const markup = renderToStaticMarkup(
      <DocFrame url="blob:x" mime="application/pdf" name="q3.pdf" fill />,
    );
    expect(markup).toContain("flex-1");
    expect(markup).not.toContain("60vh");
  });

  it("keeps the bounded height when it is only a preview in the transcript", () => {
    const markup = renderToStaticMarkup(
      <DocFrame url="blob:x" mime="application/pdf" name="q3.pdf" />,
    );
    expect(markup).toContain("60vh");
  });
});

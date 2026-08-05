import { renderToStaticMarkup } from "react-dom/server";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  clampArtifactText,
  readArtifactText,
  TEXT_ARTIFACT_LIMIT,
  TextBody,
} from "./TextArtifact";

const html = (node: Parameters<typeof renderToStaticMarkup>[0]) =>
  renderToStaticMarkup(node);

afterEach(() => {
  vi.unstubAllGlobals();
});

// Regression: a markdown artifact was handed to the document iframe, which
// painted `# Heading` as `# Heading` — the source of the note instead of the
// note — and the app has exactly one markdown renderer that already knows
// better.
describe("a written artifact", () => {
  it("renders markdown as prose, not as its own source", () => {
    const markup = html(
      <TextBody
        text={"# Ship it\n\n- one\n- two\n"}
        mime="text/markdown"
        name="vis-issue-115-comment.md"
      />,
    );
    expect(markup).toContain("<h1");
    expect(markup).toContain("Ship it");
    expect(markup).toContain("<li");
    expect(markup).not.toContain("# Ship it");
  });

  it("trusts the name when the gateway only guessed a generic type", () => {
    const markup = html(
      <TextBody
        text={"## Heading"}
        mime="application/octet-stream"
        name="notes.md"
      />,
    );
    expect(markup).toContain("<h2");
  });

  it("paints anything else verbatim, wrapped and monospaced", () => {
    const markup = html(
      <TextBody
        text={"# not markdown\n  spaced"}
        mime="text/plain"
        name="a.log"
      />,
    );
    expect(markup).toContain("<pre");
    expect(markup).toContain("whitespace-pre-wrap");
    expect(markup).toContain("# not markdown");
  });
});

describe("reading the bytes", () => {
  it("never asks a phone to lay out a whole log", () => {
    const clamped = clampArtifactText("x".repeat(TEXT_ARTIFACT_LIMIT + 10));
    expect(clamped.length).toBeLessThan(TEXT_ARTIFACT_LIMIT + 40);
    expect(clamped).toContain("… truncated");
    expect(clampArtifactText("short")).toBe("short");
  });

  it("reads the artifact url as text", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => new Response("# hi", { status: 200 })),
    );
    expect(await readArtifactText("blob:one")).toBe("# hi");
  });

  it("refuses a failed read instead of painting an empty note", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => new Response("nope", { status: 404 })),
    );
    await expect(readArtifactText("blob:two")).rejects.toThrow("404");
  });
});

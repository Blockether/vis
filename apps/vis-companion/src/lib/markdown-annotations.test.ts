import { describe, expect, it } from "vitest";

import {
  parseAnnotated,
  quoteOf,
  renderAnnotated,
  QUOTE_LIMIT,
} from "./markdown-annotations";

describe("markdown annotations", () => {
  it("reads a plain note as prose with no comments", () => {
    const parsed = parseAnnotated("# Title\n\nBody.\n");
    expect(parsed.body).toBe("# Title\n\nBody.\n");
    expect(parsed.comments).toEqual([]);
  });

  it("round-trips comments through the document itself", () => {
    const saved = renderAnnotated("# Title\n\nBody.", [
      { quote: "Body.", body: "Say more here." },
      { quote: "Title", body: "Rename this." },
    ]);
    expect(saved).toContain("## Comments");
    const parsed = parseAnnotated(saved);
    expect(parsed.body).toBe("# Title\n\nBody.");
    expect(parsed.comments).toEqual([
      { quote: "Body.", body: "Say more here." },
      { quote: "Title", body: "Rename this." },
    ]);
  });

  it("drops the section again when the last comment is removed", () => {
    expect(renderAnnotated("Body.", [])).toBe("Body.\n");
    expect(renderAnnotated("Body.", [{ quote: "Body.", body: "  " }])).toBe(
      "Body.\n",
    );
  });

  it("keeps a comment on one line", () => {
    const saved = renderAnnotated("Body.", [
      { quote: "a\nb", body: "one\ntwo" },
    ]);
    expect(saved).toContain("- **“a b”** — one two");
    expect(parseAnnotated(saved).comments).toEqual([
      { quote: "a b", body: "one two" },
    ]);
  });

  it("leaves a comments section this app did not write alone", () => {
    const text = "# Title\n\n## Comments\n\nWritten by hand.\n";
    expect(parseAnnotated(text)).toEqual({ body: text, comments: [] });
  });

  it("elides a long selection in the middle", () => {
    const quote = quoteOf("x".repeat(400));
    expect(quote.length).toBeLessThanOrEqual(QUOTE_LIMIT);
    expect(quote).toContain("…");
    expect(quoteOf("  spaced   out  ")).toBe("spaced out");
  });
});

// A remark about the note itself carries no quote, and has to survive a save.
describe("a whole-document comment", () => {
  it("round-trips under its own marker", () => {
    const saved = renderAnnotated("# Plan\n", [
      { quote: "", body: "This plan is stale." },
    ]);
    expect(saved).toContain("- **Whole document** \u2014 This plan is stale.");
    const back = parseAnnotated(saved);
    expect(back.body).toBe("# Plan");
    expect(back.comments).toEqual([{ quote: "", body: "This plan is stale." }]);
  });
});

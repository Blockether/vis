import { splitBlocks } from "./Markdown";

/* The backtick bug the user hit ("some `` are not correctly showing"): a
   still-streaming answer arrives with an OPEN fence whose closing ``` hasn't
   landed yet. splitBlocks must render that as a code block NOW instead of
   leaking raw backticks. These pin both the settled and the streaming cases. */

describe("splitBlocks", () => {
  it("returns a single text block for plain prose", () => {
    expect(splitBlocks("just words")).toEqual([{ kind: "text", body: "just words" }]);
  });

  it("splits a closed fence into a code block with lang + body", () => {
    const blocks = splitBlocks("```clojure\n(+ 1 2)\n```");
    expect(blocks).toEqual([{ kind: "code", lang: "clojure", body: "(+ 1 2)" }]);
  });

  it("keeps surrounding text around a closed fence", () => {
    const blocks = splitBlocks("before\n```\ncode\n```\nafter");
    expect(blocks).toEqual([
      { kind: "text", body: "before\n" },
      { kind: "code", lang: "", body: "code" },
      { kind: "text", body: "\nafter" }
    ]);
  });

  it("handles two fences in one answer", () => {
    const blocks = splitBlocks("```js\na\n```\nmid\n```py\nb\n```");
    expect(blocks).toEqual([
      { kind: "code", lang: "js", body: "a" },
      { kind: "text", body: "\nmid\n" },
      { kind: "code", lang: "py", body: "b" }
    ]);
  });

  it("renders an OPEN (streaming) fence as a code block, not raw backticks", () => {
    const blocks = splitBlocks("intro\n```clojure\n(def x 1");
    expect(blocks).toEqual([
      { kind: "text", body: "intro\n" },
      { kind: "code", lang: "clojure", body: "(def x 1" }
    ]);
  });

  it("handles an open fence whose language line has no newline yet", () => {
    const blocks = splitBlocks("```clojure");
    expect(blocks).toEqual([{ kind: "code", lang: "clojure", body: "" }]);
  });

  it("handles a bare open fence with no lang and no body", () => {
    const blocks = splitBlocks("text\n```");
    expect(blocks).toEqual([
      { kind: "text", body: "text\n" },
      { kind: "code", lang: "", body: "" }
    ]);
  });

  it("strips only a single trailing newline from a code body", () => {
    const blocks = splitBlocks("```\nline1\nline2\n```");
    expect(blocks).toEqual([{ kind: "code", lang: "", body: "line1\nline2" }]);
  });

  it("returns nothing meaningful for an empty string", () => {
    expect(splitBlocks("")).toEqual([]);
  });
});

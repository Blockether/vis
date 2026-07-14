import { tokenize } from "./Highlight";

/* The <Highlight> component needs a rendered tree; here we lock the pure
   tokenizer that feeds it — the piece that actually decides the colours. */

const kinds = (code: string) => tokenize(code).map((t) => t.k);
const of = (code: string, kind: string) =>
  tokenize(code)
    .filter((t) => t.k === kind)
    .map((t) => t.t);

describe("tokenize", () => {
  it("round-trips the source exactly", () => {
    const src = 'def f(x): return "hi"  # note\n';
    expect(
      tokenize(src)
        .map((t) => t.t)
        .join(""),
    ).toBe(src);
  });

  it("flags keywords, strings, numbers and comments", () => {
    expect(of("def f():", "keyword")).toEqual(["def"]);
    expect(of('x = "hello"', "string")).toEqual(['"hello"']);
    expect(of("y = 42", "number")).toEqual(["42"]);
    expect(of("# a python comment", "comment")).toEqual(["# a python comment"]);
    expect(of(";; a clojure comment", "comment")).toEqual([
      ";; a clojure comment",
    ]);
    expect(of("// a js comment", "comment")).toEqual(["// a js comment"]);
  });

  it("treats plain identifiers as words, not keywords", () => {
    expect(of("myVar + other", "keyword")).toEqual([]);
    expect(of("myVar", "word")).toEqual(["myVar"]);
  });

  it("never loops on an empty string", () => {
    expect(kinds("")).toEqual([]);
  });
});

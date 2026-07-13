import {
  activeFileMention,
  applyFileMention,
  suggestLabel,
} from "./ComposerSuggest";

describe("composer @-file suggestions", () => {
  it("detects a trailing @ token and sends only the query to the gateway", () => {
    expect(activeFileMention("please inspect @src/com")).toEqual({
      start: 15,
      end: 23,
      q: "src/com",
    });
  });

  it("supports an empty @ query", () => {
    expect(activeFileMention("open @")).toEqual({ start: 5, end: 6, q: "" });
  });

  it("does not treat @@ as a file sigil", () => {
    expect(activeFileMention("literal @@")).toBeNull();
  });

  it("replaces just the active token with the selected path", () => {
    const mention = activeFileMention("read @src then", 9)!;
    expect(
      applyFileMention("read @src then", mention, { name: "src/app.clj" }),
    ).toBe("read @src/app.clj then");
  });

  it("formats the gateway row metadata", () => {
    expect(
      suggestLabel({
        name: "src/app.clj",
        status: "tracked",
        age: "2d",
        size: 42,
      }),
    ).toBe("tracked · 2d · 42b");
  });
});

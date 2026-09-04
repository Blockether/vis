import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

const css = readFileSync(new URL("./index.css", import.meta.url), "utf8");
const packageJson = JSON.parse(
  readFileSync(new URL("../package.json", import.meta.url), "utf8"),
) as { dependencies?: Record<string, string> };

describe("the companion typeface", () => {
  it("ships JetBrains Mono as the one face for every kind of text", () => {
    expect(css).not.toContain("@fontsource-variable/inter");
    expect(css).not.toContain("Inter Variable");
    expect(css).toMatch(
      /--font-sans: 'JetBrains Mono Variable'[^;]*;\s*--font-mono: 'JetBrains Mono Variable'/,
    );
    expect(packageJson.dependencies).not.toHaveProperty(
      "@fontsource-variable/inter",
    );
  });
});

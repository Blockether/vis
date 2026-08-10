// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from "vitest";

import { rememberReadingPosition } from "./reading-position";

// Regression, user report ("I'm at the bottom, I reload, and I'm not at the
// bottom"): the transcript measured the reader's place on every scroll and kept
// it in a module map, which reload throws away with the rest of the context. The
// screen then re-opened on the newest turn — right for someone who WAS at the
// bottom, wrong for everyone else, and unprovable either way.

/** A fresh JavaScript context on the same tab: exactly what reload leaves behind. */
async function afterReload() {
  vi.resetModules();
  return import("./reading-position");
}

describe("the transcript reading position across a reload", () => {
  beforeEach(() => {
    sessionStorage.clear();
    vi.resetModules();
  });

  it("hands the same distance from the end back to a fresh context", async () => {
    rememberReadingPosition("s1", 3200);

    expect((await afterReload()).parkedReadingPosition("s1")).toBe(3200);
  });

  it("parks nothing for a reader who was at the bottom, so reload lands there", async () => {
    rememberReadingPosition("s1", null);

    expect((await afterReload()).parkedReadingPosition("s1")).toBeNull();
  });

  it("keeps one place per session", async () => {
    rememberReadingPosition("s1", 3200);
    rememberReadingPosition("s2", 90);

    const reloaded = await afterReload();
    expect(reloaded.parkedReadingPosition("s2")).toBe(90);
    expect(reloaded.parkedReadingPosition("s3")).toBeNull();
  });

  it("ignores a stored value an older build wrote in another shape", async () => {
    sessionStorage.setItem("vis.readingPositions", '["not","a","map"]');

    expect((await afterReload()).parkedReadingPosition("s1")).toBeNull();
  });
});

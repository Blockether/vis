// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { flushParked, readParked, writeParked } from "./parked";

// Regression, user report ("scrolling a live session hangs on iOS"): a parked
// place went straight into `sessionStorage` on the way in, and both readers mark
// their place from a scroll handler — so a single flick paid one synchronous
// serialize-and-store per animation frame, in the gesture this module exists to
// serve. The place itself must still survive everything it survived before.

const KEY = "vis.test.parked";

/** Whatever was parked, unrevived. */
const asIs = (raw: unknown) => raw;

describe("a parked place", () => {
  beforeEach(() => {
    // Back on the glass: the previous test may have left the page "leaving".
    window.dispatchEvent(new Event("pageshow"));
    flushParked();
    sessionStorage.clear();
  });

  afterEach(() => {
    vi.useRealTimers();
    vi.restoreAllMocks();
  });

  it("charges a whole gesture ONE store write", () => {
    const writes = vi.spyOn(globalThis.sessionStorage, "setItem");

    for (let frame = 0; frame < 60; frame += 1) {
      writeParked(KEY, { s1: 4000 - frame });
    }

    expect(writes).not.toHaveBeenCalled();
    flushParked();
    expect(writes).toHaveBeenCalledTimes(1);
    expect(JSON.parse(sessionStorage.getItem(KEY) ?? "null")).toEqual({
      s1: 3941,
    });
  });

  it("answers with the mark still in hand", () => {
    writeParked(KEY, { s1: 10 });

    expect(readParked(KEY, asIs)).toEqual({ s1: 10 });
  });

  it("answers with nothing for a place forgotten but not yet written", () => {
    sessionStorage.setItem(KEY, JSON.stringify({ s1: 10 }));

    writeParked(KEY, null);

    expect(readParked(KEY, asIs)).toBeNull();
  });

  it("writes the last mark once the gesture goes quiet", () => {
    vi.useFakeTimers();

    writeParked(KEY, { s1: 1200 });
    expect(sessionStorage.getItem(KEY)).toBeNull();

    vi.advanceTimersByTime(400);

    expect(JSON.parse(sessionStorage.getItem(KEY) ?? "null")).toEqual({
      s1: 1200,
    });
  });

  it("writes what it holds when the page can be thrown away", () => {
    writeParked(KEY, { s1: 900 });

    window.dispatchEvent(new Event("pagehide"));

    expect(JSON.parse(sessionStorage.getItem(KEY) ?? "null")).toEqual({
      s1: 900,
    });
  });

  it("writes through for a screen that parks its place ON the way out", () => {
    // The screen's own `pagehide` listener is added after this module's, so the
    // mark it writes there arrives when the flush has already run.
    window.dispatchEvent(new Event("pagehide"));

    writeParked(KEY, { s1: 640 });

    expect(JSON.parse(sessionStorage.getItem(KEY) ?? "null")).toEqual({
      s1: 640,
    });
  });

  it("coalesces again once the reader is back", () => {
    window.dispatchEvent(new Event("pagehide"));
    window.dispatchEvent(new Event("pageshow"));
    const writes = vi.spyOn(globalThis.sessionStorage, "setItem");

    writeParked(KEY, { s1: 5 });

    expect(writes).not.toHaveBeenCalled();
  });
});

import { relTime, shortId, timeHHMM } from "./theme";

/* Sessions-drawer formatting helpers — small, but they render on every row. */

describe("shortId", () => {
  it("takes the first 8 chars", () => {
    expect(shortId("b90a7b21-c983-4b6b")).toBe("b90a7b21");
  });
  it("is empty for a missing id", () => {
    expect(shortId(undefined)).toBe("");
    expect(shortId("")).toBe("");
  });
});

describe("relTime", () => {
  const now = Date.now();
  it("is empty for a missing epoch", () => {
    expect(relTime(undefined)).toBe("");
  });
  it("is empty for a future / negative delta", () => {
    expect(relTime(now + 10_000)).toBe("");
  });
  it("buckets seconds as 'just now'", () => {
    expect(relTime(now - 5_000)).toBe("just now");
  });
  it("buckets minutes", () => {
    expect(relTime(now - 5 * 60_000)).toBe("5m ago");
  });
  it("buckets hours", () => {
    expect(relTime(now - 3 * 3_600_000)).toBe("3h ago");
  });
  it("buckets days", () => {
    expect(relTime(now - 2 * 86_400_000)).toBe("2d ago");
  });
  it("normalizes a seconds-granularity epoch (< 1e12) to ms", () => {
    const epochSecs = Math.floor((now - 5 * 60_000) / 1000);
    expect(relTime(epochSecs)).toBe("5m ago");
  });
});

describe("timeHHMM", () => {
  it("is undefined for a missing epoch", () => {
    expect(timeHHMM(undefined)).toBeUndefined();
  });
  it("formats a valid epoch as HH:MM", () => {
    expect(timeHHMM(Date.now())).toMatch(/^\d{2}:\d{2}$/);
  });
  it("is undefined for a non-finite epoch", () => {
    expect(timeHHMM(Number.NaN)).toBeUndefined();
  });
});

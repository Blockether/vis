import { describe, expect, it, vi } from "vitest";

// Every ASC request the distribution makes goes through the real `asc`; the retry wrapper
// is what we are testing, so the transport is the only thing stubbed.
const calls = [];
vi.mock("./asc.mjs", () => ({
  appIdFor: vi.fn(),
  ascToken: vi.fn(),
  waitForBuild: vi.fn(),
  asc: vi.fn((token, method, path, body) => {
    calls.push({ token, method, path, body });
    if (calls.length === 1)
      return Promise.reject(
        Object.assign(new Error("ASC → 401"), { status: 401 }),
      );
    return Promise.resolve({ data: { id: "ok" } });
  }),
}));

import { linkTargets, retryingApi, retryUnauthorized } from "./testflight.mjs";

const asc = (status) =>
  Object.assign(
    new Error(`ASC POST /v1/betaAppReviewSubmissions → ${status}`),
    { status },
  );

describe("retryUnauthorized", () => {
  it("mints one token and stops when the call works", async () => {
    const tokens = [];
    const minted = ["t1", "t2"];
    const res = await retryUnauthorized(
      () => minted.shift(),
      (token) => {
        tokens.push(token);
        return Promise.resolve("ok");
      },
    );
    expect(res).toBe("ok");
    expect(tokens).toEqual(["t1"]);
  });

  // The failure that took build 3083's release down after the .ipa was already uploaded.
  it("retries a 401 once, with a FRESH token", async () => {
    const tokens = [];
    const minted = ["stale", "fresh"];
    const res = await retryUnauthorized(
      () => minted.shift(),
      (token) => {
        tokens.push(token);
        return tokens.length === 1
          ? Promise.reject(asc(401))
          : Promise.resolve("submitted");
      },
    );
    expect(res).toBe("submitted");
    expect(tokens).toEqual(["stale", "fresh"]);
  });

  it("gives up when the second attempt is refused too", async () => {
    let calls = 0;
    await expect(
      retryUnauthorized(
        () => "token",
        () => {
          calls += 1;
          return Promise.reject(asc(401));
        },
      ),
    ).rejects.toThrow("401");
    expect(calls).toBe(2);
  });

  // A duplicate submission must still reach the caller's isDuplicate() check, and a
  // real refusal must still fail the step: only 401 is worth a second try.
  it("never retries any other status", async () => {
    for (const status of [409, 403, 422, 500]) {
      let calls = 0;
      await expect(
        retryUnauthorized(
          () => "token",
          () => {
            calls += 1;
            return Promise.reject(asc(status));
          },
        ),
      ).rejects.toThrow(String(status));
      expect(calls).toBe(1);
    }
  });
});

describe("retryingApi", () => {
  // Not only the two POSTs that failed once: a 401 on ANY request of an already-uploaded
  // build's distribution must cost a fresh token, not the release.
  it("mints per request and replays a 401 with a fresh token", async () => {
    const minted = ["t1", "t2", "t3"];
    const api = retryingApi(() => minted.shift());

    const first = await api("GET", "/v1/apps/42");

    expect(first).toEqual({ data: { id: "ok" } });
    expect(calls.map((c) => [c.token, c.method, c.path])).toEqual([
      ["t1", "GET", "/v1/apps/42"],
      ["t2", "GET", "/v1/apps/42"],
    ]);

    await api("POST", "/v1/betaGroups", { data: 1 });
    expect(calls.at(-1)).toEqual({
      token: "t3",
      method: "POST",
      path: "/v1/betaGroups",
      body: { data: 1 },
    });
  });
});

// Regression: an old TestFlight invitation served 0.1.14 (build 2804) while the Public link
// served 0.1.32 (build 3774), because only the named group ever got the new build linked.
// Regression: the fan-out then tried the INTERNAL group too and App Store Connect answered
// `422 Builds cannot be assigned to this internal group`, failing the release job (run
// 31207385878) after the build had already been uploaded and distributed.
describe("linkTargets", () => {
  const groups = [
    { id: "public" },
    { id: "external", attributes: { isInternalGroup: false } },
    { id: "internal", attributes: { isInternalGroup: true } },
  ];

  it("carries the build to every EXTERNAL beta group, named one first", () => {
    expect(linkTargets(groups, "public")).toEqual(["public", "external"]);
  });

  it("never links the named group twice", () => {
    expect(linkTargets(groups, "external")).toEqual(["external", "public"]);
  });

  it("is just the named group when it is the only one", () => {
    expect(linkTargets([{ id: "public" }], "public")).toEqual(["public"]);
  });
});

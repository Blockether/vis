import { describe, expect, it } from "vitest";
import { linkTargets } from "./testflight.mjs";

// Every ASC request this file used to wrap is retried inside the transport now
// (scripts/asc.mjs, scripts/asc.test.mjs): a 401, a dropped socket and a 429 are the
// client's business, not the distribution's.

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

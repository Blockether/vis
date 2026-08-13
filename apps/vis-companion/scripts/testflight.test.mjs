import { describe, expect, it } from "vitest";
import { linkTargets, planDistribution } from "./testflight.mjs";

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

// Regression: an iOS upload reached only the internal groups unless `--public` was passed, so the
// public TestFlight link served build 4042 while the team group had 4075 and every Play tester
// track already served 4090. The default is now every tester audience, planned before the build.
describe("planDistribution", () => {
  it("fans out to every tester audience when nothing is asked for", () => {
    expect(planDistribution()).toEqual({
      audiences: ["internal", "public"],
      isPublic: true,
      group: "Public",
      review: true,
    });
  });

  it("keeps the build inside the team when internal is asked for alone", () => {
    const plan = planDistribution({ audiences: "internal" });
    expect(plan.audiences).toEqual(["internal"]);
    expect(plan.isPublic).toBe(false);
    // Nothing is submitted to Beta App Review when nothing goes public.
    expect(plan.review).toBe(false);
  });

  it("still carries internal when only public is asked for — Apple gives it away", () => {
    expect(planDistribution({ audiences: ["public"] }).audiences).toEqual(["internal", "public"]);
  });

  it("reads a comma list and a repeated flag the same way", () => {
    expect(planDistribution({ audiences: "internal,public" })).toEqual(
      planDistribution({ audiences: ["internal", "public"] }),
    );
  });

  it("refuses an unknown audience before anything is built", () => {
    expect(() => planDistribution({ audiences: "nightly" })).toThrow(/unknown audience "nightly"/);
  });

  it("treats a named group as the public group", () => {
    const plan = planDistribution({ audiences: "internal", group: "Public Beta" });
    expect(plan.isPublic).toBe(true);
    expect(plan.group).toBe("Public Beta");
  });

  it("links without a review round trip when review is off", () => {
    expect(planDistribution({ review: false })).toMatchObject({ isPublic: true, review: false });
  });
});

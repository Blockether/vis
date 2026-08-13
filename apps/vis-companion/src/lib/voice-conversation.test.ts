import { describe, expect, it } from "vitest";
import { VoiceTurnOwnership } from "./voice-conversation";

describe("VoiceTurnOwnership", () => {
  it("does not revive an exited voice turn when its submission resolves late", () => {
    const ownership = new VoiceTurnOwnership();
    const oldLease = ownership.enter();

    ownership.leave();
    ownership.enter();

    expect(ownership.claim("turn-a", oldLease)).toBe(false);
    expect(ownership.settle("turn-a")).toBe(false);
  });

  it("speaks an active turn once", () => {
    const ownership = new VoiceTurnOwnership();
    const lease = ownership.enter();

    expect(ownership.claim("turn-a", lease)).toBe(true);
    expect(ownership.settle("turn-a")).toBe(true);
    expect(ownership.settle("turn-a")).toBe(false);
  });

  it("invalidates outstanding turns when voice mode exits", () => {
    const ownership = new VoiceTurnOwnership();
    const lease = ownership.enter();
    expect(ownership.claim("turn-a", lease)).toBe(true);

    ownership.leave();

    expect(ownership.settle("turn-a")).toBe(false);
  });
});

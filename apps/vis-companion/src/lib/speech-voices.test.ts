import { describe, expect, it } from "vitest";

import { BEST_DEVICE_VOICE_LIMIT, bestDeviceVoices, type DeviceVoice } from "./speech-voices";

const voices: DeviceVoice[] = [
  { id: "premium-local", label: "Premium Local", language: "en-US", quality: 500, isLocal: true },
  { id: "premium-web", label: "Premium Web", language: "en-US", quality: 500, isLocal: false },
  { id: "enhanced", label: "Enhanced", language: "en-US", quality: 450, isLocal: true },
  { id: "high", label: "High", language: "en-US", quality: 400, isLocal: true },
  { id: "good", label: "Good", language: "en-US", quality: 350, isLocal: true },
  { id: "ordinary", label: "Ordinary", language: "en-US", quality: 300, isLocal: true },
  { id: "compact", label: "Compact", language: "en-US", quality: 100, isLocal: true },
  { id: "fr-premium", label: "French Premium", language: "fr-FR", quality: 500, isLocal: true },
];

describe("best device voices", () => {
  it("keeps a short quality-ranked list in the device languages", () => {
    const chosen = bestDeviceVoices(voices, null, ["en-US"]);

    expect(chosen).toHaveLength(BEST_DEVICE_VOICE_LIMIT);
    expect(chosen.map((voice) => voice.id)).toEqual([
      "premium-local",
      "premium-web",
      "enhanced",
    ]);
    expect(chosen.some((voice) => voice.language === "fr-FR")).toBe(false);
  });

  it("keeps the stored voice reachable without exceeding the limit", () => {
    const chosen = bestDeviceVoices(voices, "compact", ["en-US"]);

    expect(chosen).toHaveLength(BEST_DEVICE_VOICE_LIMIT);
    expect(chosen.map((voice) => voice.id)).toEqual([
      "premium-local",
      "premium-web",
      "compact",
    ]);
  });

  it("recognises the quality markers carried by Apple voice identifiers", () => {
    const chosen = bestDeviceVoices(
      [
        { id: "com.apple.voice.compact.en-US.Fred", label: "Fred", language: "en-US" },
        { id: "com.apple.voice.premium.en-US.Ava", label: "Ava", language: "en-US" },
        {
          id: "com.apple.voice.natural.en-US.Samantha",
          label: "Samantha",
          language: "en-US",
        },
        { id: "com.apple.voice.enhanced.en-US.Alex", label: "Alex", language: "en-US" },
      ],
      null,
      ["en-US"],
    );

    expect(chosen.map((voice) => voice.id)).toEqual([
      "com.apple.voice.premium.en-US.Ava",
      "com.apple.voice.natural.en-US.Samantha",
      "com.apple.voice.enhanced.en-US.Alex",
    ]);
    expect(chosen.some((voice) => voice.id.includes("compact"))).toBe(false);
  });
});

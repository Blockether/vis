import { describe, expect, it } from "vitest";

import {
  BEST_DEVICE_VOICE_LIMIT,
  bestDeviceVoices,
  iosVoiceDownloadGuidance,
  type DeviceVoice,
} from "./speech-voices";

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

  it("offers only the three recommended public Apple voices on iOS", () => {
    const chosen = bestDeviceVoices(
      [
        { id: "com.apple.voice.premium.en-US.Zoe", label: "Zoe (Premium)", language: "en-US", quality: 500 },
        { id: "com.apple.voice.premium.en-US.Ava", label: "Ava (Premium)", language: "en-US", quality: 500 },
        { id: "com.apple.voice.enhanced.en-US.Samantha", label: "Samantha (Enhanced)", language: "en-US", quality: 450 },
        { id: "com.apple.voice.premium.en-GB.Serena", label: "Serena (Premium)", language: "en-GB", quality: 500 },
        { id: "com.apple.voice.enhanced.en-IE.Moira", label: "Moira (Enhanced)", language: "en-IE", quality: 450 },
        { id: "com.apple.voice.premium.en-US.Samantha", label: "Samantha (Premium)", language: "en-US", quality: 500 },
        { id: "com.apple.voice.enhanced.en-US.Evan", label: "Evan (Enhanced)", language: "en-US", quality: 450 },
      ],
      "com.apple.voice.premium.en-GB.Serena",
      ["en-US"],
      "ios",
    );

    expect(chosen.map((voice) => voice.id)).toEqual([
      "com.apple.voice.premium.en-US.Zoe",
      "com.apple.voice.premium.en-US.Ava",
      "com.apple.voice.enhanced.en-US.Samantha",
    ]);
  });

  // Regression, session 3d6dc388-a21c-4005-b498-87c02668cb34: iOS showed only the one
  // hard-coded recommendation installed on the phone and hid every other usable system voice.
  it("fills an incomplete iOS recommendation list with installed voices", () => {
    const chosen = bestDeviceVoices(
      [
        {
          id: "com.apple.voice.enhanced.en-US.Samantha",
          label: "Samantha (Enhanced)",
          language: "en-US",
          quality: 450,
        },
        {
          id: "com.apple.voice.premium.en-US.Serena",
          label: "Serena (Premium)",
          language: "en-US",
          quality: 500,
        },
        {
          id: "com.apple.voice.enhanced.en-US.Evan",
          label: "Evan (Enhanced)",
          language: "en-US",
          quality: 450,
        },
        {
          id: "com.apple.voice.compact.en-US.Fred",
          label: "Fred",
          language: "en-US",
          quality: 300,
        },
      ],
      null,
      ["en-US"],
      "ios",
    );

    expect(chosen.map((voice) => voice.label)).toEqual([
      "Samantha (Enhanced)",
      "Serena (Premium)",
      "Evan (Enhanced)",
    ]);
  });

  // Regression, session 3d6dc388-a21c-4005-b498-87c02668cb34: filling missing
  // recommendations with standard Apple voices exposed robotic choices as desirable voices.
  it("does not pad iOS choices with standard-quality voices", () => {
    const chosen = bestDeviceVoices(
      [
        {
          id: "com.apple.voice.enhanced.en-US.Samantha",
          label: "Samantha (Enhanced)",
          language: "en-US",
          quality: 450,
        },
        {
          id: "com.apple.voice.compact.en-US.Fred",
          label: "Fred",
          language: "en-US",
          quality: 300,
        },
        {
          id: "com.apple.voice.compact.en-US.Grandma",
          label: "Grandma",
          language: "en-US",
          quality: 300,
        },
      ],
      null,
      ["en-US"],
      "ios",
    );

    expect(chosen.map((voice) => voice.label)).toEqual(["Samantha (Enhanced)"]);
  });

  it("explains Apple's system-managed voice download path only on iOS", () => {
    expect(iosVoiceDownloadGuidance("ios")).toContain(
      "Settings → Accessibility → Read & Speak",
    );
    expect(iosVoiceDownloadGuidance("android")).toBeNull();
    expect(iosVoiceDownloadGuidance("web")).toBeNull();
  });
});

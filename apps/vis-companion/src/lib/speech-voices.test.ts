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

  // Regression, Vis session 26e737d2-a64b-4d91-9aa9-889cec313202: iOS capped the
  // picker at three hard-coded voices and hid other downloaded Premium and Enhanced voices.
  it("offers every public Premium and Enhanced Apple voice on iOS", () => {
    const chosen = bestDeviceVoices(
      [
        { id: "com.apple.voice.premium.en-US.Zoe", label: "Zoe (Premium)", language: "en-US", quality: 500 },
        { id: "com.apple.voice.premium.en-US.Ava", label: "Ava (Premium)", language: "en-US", quality: 500 },
        { id: "com.apple.voice.enhanced.en-US.Samantha", label: "Samantha (Enhanced)", language: "en-US", quality: 450 },
        { id: "com.apple.voice.premium.en-GB.Serena", label: "Serena (Premium)", language: "en-GB", quality: 500 },
        { id: "com.apple.voice.enhanced.en-IE.Moira", label: "Moira (Enhanced)", language: "en-IE", quality: 450 },
        { id: "com.apple.voice.premium.en-US.Samantha", label: "Samantha (Premium)", language: "en-US", quality: 500 },
        { id: "com.apple.voice.enhanced.en-US.Evan", label: "Evan (Enhanced)", language: "en-US", quality: 450 },
        { id: "com.apple.voice.premium.fr-FR.Amelie", label: "Amélie (Premium)", language: "fr-FR", quality: 500 },
      ],
      "com.apple.voice.premium.en-GB.Serena",
      ["en-US"],
      "ios",
    );

    expect(chosen.map((voice) => voice.id)).toEqual([
      "com.apple.voice.premium.en-US.Ava",
      "com.apple.voice.premium.en-US.Samantha",
      "com.apple.voice.premium.en-US.Zoe",
      "com.apple.voice.enhanced.en-US.Evan",
      "com.apple.voice.enhanced.en-US.Samantha",
      "com.apple.voice.premium.en-GB.Serena",
      "com.apple.voice.enhanced.en-IE.Moira",
      "com.apple.voice.premium.fr-FR.Amelie",
    ]);
  });

  // Regression, session 3d6dc388-a21c-4005-b498-87c02668cb34: iOS exposed
  // robotic standard voices beside downloaded natural voices.
  it("does not show standard-quality voices on iOS", () => {
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

  // Regression, session 3d6dc388-a21c-4005-b498-87c02668cb34: the Apple voice
  // backing System default was also rendered as an identical explicit choice.
  it("does not duplicate the iOS system default as an explicit voice", () => {
    const chosen = bestDeviceVoices(
      [
        {
          id: "com.apple.voice.enhanced.en-US.Samantha",
          label: "Samantha (Enhanced)",
          language: "en-US",
          quality: 450,
          isDefault: true,
        },
        {
          id: "com.apple.voice.premium.en-US.Ava",
          label: "Ava (Premium)",
          language: "en-US",
          quality: 500,
          isDefault: false,
        },
      ],
      null,
      ["en-US"],
      "ios",
    );

    expect(chosen.map((voice) => voice.label)).toEqual(["Ava (Premium)"]);
  });

  it("explains Apple's system-managed voice download path only on iOS", () => {
    expect(iosVoiceDownloadGuidance("ios")).toContain(
      "Settings → Accessibility → Read & Speak",
    );
    expect(iosVoiceDownloadGuidance("android")).toBeNull();
    expect(iosVoiceDownloadGuidance("web")).toBeNull();
  });
});

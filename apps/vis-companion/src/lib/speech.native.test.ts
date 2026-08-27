import { beforeEach, describe, expect, it, vi } from "vitest";

const nativeSpeech = vi.hoisted(() => ({
  getVoices: vi.fn(),
  speak: vi.fn(),
  stop: vi.fn(),
  openVoiceSettings: vi.fn(),
}));

vi.mock("@capacitor/core", () => ({
  Capacitor: { getPlatform: () => "ios" },
  registerPlugin: () => nativeSpeech,
}));

import { deviceVoices, openIosVoiceSettings } from "./speech-voices";
import { speechOutput } from "./speech";

beforeEach(() => {
  nativeSpeech.getVoices.mockReset();
  nativeSpeech.speak.mockReset().mockResolvedValue(undefined);
  nativeSpeech.stop.mockReset().mockResolvedValue(undefined);
  nativeSpeech.openVoiceSettings.mockReset().mockResolvedValue(undefined);
});

describe("native iOS speech", () => {
  it("reads Apple's public voice catalogue through the native bridge", async () => {
    nativeSpeech.getVoices.mockResolvedValue({
      voices: [
        {
          id: "com.apple.voice.premium.en-US.Zoe",
          label: "Zoe (Premium)",
          language: "en-US",
          quality: 500,
        },
      ],
    });

    await expect(deviceVoices()).resolves.toEqual([
      {
        id: "com.apple.voice.premium.en-US.Zoe",
        label: "Zoe (Premium)",
        language: "en-US",
        isDefault: undefined,
        quality: 500,
        isLocal: undefined,
      },
    ]);
    expect(nativeSpeech.getVoices).toHaveBeenCalledOnce();
  });

  it("opens Apple's voice catalogue in iOS Settings through the native bridge", async () => {
    await expect(openIosVoiceSettings()).resolves.toBeUndefined();

    expect(nativeSpeech.openVoiceSettings).toHaveBeenCalledOnce();
  });

  it("speaks, auditions, and stops the exact public Apple voice natively", async () => {
    await speechOutput.playDeviceSample(
      "This is what this voice sounds like.",
      "com.apple.voice.premium.en-US.Zoe",
      1.2,
    );

    expect(nativeSpeech.speak).toHaveBeenCalledWith({
      text: "This is what this voice sounds like.",
      voice: "com.apple.voice.premium.en-US.Zoe",
      rate: 1.2,
    });
    nativeSpeech.stop.mockClear();
    speechOutput.stop();
    expect(nativeSpeech.stop).toHaveBeenCalledOnce();
  });
});

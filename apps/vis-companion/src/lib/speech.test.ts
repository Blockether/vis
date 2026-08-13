// @vitest-environment jsdom

import { afterEach, describe, expect, it, vi } from "vitest";
import { speechOutput } from "./speech";

class Utterance {
  onend: (() => void) | null = null;
  onerror: ((event: { error: string }) => void) | null = null;
  readonly text: string;
  constructor(text: string) {
    this.text = text;
  }
}

afterEach(() => {
  speechOutput.stop();
  vi.unstubAllGlobals();
});

describe("speechOutput", () => {
  it("speaks locally and resolves when synthesis ends", async () => {
    const speak = vi.fn((utterance: Utterance) => utterance.onend?.());
    vi.stubGlobal("SpeechSynthesisUtterance", Utterance);
    vi.stubGlobal("speechSynthesis", { speak, cancel: vi.fn() });
    await speechOutput.speak("Ready to hear.");

    expect(speak).toHaveBeenCalledOnce();
    expect((speak.mock.calls[0]?.[0] as Utterance).text).toBe("Ready to hear.");
  });

  it("stops playback immediately", () => {
    const cancel = vi.fn();
    vi.stubGlobal("SpeechSynthesisUtterance", Utterance);
    vi.stubGlobal("speechSynthesis", { speak: vi.fn(), cancel });
    void speechOutput.speak("Long answer.");
    cancel.mockClear();
    speechOutput.stop();

    expect(cancel).toHaveBeenCalledOnce();
  });
});

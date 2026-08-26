/**
 * Which TTS engine speaks a reply, proven at the seam that decides it.
 *
 * A selected machine engine receives its exact id. This device remains the safe answer
 * when no machine is open or that engine cannot speak, so routing never drops the reply.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type { SpeechPrefs } from "./types";

class FakeUtterance {
  onend: (() => void) | null = null;
  onerror: ((event: { error: string }) => void) | null = null;
  rate = 1;
  voice: unknown = null;

  readonly text: string;

  constructor(text: string) {
    this.text = text;
  }
}

const said: { text: string; rate: number }[] = [];
const played: string[] = [];

class FakeAudio {
  onended: (() => void) | null = null;
  onerror: (() => void) | null = null;
  src: string;

  constructor(url: string) {
    this.src = url;
    played.push(url);
  }

  play(): Promise<void> {
    setTimeout(() => this.onended?.(), 0);
    return Promise.resolve();
  }

  pause(): void {}
}

const prefs = (over: Partial<SpeechPrefs> = {}): SpeechPrefs => ({
  asrEngine: null,
  ttsEngine: null,
  deviceVoice: null,
  gatewayVoice: null,
  rate: 1,
  ...over,
});

beforeEach(() => {
  said.length = 0;
  played.length = 0;
  vi.stubGlobal("window", globalThis);
  vi.stubGlobal("SpeechSynthesisUtterance", FakeUtterance);
  vi.stubGlobal("speechSynthesis", {
    speak: (utterance: FakeUtterance) => {
      said.push({ text: utterance.text, rate: utterance.rate });
      utterance.onend?.();
    },
    cancel: () => undefined,
    getVoices: () => [],
  });
  vi.stubGlobal("Audio", FakeAudio);
  // A subclass, not a bare object: `new URL(...)` is used all over the client, so a
  // stub that only carried the two object-URL statics would break every caller.
  const RealURL = globalThis.URL;
  class StubURL extends RealURL {
    static createObjectURL(): string {
      return "blob:reply";
    }

    static revokeObjectURL(): void {}
  }
  vi.stubGlobal("URL", StubURL);
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("spoken reply routing", () => {
  it("plays what the machine sent, and does not also say it here", async () => {
    const { speechOutput } = await import("./speech");
    const speak = vi
      .fn()
      .mockResolvedValue(new Blob([new Uint8Array([1, 2])], { type: "audio/wav" }));
    speechOutput.apply(
      prefs({ ttsEngine: "pocket-tts-local", gatewayVoice: "kristin" }),
    );
    speechOutput.setGateway({ speak });

    await speechOutput.speak("Spoken by the machine.");

    expect(speak).toHaveBeenCalledWith(
      "Spoken by the machine.",
      "kristin",
      "pocket-tts-local",
    );
    expect(played).toEqual(["blob:reply"]);
    expect(said).toEqual([]);
  });

  it("speaks here when the machine cannot, and says why once", async () => {
    const { speechOutput } = await import("./speech");
    const notices: string[] = [];
    speechOutput.apply(prefs({ ttsEngine: "pocket-tts-local", rate: 1.2 }));
    speechOutput.setGateway(
      { speak: () => Promise.reject(new Error("no speech engine is registered")) },
      (message) => notices.push(message),
    );

    await speechOutput.speak("Still worth hearing.");

    expect(said).toEqual([{ text: "Still worth hearing.", rate: 1.2 }]);
    expect(played).toEqual([]);
    expect(notices).toHaveLength(1);
    expect(notices[0]).toContain("no speech engine is registered");
  });

  it("falls back to this device when no session has registered a machine", async () => {
    const { speechOutput } = await import("./speech");
    speechOutput.apply(prefs({ ttsEngine: "pocket-tts-local" }));
    speechOutput.setGateway(null);

    await speechOutput.speak("Nobody is listening to the gateway.");

    expect(said).toEqual([
      { text: "Nobody is listening to the gateway.", rate: 1 },
    ]);
  });
});

// Regression, user report: replacing one voice preview made the deliberately interrupted
// audio element reject, and Settings painted that ordinary handoff as an error.
describe("voice sample playback", () => {
  it("settles the previous sample when a new one replaces it", async () => {
    const samples: InterruptingAudio[] = [];
    class InterruptingAudio {
      onloadedmetadata: (() => void) | null = null;
      ontimeupdate: (() => void) | null = null;
      onended: (() => void) | null = null;
      onerror: (() => void) | null = null;
      currentTime = 0;
      duration = 1;
      src: string;

      constructor(url: string) {
        this.src = url;
        samples.push(this);
      }

      play(): Promise<void> {
        return Promise.resolve();
      }

      pause(): void {
        this.onerror?.();
      }
    }
    vi.stubGlobal("Audio", InterruptingAudio);
    const { speechOutput } = await import("./speech");
    const sample = new Blob([new Uint8Array([1, 2])], { type: "audio/wav" });

    const first = speechOutput.playSample(sample);
    const second = speechOutput.playSample(sample);
    samples[1]?.onended?.();

    expect((await Promise.allSettled([first, second])).map(({ status }) => status)).toEqual([
      "fulfilled",
      "fulfilled",
    ]);
  });
});

// These two came from the device-only output this router replaced: it still has to
// resolve when the ENGINE finishes rather than when the call returns, and it still has
// to go quiet the moment the reader taps stop.
describe("speaking on this device", () => {
  it("resolves only once the device's engine says it finished", async () => {
    const { speechOutput } = await import("./speech");
    speechOutput.apply(prefs());
    let finish: (() => void) | null = null;
    vi.stubGlobal("speechSynthesis", {
      speak: (utterance: FakeUtterance) => {
        finish = () => utterance.onend?.();
      },
      cancel: () => undefined,
      getVoices: () => [],
    });

    let isDone = false;
    const spoken = speechOutput.speak("Ready to hear.").then(() => {
      isDone = true;
    });
    await Promise.resolve();
    expect(isDone).toBe(false);

    finish!();
    await spoken;
    expect(isDone).toBe(true);
  });

  it("goes quiet the moment it is stopped", async () => {
    const { speechOutput } = await import("./speech");
    const cancel = vi.fn();
    speechOutput.apply(prefs());
    vi.stubGlobal("speechSynthesis", {
      speak: (utterance: FakeUtterance) => utterance.onend?.(),
      cancel,
      getVoices: () => [],
    });

    await speechOutput.speak("Long answer.");
    cancel.mockClear();
    speechOutput.stop();

    expect(cancel).toHaveBeenCalledOnce();
  });

  it("ignores a scoped stop from a playback control that does not own the speech", async () => {
    const { speechOutput } = await import("./speech");
    const cancel = vi.fn();
    let finish: (() => void) | null = null;
    speechOutput.apply(prefs());
    vi.stubGlobal("speechSynthesis", {
      speak: (utterance: FakeUtterance) => {
        finish = () => utterance.onend?.();
      },
      cancel,
      getVoices: () => [],
    });

    const spoken = speechOutput.speak("The voice conversation owns this reading.");
    await Promise.resolve();
    cancel.mockClear();
    speechOutput.stop({});

    expect(cancel).not.toHaveBeenCalled();
    finish!();
    await spoken;
  });
});

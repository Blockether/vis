/**
 * WHERE a reply is spoken, proven at the seam that decides it.
 *
 * The router is the one place in the app that may drop a reply, so each of its three
 * answers is pinned: silence when the reader asked for silence, the machine's audio
 * when a machine is speaking, and THIS DEVICE - not silence - when that machine cannot.
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
  route: "device",
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
    speechOutput.apply(prefs({ route: "gateway", gatewayVoice: "kristin" }));
    speechOutput.setGateway({ speak });

    await speechOutput.speak("Spoken by the machine.");

    expect(speak).toHaveBeenCalledWith("Spoken by the machine.", "kristin");
    expect(played).toEqual(["blob:reply"]);
    expect(said).toEqual([]);
  });

  it("speaks here when the machine cannot, and says why once", async () => {
    const { speechOutput } = await import("./speech");
    const notices: string[] = [];
    speechOutput.apply(prefs({ route: "gateway", rate: 1.2 }));
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
    speechOutput.apply(prefs({ route: "gateway" }));
    speechOutput.setGateway(null);

    await speechOutput.speak("Nobody is listening to the gateway.");

    expect(said).toEqual([
      { text: "Nobody is listening to the gateway.", rate: 1 },
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
});

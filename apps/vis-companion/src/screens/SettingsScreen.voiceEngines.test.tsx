// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { useState } from "react";

import { SpeechEnginesPanel } from "./SettingsScreen";
import { GatewayError } from "../lib/gateway";
import type { GatewayClient } from "../lib/gateway";
import { getSpeechPrefs } from "../lib/storage";
import type { SpeechPrefs, VoiceModelState } from "../lib/types";

const initialPrefs: SpeechPrefs = {
  asrEngine: null,
  ttsEngine: null,
  deviceVoice: null,
  gatewayVoice: null,
  rate: 1,
};

const ready = (engine: string): VoiceModelState => ({ status: "ready", engine });
const absent = (message: string, reasons?: string[]) =>
  new GatewayError(501, message, reasons ? { error: message, reasons } : { error: message });

function machine(
  listen: VoiceModelState | Error = ready("parakeet-local"),
  speak: VoiceModelState | Error = ready("pocket-tts-local"),
  options: { hasAsr?: boolean; hasTts?: boolean } = {},
) {
  const asked: string[] = [];
  const reply =
    (which: "asr" | "tts", answer: VoiceModelState | Error) =>
    ({ start = false, engine }: { start?: boolean; engine?: string | null } = {}) => {
      asked.push(`${which}:${engine ?? "default"}:${start ? "start" : "read"}`);
      return answer instanceof Error ? Promise.reject(answer) : Promise.resolve(answer);
    };
  const asrEngines =
    options.hasAsr === false
      ? []
      : [
          { id: "parakeet-local", label: "Parakeet (local)" },
          { id: "whisper-local", label: "Whisper (local)" },
        ];
  const ttsEngines =
    options.hasTts === false
      ? []
      : [
          { id: "piper-local", label: "Piper (local)", voices: [] },
          { id: "pocket-tts-local", label: "Pocket TTS (local)", voices: [] },
        ];
  return {
    asked,
    client: {
      cachedCapabilities: vi.fn(() => null),
      capabilities: vi.fn().mockResolvedValue({
        features: {
          voice: {
            engines: asrEngines,
            selected: asrEngines[0]?.id ?? null,
            model: listen instanceof Error ? null : listen,
          },
          speech: {
            engines: ttsEngines,
            selected: ttsEngines[0]?.id ?? null,
            model: speak instanceof Error ? null : speak,
          },
        },
      }),
      voiceModel: vi.fn(reply("asr", listen)),
      speechModel: vi.fn(reply("tts", speak)),
    } as unknown as GatewayClient,
  };
}

function Harness({ client }: { client: GatewayClient }) {
  const [prefs, setPrefs] = useState(initialPrefs);
  async function onChange(write: () => Promise<void>) {
    await write();
    const next = await getSpeechPrefs();
    setPrefs(next);
    return next;
  }
  return <SpeechEnginesPanel client={client} prefs={prefs} onChange={onChange} />;
}

beforeEach(() => {
  localStorage.clear();
  Object.defineProperty(window, "speechSynthesis", {
    configurable: true,
    value: {
      getVoices: () => [
        { voiceURI: "device-default", name: "Daniel", lang: "en-GB", default: true },
        { voiceURI: "device-samantha", name: "Samantha", lang: "en-US", default: false },
      ],
      speak: () => undefined,
      cancel: () => undefined,
    },
  });
});

afterEach(() => {
  document.body.innerHTML = "";
  vi.restoreAllMocks();
});

function choice(name: RegExp) {
  return screen
    .getAllByRole("button", { name })
    .find((button) => button.hasAttribute("aria-pressed"));
}
describe("the speech-engines band", () => {
  it("presents ASR and TTS as disclosures instead of separate listening and reply sections", async () => {
    const { client } = machine();
    render(<Harness client={client} />);

    const asr = await screen.findByRole("button", { name: /ASR/ });
    const tts = screen.getByRole("button", { name: /TTS/ });
    expect(asr.getAttribute("aria-expanded")).toBe("false");
    expect(tts.getAttribute("aria-expanded")).toBe("false");
    expect(screen.queryByText("Listening")).toBeNull();
    expect(screen.queryByText("Speaking")).toBeNull();
    expect(screen.queryByText("Spoken replies")).toBeNull();
    expect(screen.queryByText("Whisper (local)")).toBeNull();

    fireEvent.click(asr);
    expect(choice(/Parakeet \(local\)/)).toBeTruthy();
    expect(screen.getByText("Whisper (local)")).toBeTruthy();
  });

  it("stores the specific ASR engine chosen from the expanded list", async () => {
    const { client, asked } = machine();
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /ASR/ }));
    const whisper = await screen.findByRole("button", { name: /Whisper \(local\)/ });
    fireEvent.click(whisper);

    await waitFor(async () =>
      expect((await getSpeechPrefs()).asrEngine).toBe("whisper-local"),
    );
    expect(whisper.getAttribute("aria-pressed")).toBe("true");
    await waitFor(() => expect(asked).toContain("asr:whisper-local:read"));
  });

  it("treats this device and every advertised machine engine as TTS choices", async () => {
    const { client } = machine();
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /TTS/ }));
    await waitFor(() => expect(choice(/This device/)).toBeTruthy());
    const device = choice(/This device/)!;
    const pocket = screen.getByRole("button", { name: /Pocket TTS \(local\)/ });
    expect(device.getAttribute("aria-pressed")).toBe("true");
    expect(screen.getByRole("button", { name: /Piper \(local\)/ })).toBeTruthy();
    expect(screen.getByRole("button", { name: /Samantha/ })).toBeTruthy();

    fireEvent.click(pocket);

    await waitFor(async () =>
      expect((await getSpeechPrefs()).ttsEngine).toBe("pocket-tts-local"),
    );
    expect(pocket.getAttribute("aria-pressed")).toBe("true");
    expect(screen.queryByRole("button", { name: /Samantha/ })).toBeNull();
  });

  it("reports a failed selected engine and retries only that engine", async () => {
    const { client, asked } = machine({
      status: "failed",
      engine: "parakeet-local",
      error: "the archive did not match its checksum",
    });
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /ASR/ }));
    expect(
      await screen.findByText("the archive did not match its checksum"),
    ).toBeTruthy();
    fireEvent.click(screen.getByText("Try again"));

    await waitFor(() =>
      expect(asked).toContain("asr:parakeet-local:start"),
    );
    expect(asked.filter((one) => one.startsWith("tts:") && one.endsWith(":start"))).toHaveLength(0);
  });

  it("keeps both disclosures useful when the machine has no voice extension", async () => {
    const { client } = machine(
      absent("no voice transcription engine is registered", [
        "foundation voice could not load its native library",
      ]),
      absent("no speech synthesis engine is registered"),
      { hasAsr: false, hasTts: false },
    );
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /ASR/ }));
    expect(
      await screen.findByText(/could not load its native library/),
    ).toBeTruthy();
    fireEvent.click(screen.getByRole("button", { name: /TTS/ }));
    await waitFor(() => expect(choice(/This device/)).toBeTruthy());
  });
});

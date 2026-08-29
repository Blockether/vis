// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { useState } from "react";

import { SpeechEnginesPanel } from "./SettingsScreen";
import { GatewayError } from "../lib/gateway";
import type { GatewayClient } from "../lib/gateway";
import { getSpeechPrefs } from "../lib/storage";
import type { SpeechPrefs, VoiceModelState } from "../lib/types";

class DevicePreviewUtterance {
  onend: (() => void) | null = null;
  onerror: ((event: { error: string }) => void) | null = null;
  rate = 1;
  voice: SpeechSynthesisVoice | null = null;

  readonly text: string;

  constructor(text: string) {
    this.text = text;
  }
}

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
      speechVoices: vi.fn(({ engine }: { engine?: string | null } = {}) =>
        Promise.resolve({
          engine: { id: engine ?? "piper-local", label: "Piper (local)" },
          voices: [
            {
              id: "amy",
              label: "Amy",
              language: "en-US",
              model: { status: "ready", engine: engine ?? "piper-local" },
            },
          ],
        }),
      ),
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
        { voiceURI: "com.apple.voice.premium.en-US.Samantha", name: "Samantha", lang: "en-US", default: false },
        { voiceURI: "com.apple.voice.premium.en-US.Ava", name: "Ava", lang: "en-US", default: false },
        { voiceURI: "com.apple.voice.enhanced.en-US.Alex", name: "Alex", lang: "en-US", default: false },
        { voiceURI: "com.apple.voice.natural.en-US.Tom", name: "Tom", lang: "en-US", default: false },
        { voiceURI: "com.apple.voice.enhanced.en-US.Zoe", name: "Zoe", lang: "en-US", default: false },
        { voiceURI: "com.apple.voice.compact.en-US.Fred", name: "Fred Compact", lang: "en-US", default: false },
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
    expect(screen.queryByText("Whisper (gateway)")).toBeNull();

    fireEvent.click(asr);
    expect(choice(/Parakeet \(gateway\)/)).toBeTruthy();
    expect(screen.getByText("Whisper (gateway)")).toBeTruthy();
    expect(screen.queryByText("Whisper (local)")).toBeNull();
  });

  it("stores the specific ASR engine chosen from the expanded list", async () => {
    const { client, asked } = machine();
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /ASR/ }));
    const whisper = await screen.findByRole("button", { name: /Whisper \(gateway\)/ });
    fireEvent.click(whisper);

    await waitFor(async () =>
      expect((await getSpeechPrefs()).asrEngine).toBe("whisper-local"),
    );
    expect(whisper.getAttribute("aria-pressed")).toBe("true");
    await waitFor(() => expect(asked).toContain("asr:whisper-local:read"));
  });

  // Regression, user report: selecting a TTS engine also opened its settings, while
  // settings for every other engine were inaccessible from their own rows.
  it("keeps every TTS engine disclosure closed and independent from selection", async () => {
    const { client } = machine();
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /TTS/ }));
    const engines = await screen.findByRole("group", { name: "TTS engines" });
    const device = choice(/This device/)!;
    const piper = choice(/Piper \(gateway\)/)!;
    const disclosures = [
      within(engines).getByRole("button", { name: "Settings for This device" }),
      within(engines).getByRole("button", { name: "Settings for Piper (gateway)" }),
      within(engines).getByRole("button", { name: "Settings for Pocket TTS (gateway)" }),
    ];

    expect(disclosures.map((button) => button.getAttribute("aria-expanded"))).toEqual([
      "false",
      "false",
      "false",
    ]);
    expect(within(engines).queryByRole("group", { name: "Voices" })).toBeNull();

    fireEvent.click(disclosures[1]!);

    expect(device.getAttribute("aria-pressed")).toBe("true");
    expect(piper.getAttribute("aria-pressed")).toBe("false");
    await waitFor(() =>
      expect(client.speechVoices).toHaveBeenCalledWith(
        expect.objectContaining({ engine: "piper-local" }),
      ),
    );
    const piperOwner = piper.closest('[data-speech-engine="piper-local"]') as HTMLElement;
    const deviceOwner = device.closest('[data-speech-engine="device"]') as HTMLElement;
    expect(await within(piperOwner).findByRole("group", { name: "Voices" })).toBeTruthy();

    fireEvent.click(disclosures[0]!);
    expect(within(piperOwner).getByRole("group", { name: "Voices" })).toBeTruthy();
    expect(within(deviceOwner).getByRole("group", { name: "Voices" })).toBeTruthy();

    fireEvent.click(disclosures[1]!);
    expect(within(piperOwner).queryByRole("group", { name: "Voices" })).toBeNull();
    expect(within(deviceOwner).getByRole("group", { name: "Voices" })).toBeTruthy();
  });

  it("separates gateway engines from the selected device's premium voices", async () => {
    const { client } = machine();
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /TTS/ }));
    const engines = await screen.findByRole("group", { name: "TTS engines" });
    const device = choice(/This device/)!;
    const pocket = choice(/Pocket TTS \(gateway\)/)!;
    expect(device.getAttribute("aria-pressed")).toBe("true");
    expect(choice(/Piper \(gateway\)/)).toBeTruthy();
    const deviceEngine = device.closest('[data-speech-engine="device"]') as HTMLElement;
    expect(screen.queryByRole("button", { name: /Piper \(local\)/ })).toBeNull();

    fireEvent.click(
      within(engines).getByRole("button", { name: "Settings for This device" }),
    );
    const deviceVoices = within(deviceEngine).getByRole("group", { name: "Voices" });
    expect(within(deviceVoices).getByRole("button", { name: /^Samantha/ })).toBeTruthy();
    expect(within(deviceVoices).getByRole("button", { name: /^Ava/ })).toBeTruthy();
    expect(within(deviceVoices).getByRole("button", { name: /^Tom/ })).toBeTruthy();
    expect(within(deviceVoices).queryByRole("button", { name: /Piper/ })).toBeNull();
    expect(screen.queryByRole("button", { name: /Alex/ })).toBeNull();
    expect(screen.queryByRole("button", { name: /Zoe/ })).toBeNull();
    expect(screen.queryByRole("button", { name: /Daniel/ })).toBeNull();
    expect(screen.queryByRole("button", { name: /Fred Compact/ })).toBeNull();

    fireEvent.click(screen.getByRole("button", { name: /^Samantha/ }));
    await waitFor(async () =>
      expect((await getSpeechPrefs()).deviceVoice).toBe(
        "com.apple.voice.premium.en-US.Samantha",
      ),
    );
    expect(
      screen.getByRole("button", { name: /TTS.*This device · Samantha/ }),
    ).toBeTruthy();
    fireEvent.click(pocket);

    await waitFor(async () =>
      expect((await getSpeechPrefs()).ttsEngine).toBe("pocket-tts-local"),
    );
    expect(pocket.getAttribute("aria-pressed")).toBe("true");
    expect(screen.getByRole("button", { name: /^Samantha/ })).toBeTruthy();
  });

  // Regression, user report: iOS listed its system voices but gave them no sample action,
  // unlike every gateway voice beside them.
  it("plays and stops an exact system voice on this device", async () => {
    const { client } = machine();
    const spoken: DevicePreviewUtterance[] = [];
    const speak = vi.fn((utterance: DevicePreviewUtterance) => spoken.push(utterance));
    const cancel = vi.fn();
    const synthesis = window.speechSynthesis;
    Object.defineProperty(window, "speechSynthesis", {
      configurable: true,
      value: { getVoices: () => synthesis.getVoices(), speak, cancel },
    });
    vi.stubGlobal("SpeechSynthesisUtterance", DevicePreviewUtterance);
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /TTS/ }));
    const engines = await screen.findByRole("group", { name: "TTS engines" });
    fireEvent.click(within(engines).getByRole("button", { name: "Settings for This device" }));

    fireEvent.click(
      await screen.findByRole("button", { name: "Play a sample of Samantha" }),
    );
    expect(
      await screen.findByRole("button", { name: "Stop the sample of Samantha" }),
    ).toBeTruthy();
    expect(spoken).toHaveLength(1);
    expect(spoken[0]?.text.length).toBeGreaterThan(0);
    expect(spoken[0]?.voice?.voiceURI).toBe("com.apple.voice.premium.en-US.Samantha");
    expect(screen.getByRole("button", { name: "Play a sample of Ava" })).toBeTruthy();

    fireEvent.click(screen.getByRole("button", { name: "Play a sample of Ava" }));
    expect(
      await screen.findByRole("button", { name: "Play a sample of Samantha" }),
    ).toBeTruthy();
    const stopAva = screen.getByRole("button", { name: "Stop the sample of Ava" });
    fireEvent.click(stopAva);

    expect(
      await screen.findByRole("button", { name: "Play a sample of Ava" }),
    ).toBeTruthy();
    expect(cancel).toHaveBeenCalled();
  });

  // Regression, user report: gateway voices were detached into a separate panel below
  // every speech engine instead of belonging to the engine whose disclosure opened.
  it("keeps gateway voices directly under their owning TTS engine", async () => {
    const { client } = machine();
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /TTS/ }));
    const piper = choice(/Piper \(gateway\)/)!;
    fireEvent.click(piper);
    fireEvent.click(screen.getByRole("button", { name: "Settings for Piper (gateway)" }));

    const engine = piper.closest('[data-speech-engine="piper-local"]');
    expect(engine).toBeTruthy();
    const voiceGroup = await within(engine as HTMLElement).findByRole("group", {
      name: "Voices",
    });
    // It belongs to the engine by POSITION: no indent leaves its rows short of the left
    // edge the engine's own row reaches, and a rail down its left says where the cluster
    // begins AND where it stops. The rail is the panel's own hairline ink at double weight,
    // never the selection amber, which would draw structure with the colour of a choice.
    const groupClasses = voiceGroup.className.split(/\s+/);
    expect(groupClasses).not.toContain("pl-3");
    expect(groupClasses).not.toContain("border-accent/40");
    const foot = voiceGroup.lastElementChild as HTMLElement;
    expect(foot.className.split(/\s+/)).toEqual(
      expect.arrayContaining(["h-px", "w-full", "bg-dialog-edge"]),
    );
  });
  it("reads and downloads an unselected engine from its own row", async () => {
    const { client, asked } = machine();
    vi.mocked(client.voiceModel).mockImplementation(
      ({ start = false, engine }: { start?: boolean; engine?: string | null } = {}) => {
        asked.push(`asr:${engine ?? "default"}:${start ? "start" : "read"}`);
        if (engine === "whisper-local") {
          return Promise.resolve(
            start
              ? { status: "downloading", engine, progress: 0 }
              : { status: "absent", engine },
          );
        }
        return Promise.resolve(ready(engine ?? "parakeet-local"));
      },
    );
    render(<Harness client={client} />);

    fireEvent.click(await screen.findByRole("button", { name: /ASR/ }));
    await waitFor(() => expect(asked).toContain("asr:whisper-local:read"));
    expect(choice(/Parakeet \(gateway\)/)?.getAttribute("aria-pressed")).toBe("true");
    fireEvent.click(await screen.findByRole("button", { name: "Download Whisper model" }));

    await waitFor(() => expect(asked).toContain("asr:whisper-local:start"));
    expect(choice(/Parakeet \(gateway\)/)?.getAttribute("aria-pressed")).toBe("true");
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
      (await screen.findAllByText("the archive did not match its checksum")).length,
    ).toBeGreaterThan(0);
    fireEvent.click(screen.getByRole("button", { name: "Retry Parakeet model" }));

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

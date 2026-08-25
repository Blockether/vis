// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { VoicesPanel } from "./SettingsScreen";
import { speechOutput } from "../lib/speech";
import type { GatewayClient } from "../lib/gateway";
import type { SpeechPrefs, SpeechVoices } from "../lib/types";

const prefs: SpeechPrefs = {
  asrEngine: null,
  ttsEngine: "piper-local",
  deviceVoice: null,
  gatewayVoice: null,
  rate: 1,
};

beforeEach(() => localStorage.clear());
afterEach(() => {
  document.body.innerHTML = "";
  vi.restoreAllMocks();
});

describe("licence-gated gateway voices", () => {
  // Regression, user report: an unavailable voice showed a choice mark beside its
  // download action, making one row look like two competing controls.
  it("replaces the unavailable choice mark with its download action", async () => {
    const catalogue: SpeechVoices = {
      engine: { id: "piper-local", label: "Piper (local)" },
      voices: [
        {
          id: "ryan",
          label: "Ryan (en-US, high)",
          language: "en-US",
          is_opt_in: true,
          license: "CC-BY-NC-SA-4.0",
          notice: "Non-commercial use only, with attribution.",
          source_url: "https://example.com/ryan",
          model: { status: "absent", engine: "piper-local" },
        },
      ],
    };
    const client = {
      speechVoices: vi.fn().mockResolvedValue(catalogue),
      speechModel: vi.fn().mockResolvedValue({
        status: "downloading",
        engine: "piper-local",
        progress: 0,
      }),
    } as unknown as GatewayClient;
    const onChange = vi.fn().mockResolvedValue(prefs);

    render(<VoicesPanel client={client} prefs={prefs} onChange={onChange} />);
    const voiceChoice = await screen.findByRole("button", { name: /Ryan/ });
    const voiceRow = voiceChoice.parentElement;
    expect(voiceRow).toBeTruthy();
    expect(voiceChoice.textContent).not.toContain("○");
    const download = within(voiceRow as HTMLElement).getByRole("button", { name: "Download" });
    fireEvent.click(download);

    expect(screen.getByText("Non-commercial use only, with attribution.")).toBeTruthy();
    expect(screen.getByText(/CC-BY-NC-SA-4.0/)).toBeTruthy();
    expect(client.speechModel).not.toHaveBeenCalled();

    fireEvent.click(screen.getByRole("button", { name: "Accept and download" }));
    await waitFor(() =>
      expect(client.speechModel).toHaveBeenCalledWith({
        start: true,
        engine: "piper-local",
        voice: "ryan",
        isLicenseAccepted: true,
      }),
    );
  });
});

describe("hearing a voice before choosing it", () => {
  const catalogue: SpeechVoices = {
    engine: { id: "piper-local", label: "Piper (local)" },
    voices: [
      {
        id: "kristin",
        label: "Kristin (en-US, medium)",
        language: "en-US",
        is_sample_ready: true,
        model: { status: "absent", engine: "piper-local" },
      },
      {
        id: "cori",
        label: "Cori (en-GB, high)",
        language: "en-GB",
        is_sample_preparable: true,
        model: { status: "ready", engine: "piper-local" },
      },
      {
        id: "mystery",
        label: "Mystery (en-US)",
        language: "en-US",
        model: { status: "ready", engine: "piper-local" },
      },
    ],
  };

  function mount(sample: Blob) {
    const client = {
      speechVoices: vi.fn().mockResolvedValue(catalogue),
      speechVoiceSample: vi.fn().mockResolvedValue(sample),
      speechModel: vi.fn(),
    } as unknown as GatewayClient;
    const onChange = vi.fn().mockResolvedValue(prefs);
    const played = vi
      .spyOn(speechOutput, "playSample")
      .mockResolvedValue(undefined);
    render(<VoicesPanel client={client} prefs={prefs} onChange={onChange} />);
    return { client, onChange, played };
  }

  // A Piper voice is a 60-100 MB download, so a catalogue that can only be READ makes
  // every choice blind: the sample has to play while the model is still absent.
  it("plays a voice whose model is not downloaded", async () => {
    const sample = new Blob(["wav"], { type: "audio/wav" });
    const { client, played } = mount(sample);

    fireEvent.click(
      await screen.findByRole("button", {
        name: "Play a sample of Kristin (en-US, medium)",
      }),
    );

    await waitFor(() =>
      expect(client.speechVoiceSample).toHaveBeenCalledWith("kristin", {
        engine: "piper-local",
      }),
    );
    await waitFor(() => expect(played).toHaveBeenCalledWith(sample));
    expect(client.speechModel).not.toHaveBeenCalled();
  });

  // Regression, session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25: every sample
  // was a worded button at the far right, detached from the voice it lets you hear.
  it("puts an icon-only sample action before the voice name", async () => {
    mount(new Blob(["wav"], { type: "audio/wav" }));

    const play = await screen.findByRole("button", {
      name: "Play a sample of Kristin (en-US, medium)",
    });
    const voice = screen.getByRole("button", { name: /^Kristin/ });
    const row = voice.parentElement;

    expect(row?.firstElementChild).toBe(play);
    expect(row?.children.item(1)).toBe(voice);
    expect(play.textContent).toBe("");
    expect(play.querySelector("svg")).not.toBeNull();
  });

  it("offers no sample where there is nothing to play", async () => {
    mount(new Blob(["wav"], { type: "audio/wav" }));

    await screen.findByRole("button", { name: /^Kristin/ });
    expect(
      screen.queryByRole("button", { name: /Play a sample of Mystery/ }),
    ).toBeNull();
  });

  // Choosing is silent otherwise: the preference is saved and nothing tells the ear
  // what it just bought.
  it("plays the voice it has just chosen", async () => {
    const sample = new Blob(["wav"], { type: "audio/wav" });
    const { client, onChange, played } = mount(sample);

    fireEvent.click(await screen.findByRole("button", { name: /^Cori/ }));

    await waitFor(() => expect(onChange).toHaveBeenCalled());
    await waitFor(() => expect(played).toHaveBeenCalledWith(sample));
    expect(client.speechVoiceSample).toHaveBeenCalledWith("cori", {
      engine: "piper-local",
    });
  });
});

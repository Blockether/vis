// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { VoicesPanel } from "./settings/SpeechSettings";
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
  // Regression, user report: a voice that still needed its model put Play on the left and a
  // worded Download action on the right. Download is the row's one leading action until ready.
  it("puts an icon-only download action before an unavailable voice", async () => {
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
    const voiceChoice = await screen.findByRole("button", {
      name: /^Ryan \(en-US, high\)en-US/,
    });
    const actionStrip = voiceChoice.parentElement;
    expect(actionStrip).toBeTruthy();
    expect(voiceChoice.textContent).not.toContain("○");
    const download = within(actionStrip as HTMLElement).getByRole("button", {
      name: "Download Ryan (en-US, high)",
    });
    expect(actionStrip?.firstElementChild).toBe(download);
    expect(actionStrip?.children.item(1)).toBe(voiceChoice);
    expect(download.textContent).toBe("");
    expect(download.querySelector("svg")).not.toBeNull();
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
        model: { status: "ready", engine: "piper-local" },
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

  function mount(sample: Blob, answer: SpeechVoices = catalogue) {
    const client = {
      speechVoices: vi.fn().mockResolvedValue(answer),
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

  // Regression, user report: even a cheap ready-made sample must not disguise that the
  // selected voice itself still needs a model download. Download owns the leading square first.
  it("shows download instead of play while the voice model is absent", async () => {
    const unavailable: SpeechVoices = {
      ...catalogue,
      voices: catalogue.voices.map((voice) =>
        voice.id === "kristin"
          ? { ...voice, model: { status: "absent", engine: "piper-local" } }
          : voice,
      ),
    };
    mount(new Blob(["wav"], { type: "audio/wav" }), unavailable);

    expect(
      await screen.findByRole("button", {
        name: "Download Kristin (en-US, medium)",
      }),
    ).toBeTruthy();
    expect(
      screen.queryByRole("button", {
        name: "Play a sample of Kristin (en-US, medium)",
      }),
    ).toBeNull();
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

  // Regression, user report: an active sample still looked like Play, so there was no way
  // to stop it or see which voice owned the one shared player.
  it("turns play into stop and hands that control to the next voice", async () => {
    const sample = new Blob(["wav"], { type: "audio/wav" });
    const client = {
      speechVoices: vi.fn().mockResolvedValue(catalogue),
      speechVoiceSample: vi.fn().mockResolvedValue(sample),
      speechModel: vi.fn(),
    } as unknown as GatewayClient;
    vi.spyOn(speechOutput, "playSample").mockImplementation(
      () => new Promise<void>(() => undefined),
    );
    const stopped = vi.spyOn(speechOutput, "stop");
    render(<VoicesPanel client={client} prefs={prefs} onChange={vi.fn()} />);

    fireEvent.click(
      await screen.findByRole("button", {
        name: "Play a sample of Kristin (en-US, medium)",
      }),
    );

    expect(
      await screen.findByRole("button", {
        name: "Stop the sample of Kristin (en-US, medium)",
      }),
    ).toBeTruthy();
    fireEvent.click(
      screen.getByRole("button", { name: "Play a sample of Cori (en-GB, high)" }),
    );

    expect(
      await screen.findByRole("button", {
        name: "Play a sample of Kristin (en-US, medium)",
      }),
    ).toBeTruthy();
    const stopCori = screen.getByRole("button", {
      name: "Stop the sample of Cori (en-GB, high)",
    });
    fireEvent.click(stopCori);

    expect(
      await screen.findByRole("button", {
        name: "Play a sample of Cori (en-GB, high)",
      }),
    ).toBeTruthy();
    expect(stopped).toHaveBeenCalledTimes(2);
  });

  it("offers no sample where there is nothing to play", async () => {
    mount(new Blob(["wav"], { type: "audio/wav" }));

    await screen.findByRole("button", { name: /^Kristin/ });
    expect(
      screen.queryByRole("button", { name: /Play a sample of Mystery/ }),
    ).toBeNull();
  });

  // Regression, user report: moving from one voice to another showed the first sample's
  // interrupted stream as an error instead of treating the newer audition as its replacement.
  it("silently replaces an interrupted sample stream", async () => {
    const sample = new Blob(["wav"], { type: "audio/wav" });
    const interrupted = new Error("The sample stream was interrupted.");
    let firstSignal: AbortSignal | undefined;
    let firstSettled = false;
    let rejectFirst: ((reason: Error) => void) | null = null;
    const client = {
      speechVoices: vi.fn().mockResolvedValue(catalogue),
      speechVoiceSample: vi.fn(
        (id: string, options: { signal?: AbortSignal } = {}) => {
          if (id === "kristin") {
            firstSignal = options.signal;
            return new Promise<Blob>((_resolve, reject) => {
              rejectFirst = (reason) => {
                firstSettled = true;
                reject(reason);
              };
              options.signal?.addEventListener(
                "abort",
                () => rejectFirst?.(interrupted),
                { once: true },
              );
            });
          }
          rejectFirst?.(interrupted);
          return Promise.resolve(sample);
        },
      ),
      speechModel: vi.fn(),
    } as unknown as GatewayClient;
    const played = vi.spyOn(speechOutput, "playSample").mockResolvedValue(undefined);
    render(<VoicesPanel client={client} prefs={prefs} onChange={vi.fn()} />);

    fireEvent.click(
      await screen.findByRole("button", {
        name: "Play a sample of Kristin (en-US, medium)",
      }),
    );
    await waitFor(() => expect(client.speechVoiceSample).toHaveBeenCalledTimes(1));
    fireEvent.click(
      screen.getByRole("button", { name: "Play a sample of Cori (en-GB, high)" }),
    );

    await waitFor(() => expect(played).toHaveBeenCalledWith(sample));
    await waitFor(() => expect(firstSettled).toBe(true));
    expect(firstSignal?.aborted).toBe(true);
    expect(screen.queryByText(interrupted.message)).toBeNull();
  });

  // Choosing is silent otherwise: the preference is saved and nothing tells the ear
  // what it just bought.
  it("plays the voice it has just chosen", async () => {
    const sample = new Blob(["wav"], { type: "audio/wav" });
    const { client, onChange, played } = mount(sample);

    fireEvent.click(await screen.findByRole("button", { name: /^Cori/ }));

    await waitFor(() => expect(onChange).toHaveBeenCalled());
    await waitFor(() => expect(played).toHaveBeenCalledWith(sample));
    expect(client.speechVoiceSample).toHaveBeenCalledWith(
      "cori",
      expect.objectContaining({ engine: "piper-local" }),
    );
  });
});

describe("imported voice actions", () => {
  // Regression, user report: Forget occupied a permanent column instead of the shared drawer.
  it("reveals an icon and Forget action in the voice drawer and confirms deletion", async () => {
    const voice = { id: "my-voice", label: "My voice", is_imported: true };
    const catalogue: SpeechVoices = {
      engine: { id: "piper-local", label: "Piper" },
      voices: [voice],
    };
    const client = {
      speechVoices: vi.fn().mockResolvedValue(catalogue),
      forgetSpeechVoice: vi.fn().mockImplementation(async () => {
        catalogue.voices = [];
      }),
    } as unknown as GatewayClient;
    render(<VoicesPanel client={client} prefs={prefs} onChange={vi.fn()} />);
    const choice = await screen.findByRole("button", { name: /^My voice/ });
    const track = choice.closest<HTMLElement>("[data-swipe-track]");
    expect(track).not.toBeNull();
    const action = within(track!).getByRole("button", {
      name: "Forget My voice",
    });
    expect(action).toHaveTextContent("Forget");
    expect(action.querySelector("svg")).not.toBeNull();
    track!.scrollTo = vi.fn();
    fireEvent.click(action);
    expect(client.forgetSpeechVoice).not.toHaveBeenCalled();
    const confirmation = screen.getByRole("group", {
      name: "Forget My voice?",
    });
    fireEvent.click(
      within(confirmation).getByRole("button", { name: "Forget" }),
    );
    await waitFor(() =>
      expect(client.forgetSpeechVoice).toHaveBeenCalledWith("my-voice", {
        engine: "piper-local",
      }),
    );
    await waitFor(() =>
      expect(screen.queryByRole("button", { name: /^My voice/ })).toBeNull(),
    );
  });
});

describe("testing custom speech", () => {
  const audio = new Blob(["wav"], { type: "audio/wav" });
  function mount(gatewayVoice: string | null = "my-voice") {
    const client = {
      speechVoices: vi
        .fn()
        .mockResolvedValue({
          voices: [{ id: "my-voice", label: "My voice", is_imported: true }],
        }),
      speakText: vi.fn().mockResolvedValue(audio),
      speechVoiceSample: vi.fn(),
    } as unknown as GatewayClient;
    const onChange = vi.fn();
    const played = vi
      .spyOn(speechOutput, "playSample")
      .mockResolvedValue(undefined);
    const stopped = vi.spyOn(speechOutput, "stop").mockImplementation(() => {});
    const view = render(
      <VoicesPanel
        client={client}
        prefs={{ ...prefs, gatewayVoice }}
        engine="chatterbox-local"
        onChange={onChange}
      />,
    );
    return { ...view, client, onChange, played, stopped };
  }
  async function enter(text = "This is my own test sentence.") {
    const input = await screen.findByRole("textbox", {
      name: "Text to synthesize",
    });
    fireEvent.change(input, { target: { value: text } });
    return screen.getByRole("button", { name: "Test" });
  }
  it("synthesizes entered text with this engine and the selected voice without saving preferences", async () => {
    const { client, onChange, played } = mount();
    fireEvent.click(await enter());
    await waitFor(() => expect(played).toHaveBeenCalledWith(audio));
    expect(client.speakText).toHaveBeenCalledWith(
      null,
      "This is my own test sentence.",
      {
        engine: "chatterbox-local",
        voice: "my-voice",
        signal: expect.any(AbortSignal),
      },
    );
    expect(client.speechVoiceSample).not.toHaveBeenCalled();
    expect(onChange).not.toHaveBeenCalled();
  });
  it("uses the engine default when no named voice is selected", async () => {
    const { client, played } = mount(null);
    fireEvent.click(await enter());
    await waitFor(() => expect(played).toHaveBeenCalledWith(audio));
    expect(client.speakText).toHaveBeenCalledWith(
      null,
      expect.any(String),
      expect.objectContaining({ voice: null }),
    );
  });
  it("does not submit empty or whitespace-only text", async () => {
    const { client } = mount();
    expect(await enter("   ")).toBeDisabled();
    expect(client.speakText).not.toHaveBeenCalled();
  });
  it("can stop synthesis and ignores a late audio response", async () => {
    const { client, played, stopped } = mount();
    let finish!: (value: Blob) => void;
    vi.mocked(client.speakText).mockReturnValue(
      new Promise((resolve) => {
        finish = resolve;
      }),
    );
    fireEvent.click(await enter());
    expect(screen.getByRole("status")).toHaveTextContent("Synthesizing");
    const signal = vi.mocked(client.speakText).mock.calls[0]?.[2]?.signal;
    fireEvent.click(screen.getByRole("button", { name: "Stop test" }));
    expect(signal?.aborted).toBe(true);
    expect(stopped).toHaveBeenCalled();
    finish(audio);
    await waitFor(() =>
      expect(screen.getByRole("button", { name: "Test" })).toBeEnabled(),
    );
    expect(played).not.toHaveBeenCalled();
  });
  it("can stop playback and cancels it when the panel closes", async () => {
    const { played, stopped, unmount } = mount();
    played.mockReturnValue(new Promise(() => {}));
    fireEvent.click(await enter());
    await waitFor(() =>
      expect(screen.getByRole("status")).toHaveTextContent("Playing"),
    );
    fireEvent.click(screen.getByRole("button", { name: "Stop test" }));
    expect(stopped).toHaveBeenCalledTimes(1);
    fireEvent.click(screen.getByRole("button", { name: "Test" }));
    await waitFor(() => expect(played).toHaveBeenCalledTimes(2));
    unmount();
    expect(stopped).toHaveBeenCalledTimes(2);
  });
  it("keeps the text after a failure so Test retries it", async () => {
    const { client, played } = mount();
    vi.mocked(client.speakText).mockRejectedValueOnce(
      new Error("The machine is offline."),
    );
    fireEvent.click(await enter());
    await screen.findByText(/The machine is offline/);
    expect(
      screen.getByRole("textbox", { name: "Text to synthesize" }),
    ).toHaveValue("This is my own test sentence.");
    fireEvent.click(screen.getByRole("button", { name: "Test" }));
    await waitFor(() => expect(played).toHaveBeenCalledWith(audio));
  });
});

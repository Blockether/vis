// @vitest-environment jsdom
import {
  act,
  fireEvent,
  render,
  screen,
  waitFor,
} from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { VoicesPanel } from "./settings/SpeechSettings";
import type { GatewayClient } from "../lib/gateway";
import type { SpeechPrefs, SpeechVoices } from "../lib/types";
import { startWavRecording, type WavRecording } from "../lib/voice";

vi.mock("../lib/voice", () => ({ startWavRecording: vi.fn() }));

const prefs: SpeechPrefs = {
  asrEngine: null,
  ttsEngine: "chatterbox-local",
  deviceVoice: null,
  gatewayVoice: null,
  rate: 1,
};
const catalogue: SpeechVoices = {
  engine: {
    id: "chatterbox-local",
    label: "Chatterbox",
    is_voice_import: true,
  },
  voices: [],
};

function mount(answer = catalogue) {
  const client = {
    speechVoices: vi.fn().mockResolvedValue(answer),
    importSpeechVoice: vi
      .fn()
      .mockResolvedValue({ id: "my-voice", label: "My voice" }),
  };
  const view = render(
    <VoicesPanel
      client={client as unknown as GatewayClient}
      prefs={prefs}
      onChange={vi.fn()}
    />,
  );
  return { ...view, client };
}

function recorder(): WavRecording {
  return {
    stop: vi
      .fn()
      .mockResolvedValue(new Blob(["recorded speech"], { type: "audio/wav" })),
    cancel: vi.fn().mockResolvedValue(undefined),
    isCapturing: () => true,
  };
}

beforeEach(() => {
  vi.mocked(startWavRecording).mockReset();
});
afterEach(() => vi.restoreAllMocks());

describe("creating a voice from a recording or a file", () => {
  // Regression, user report: an edge-to-edge import action offered no microphone path.
  it("offers recording and file import as separate actions", async () => {
    mount();
    expect(
      await screen.findByRole("button", { name: "Record your voice" }),
    ).toBeEnabled();
    expect(screen.getByRole("button", { name: "Import voice" })).toBeEnabled();
    expect(startWavRecording).not.toHaveBeenCalled();
  });

  it("records locally and only uploads after confirmation", async () => {
    const take = recorder();
    vi.mocked(startWavRecording).mockResolvedValue(take);
    const { client } = mount();
    fireEvent.click(
      await screen.findByRole("button", { name: "Record your voice" }),
    );
    fireEvent.click(
      await screen.findByRole("button", { name: "Stop recording" }),
    );
    expect(await screen.findByDisplayValue("My voice")).toBeVisible();
    expect(take.stop).toHaveBeenCalledOnce();
    expect(client.importSpeechVoice).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole("button", { name: "Save voice" }));
    await waitFor(() =>
      expect(client.importSpeechVoice).toHaveBeenCalledOnce(),
    );
    const [clip, metadata, options] = client.importSpeechVoice.mock.calls[0];
    expect(clip).toBeInstanceOf(File);
    expect(clip.type).toBe("audio/wav");
    expect(metadata).toEqual({
      name: "My voice",
      lang: undefined,
      text: undefined,
    });
    expect(options).toEqual({ engine: "chatterbox-local" });
    expect(
      await screen.findByText("My voice can speak on this machine now."),
    ).toBeVisible();
  });

  it("keeps file import available and does not request microphone access", async () => {
    const { client } = mount();
    const button = await screen.findByRole("button", { name: "Import voice" });
    const input = screen.getByLabelText("Recording to import as a voice");
    const picker = vi.spyOn(input, "click");
    fireEvent.click(button);
    expect(picker).toHaveBeenCalledOnce();
    const clip = new File(["audio"], "Clear-speech.wav", { type: "audio/wav" });
    fireEvent.change(input, { target: { files: [clip] } });
    expect(screen.getByDisplayValue("Clear speech")).toBeVisible();
    fireEvent.click(screen.getByRole("button", { name: "Save voice" }));
    await waitFor(() =>
      expect(client.importSpeechVoice).toHaveBeenCalledWith(
        clip,
        { name: "Clear speech", lang: undefined, text: undefined },
        { engine: "chatterbox-local" },
      ),
    );
    expect(startWavRecording).not.toHaveBeenCalled();
  });

  it("releases the microphone when recording is cancelled", async () => {
    const take = recorder();
    vi.mocked(startWavRecording).mockResolvedValue(take);
    const { client } = mount();
    fireEvent.click(
      await screen.findByRole("button", { name: "Record your voice" }),
    );
    await screen.findByRole("button", { name: "Stop recording" });
    expect(screen.queryByRole("button", { name: "Import voice" })).toBeNull();
    fireEvent.click(screen.getByRole("button", { name: "Cancel recording" }));
    expect(take.cancel).toHaveBeenCalledOnce();
    expect(
      await screen.findByRole("button", { name: "Record your voice" }),
    ).toBeEnabled();
    expect(client.importSpeechVoice).not.toHaveBeenCalled();
  });

  it.each(["cancel", "unmount"])(
    "releases a late microphone after %s while permission is pending",
    async (action) => {
      let finish!: (value: WavRecording) => void;
      vi.mocked(startWavRecording).mockReturnValue(
        new Promise((resolve) => {
          finish = resolve;
        }),
      );
      const take = recorder();
      const { unmount, client } = mount();
      fireEvent.click(
        await screen.findByRole("button", { name: "Record your voice" }),
      );
      if (action === "cancel")
        fireEvent.click(
          screen.getByRole("button", { name: "Cancel recording" }),
        );
      else unmount();
      await act(async () => {
        finish(take);
      });
      expect(take.cancel).toHaveBeenCalledOnce();
      expect(client.importSpeechVoice).not.toHaveBeenCalled();
    },
  );

  it("releases an active microphone on unmount", async () => {
    const take = recorder();
    vi.mocked(startWavRecording).mockResolvedValue(take);
    const { unmount } = mount();
    fireEvent.click(
      await screen.findByRole("button", { name: "Record your voice" }),
    );
    await screen.findByRole("button", { name: "Stop recording" });
    unmount();
    expect(take.cancel).toHaveBeenCalledOnce();
  });

  it("offers a retry or import after microphone permission is denied", async () => {
    vi.mocked(startWavRecording).mockImplementation(async () => {
      throw new Error("Microphone permission denied");
    });
    mount();
    const record = await screen.findByRole("button", {
      name: "Record your voice",
    });
    await act(async () => {
      fireEvent.click(record);
    });
    expect(
      await screen.findByText(/Microphone permission denied/),
    ).toBeVisible();
    expect(
      screen.getByRole("button", { name: "Record your voice" }),
    ).toBeEnabled();
    expect(screen.getByRole("button", { name: "Import voice" })).toBeEnabled();
  });

  it("recovers when capture is interrupted", async () => {
    const take = recorder();
    vi.mocked(startWavRecording).mockResolvedValue(take);
    mount();
    fireEvent.click(
      await screen.findByRole("button", { name: "Record your voice" }),
    );
    await screen.findByRole("button", { name: "Stop recording" });
    act(() =>
      vi
        .mocked(startWavRecording)
        .mock.calls[0][0]?.onInterrupted?.("Microphone disconnected"),
    );
    expect(take.cancel).toHaveBeenCalledOnce();
    expect(await screen.findByText(/Recording interrupted/)).toBeVisible();
    expect(screen.getByRole("button", { name: "Import voice" })).toBeEnabled();
  });

  it("keeps the file and name after a failed save so it can be retried", async () => {
    const { client } = mount();
    client.importSpeechVoice.mockRejectedValueOnce(
      new Error("Machine disconnected"),
    );
    await screen.findByRole("button", { name: "Import voice" });
    fireEvent.change(screen.getByLabelText("Recording to import as a voice"), {
      target: {
        files: [new File(["speech"], "My voice.wav", { type: "audio/wav" })],
      },
    });
    fireEvent.click(screen.getByRole("button", { name: "Save voice" }));
    expect(
      screen.getByRole("button", { name: "Saving voice…" }),
    ).toBeDisabled();
    expect(screen.getByRole("button", { name: "Cancel" })).toBeDisabled();
    expect(await screen.findByText("Machine disconnected")).toBeVisible();
    expect(screen.getByDisplayValue("My voice")).toBeVisible();
    fireEvent.click(screen.getByRole("button", { name: "Save voice" }));
    expect(
      await screen.findByText("My voice can speak on this machine now."),
    ).toBeVisible();
    expect(client.importSpeechVoice).toHaveBeenCalledTimes(2);
  });

  it("recovers without offering a silent recording for upload", async () => {
    const take = recorder();
    vi.mocked(take.stop).mockRejectedValue(
      new Error("Microphone captured only silence"),
    );
    vi.mocked(startWavRecording).mockResolvedValue(take);
    const { client } = mount();
    fireEvent.click(
      await screen.findByRole("button", { name: "Record your voice" }),
    );
    fireEvent.click(
      await screen.findByRole("button", { name: "Stop recording" }),
    );
    expect(
      await screen.findByText(/Microphone captured only silence/),
    ).toBeVisible();
    expect(
      screen.getByRole("button", { name: "Record your voice" }),
    ).toBeEnabled();
    expect(screen.queryByRole("button", { name: "Save voice" })).toBeNull();
    expect(client.importSpeechVoice).not.toHaveBeenCalled();
  });

  it("releases capture when the selected engine changes", async () => {
    const take = recorder();
    vi.mocked(startWavRecording).mockResolvedValue(take);
    const { client, rerender } = mount();
    fireEvent.click(
      await screen.findByRole("button", { name: "Record your voice" }),
    );
    await screen.findByRole("button", { name: "Stop recording" });
    rerender(
      <VoicesPanel
        client={client as unknown as GatewayClient}
        prefs={prefs}
        engine="another-engine"
        onChange={vi.fn()}
      />,
    );
    expect(take.cancel).toHaveBeenCalledOnce();
    expect(
      screen.getByRole("button", { name: "Record your voice" }),
    ).toBeEnabled();
  });
  it("does not offer voice creation for engines without import support", async () => {
    mount({ engine: { id: "piper-local" }, voices: [] });
    await screen.findByText("This engine speaks in no named voice.");
    expect(
      screen.queryByRole("button", { name: "Record your voice" }),
    ).toBeNull();
    expect(screen.queryByRole("button", { name: "Import voice" })).toBeNull();
  });
});

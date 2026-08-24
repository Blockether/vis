// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { VoicesPanel } from "./SettingsScreen";
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

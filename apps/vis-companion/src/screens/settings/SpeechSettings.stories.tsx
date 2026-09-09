import { useMemo } from "react";
import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, userEvent, within } from "storybook/test";
import type { SpeechPrefs, SpeechVoice } from "../../lib/types";
import { VoicesPanel } from "./SpeechSettings";
import { SettingsPanel } from "./SettingsLayout";

const prefs: SpeechPrefs = {
  asrEngine: null,
  ttsEngine: "chatterbox-local",
  deviceVoice: null,
  gatewayVoice: null,
  rate: 1,
};

type VoiceClient = Parameters<typeof VoicesPanel>[0]["client"];
/** An in-memory gateway fixture shared by the stories and the offline review. */
function fixtureClient(hasVoice: boolean, failsToSave: boolean): VoiceClient {
  let voices: SpeechVoice[] = hasVoice
    ? [{ id: "my-voice", label: "My voice", language: "en", is_imported: true }]
    : [];
  return {
    speechVoices: async () => ({
      engine: {
        id: "chatterbox-local",
        label: "Chatterbox",
        is_voice_import: true,
      },
      voices,
    }),
    importSpeechVoice: async (_file, metadata) => {
      if (failsToSave)
        throw new Error(
          "Could not save this voice. Check the machine connection and try again.",
        );
      const voice = {
        id: "new-voice",
        label: metadata.name,
        language: metadata.lang,
        is_imported: true,
      };
      voices = [...voices, voice];
      return voice;
    },
    forgetSpeechVoice: async (id) => {
      voices = voices.filter((voice) => voice.id !== id);
    },
  } as VoiceClient;
}

/** Only the gateway is replaced; recording uses the production microphone adapter. */
function VoiceSetup({ hasVoice = false, failsToSave = false }) {
  const client = useMemo(
    () => fixtureClient(hasVoice, failsToSave),
    [hasVoice, failsToSave],
  );
  return (
    <SettingsPanel title="Chatterbox">
      <VoicesPanel client={client} prefs={prefs} onChange={async () => prefs} />
    </SettingsPanel>
  );
}

const meta = {
  title: "Screens/Voice setup",
  component: VoiceSetup,
  parameters: { layout: "fullscreen" },
  args: { hasVoice: false, failsToSave: false },
} satisfies Meta<typeof VoiceSetup>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Empty: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const record = await canvas.findByRole("button", {
      name: "Record your voice",
    });
    const upload = canvas.getByRole("button", { name: "Import voice" });
    const group = record.closest("section")!;
    const box = record.getBoundingClientRect();
    const groupBox = group.getBoundingClientRect();
    const uploadBox = upload.getBoundingClientRect();
    await expect(box.width).toBeLessThan(groupBox.width - 24);
    await expect(box.height).toBe(32);
    await expect(uploadBox.height).toBe(box.height);
    await expect(box.left - groupBox.left).toBeGreaterThanOrEqual(12);
    await expect(groupBox.right - uploadBox.right).toBeGreaterThanOrEqual(12);
    await expect(uploadBox.left - box.right).toBeGreaterThanOrEqual(8);
    await expect(
      parseFloat(getComputedStyle(record).fontSize),
    ).toBeGreaterThanOrEqual(11);
    if (!matchMedia("(min-width: 640px) and (pointer: fine)").matches) {
      const target = getComputedStyle(record, "::after");
      // The pseudo-element starts at the padding edge, inside the two border pixels.
      const reach =
        box.height - 2 - parseFloat(target.top) - parseFloat(target.bottom);
      await expect(reach).toBeGreaterThanOrEqual(44);
    }
  },
};

export const ExistingVoice: Story = { args: { hasVoice: true } };

async function importFile(canvasElement: HTMLElement) {
  const canvas = within(canvasElement);
  await canvas.findByRole("button", { name: "Import voice" });
  await userEvent.upload(
    canvas.getByLabelText("Recording to import as a voice"),
    new File(["voice fixture"], "My voice.wav", { type: "audio/wav" }),
  );
  await expect(canvas.getByRole("textbox", { name: "Name" })).toHaveValue(
    "My voice",
  );
  return canvas;
}

export const ReviewRecording: Story = {
  play: async ({ canvasElement }) => {
    await importFile(canvasElement);
  },
};

export const SaveVoice: Story = {
  play: async ({ canvasElement }) => {
    const canvas = await importFile(canvasElement);
    await userEvent.click(canvas.getByRole("button", { name: "Save voice" }));
    await expect(
      await canvas.findByText("My voice can speak on this machine now."),
    ).toBeVisible();
    await expect(
      canvas.getByRole("button", { name: "Record your voice" }),
    ).toBeVisible();
  },
};

export const SaveFailure: Story = {
  args: { failsToSave: true },
  play: async ({ canvasElement }) => {
    const canvas = await importFile(canvasElement);
    await userEvent.click(canvas.getByRole("button", { name: "Save voice" }));
    await expect(
      await canvas.findByText(/Could not save this voice/),
    ).toBeVisible();
    await expect(canvas.getByRole("textbox", { name: "Name" })).toHaveValue(
      "My voice",
    );
    await expect(
      canvas.getByRole("button", { name: "Save voice" }),
    ).toBeEnabled();
  },
};

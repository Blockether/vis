import { useMemo, useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import type { SpeechPrefs, SpeechVoice } from '../../lib/types';
import { VoicesPanel } from './SpeechSettings';
import { SettingsPanel } from './SettingsLayout';
import { getSpeechPrefs } from '../../lib/storage';

const prefs: SpeechPrefs = {
  asrEngine: null,
  ttsEngine: 'chatterbox-local',
  deviceVoice: null,
  gatewayVoice: null,
  rate: 1,
};

type VoiceClient = Parameters<typeof VoicesPanel>[0]['client'];
/** In-memory gateway: synthesis stays pending for the cancellable loading state. */
function fixtureClient(
  hasVoice: boolean,
  failsToSave: boolean,
  failsToSpeak: boolean,
): VoiceClient {
  let voices: SpeechVoice[] = hasVoice
    ? [{ id: 'my-voice', label: 'My voice', language: 'en', is_imported: true }]
    : [];
  const client: Pick<
    VoiceClient,
    'speechVoices' | 'speakText' | 'importSpeechVoice' | 'forgetSpeechVoice'
  > = {
    speechVoices: async () => ({
      engine: {
        id: 'chatterbox-local',
        label: 'Chatterbox',
        is_voice_import: true,
      },
      voices,
    }),
    speakText: async () => {
      if (failsToSpeak)
        throw new Error('Could not synthesize speech. Check the machine connection and try again.');
      return new Promise<Blob>(() => {});
    },
    importSpeechVoice: async (_file, metadata) => {
      if (failsToSave)
        throw new Error('Could not save this voice. Check the machine connection and try again.');
      const voice = {
        id: 'new-voice',
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
  };
  return client as VoiceClient;
}

/** Only the gateway is replaced; recording uses the production microphone adapter. */
function VoiceSetup({ hasVoice = false, failsToSave = false, failsToSpeak = false }) {
  const client = useMemo(
    () => fixtureClient(hasVoice, failsToSave, failsToSpeak),
    [hasVoice, failsToSave, failsToSpeak],
  );
  const [selectedPrefs, setSelectedPrefs] = useState({
    ...prefs,
    gatewayVoice: hasVoice ? 'my-voice' : null,
  });
  return (
    <SettingsPanel title="Chatterbox">
      <VoicesPanel
        client={client}
        prefs={selectedPrefs}
        onChange={async (write) => {
          await write();
          const next = {
            ...prefs,
            gatewayVoice: (await getSpeechPrefs()).gatewayVoice,
          };
          setSelectedPrefs(next);
          return next;
        }}
      />
    </SettingsPanel>
  );
}

const meta = {
  title: 'Screens/Voice setup',
  component: VoiceSetup,
  parameters: { layout: 'fullscreen' },
  args: { hasVoice: false, failsToSave: false, failsToSpeak: false },
} satisfies Meta<typeof VoiceSetup>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Empty: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const record = await canvas.findByRole('button', {
      name: 'Record your voice',
    });
    const upload = canvas.getByRole('button', { name: 'Import voice' });
    const group = record.closest('section')!;
    const box = record.getBoundingClientRect();
    const groupBox = group.getBoundingClientRect();
    const uploadBox = upload.getBoundingClientRect();
    await expect(box.width).toBeLessThan(groupBox.width - 24);
    await expect(box.height).toBe(32);
    await expect(uploadBox.height).toBe(box.height);
    await expect(box.left - groupBox.left).toBeGreaterThanOrEqual(12);
    await expect(groupBox.right - uploadBox.right).toBeGreaterThanOrEqual(12);
    await expect(uploadBox.left - box.right).toBeGreaterThanOrEqual(8);
    await expect(parseFloat(getComputedStyle(record).fontSize)).toBeGreaterThanOrEqual(11);
    if (!matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      const target = getComputedStyle(record, '::after');
      // The pseudo-element starts at the padding edge, inside the two border pixels.
      const reach = box.height - 2 - parseFloat(target.top) - parseFloat(target.bottom);
      await expect(reach).toBeGreaterThanOrEqual(44);
    }
  },
};

export const ExistingVoice: Story = {
  args: { hasVoice: true },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const voice = await canvas.findByRole('button', { name: /^My voice/ });
    const track = voice.closest<HTMLElement>('[data-swipe-track]')!;
    const forget = within(track).getByRole('button', {
      name: 'Forget My voice',
    });
    await expect(forget.querySelector('svg')).not.toBeNull();
    await expect(forget).toHaveTextContent('Forget');
    await expect(voice).toHaveAttribute('aria-pressed', 'true');
    await expect(canvas.getByRole('button', { name: 'Test' })).toBeDisabled();
    await expect(canvas.queryByText(/^Voice:/)).not.toBeInTheDocument();
  },
};

async function expectInlineTestAction(input: HTMLElement, button: HTMLElement) {
  await input.ownerDocument.fonts.ready;
  const field = input.getBoundingClientRect();
  const action = button.getBoundingClientRect();
  await expect(action.left - field.right).toBeGreaterThanOrEqual(8);
  // The input and its icon-labelled action must share a face and type scale.
  await expect(action.top).toBeCloseTo(field.top, 0);
  await expect(action.height).toBeCloseTo(field.height, 0);
  await expect(action.bottom).toBeCloseTo(field.bottom, 0);
  await expect(getComputedStyle(input).fontSize).toBe(getComputedStyle(button).fontSize);
}

async function startTest(canvasElement: HTMLElement) {
  const canvas = within(canvasElement);
  const input = await canvas.findByRole('textbox', {
    name: 'Text to synthesize',
  });
  await userEvent.type(input, 'This is how my voice sounds in Vis.');
  const test = canvas.getByRole('button', { name: 'Test' });
  await expectInlineTestAction(input, test);
  await userEvent.click(test);
  return { canvas, input };
}

export const TestVoice: Story = {
  args: { hasVoice: true },
  play: async ({ canvasElement }) => {
    const { canvas, input } = await startTest(canvasElement);
    await expect(canvas.getByRole('status')).toHaveTextContent('Synthesizing');
    // Pointer release runs before click; Stop must not turn into a submit button between them.
    const stop = canvas.getByRole('button', { name: 'Stop test' });
    await expectInlineTestAction(input, stop);
    const box = stop.getBoundingClientRect();
    const coords = {
      clientX: box.x + box.width / 2,
      clientY: box.y + box.height / 2,
    };
    await userEvent.pointer([
      { target: stop, coords, keys: '[MouseLeft>]' },
      { target: stop, coords, keys: '[/MouseLeft]' },
    ]);
    await expect(canvas.getByRole('button', { name: 'Test' })).toBeEnabled();
    await expect(input).toHaveValue('This is how my voice sounds in Vis.');
  },
};

export const Synthesizing: Story = {
  args: { hasVoice: true },
  play: async ({ canvasElement }) => {
    const { canvas } = await startTest(canvasElement);
    await expect(canvas.getByRole('status')).toHaveTextContent('Synthesizing');
  },
};

export const TestFailure: Story = {
  args: { hasVoice: true, failsToSpeak: true },
  play: async ({ canvasElement }) => {
    const { canvas, input } = await startTest(canvasElement);
    await expect(await canvas.findByText(/Could not synthesize speech/)).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Test' })).toBeEnabled();
    await expect(input).toHaveValue('This is how my voice sounds in Vis.');
  },
};

async function importFile(canvasElement: HTMLElement) {
  const canvas = within(canvasElement);
  await canvas.findByRole('button', { name: 'Import voice' });
  await userEvent.upload(
    canvas.getByLabelText('Recording to import as a voice'),
    new File(['voice fixture'], 'My voice.wav', { type: 'audio/wav' }),
  );
  await expect(canvas.getByRole('textbox', { name: 'Name' })).toHaveValue('My voice');
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
    await userEvent.click(canvas.getByRole('button', { name: 'Save voice' }));
    await expect(await canvas.findByText('My voice can speak on this machine now.')).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Record your voice' })).toBeVisible();
  },
};

export const SaveFailure: Story = {
  args: { failsToSave: true },
  play: async ({ canvasElement }) => {
    const canvas = await importFile(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Save voice' }));
    await expect(await canvas.findByText(/Could not save this voice/)).toBeVisible();
    await expect(canvas.getByRole('textbox', { name: 'Name' })).toHaveValue('My voice');
    await expect(canvas.getByRole('button', { name: 'Save voice' })).toBeEnabled();
  },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';
import { LiveVoicePanelContent } from './LiveVoicePanel';
import type { LiveVoiceView } from '../lib/live-voice';

const listening: LiveVoiceView = {
  phase: 'live',
  muted: false,
  outputMuted: false,
  transcript: 'Please check the tests.',
  final: false,
  state: null,
  turnId: 'voice-turn',
  message: null,
  reconciled: false,
  receipts: [],
};
const meta = {
  title: 'Session/Hands-free voice',
  component: LiveVoicePanelContent,
  args: {
    view: null,
    disabled: false,
    reason: null,
    onStart: fn(),
    onMute: fn(),
    onOutputMute: fn(),
    onStopSpeech: fn(),
    onCancelTask: fn(),
    onEnd: fn(),
  },
} satisfies Meta<typeof LiveVoicePanelContent>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Ready: Story = {};
export const Listening: Story = {
  args: { view: listening },
  play: async ({ canvas, args }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Stop speech' }));
    await expect(args.onStopSpeech).toHaveBeenCalled();
    await expect(args.onCancelTask).not.toHaveBeenCalled();
    await userEvent.click(canvas.getByRole('button', { name: 'Mute microphone' }));
    await expect(args.onMute).toHaveBeenCalledWith(true);
    await expect(canvas.getByLabelText('Provisional transcript')).toBeVisible();
  },
};
export const Muted: Story = { args: { view: { ...listening, muted: true, outputMuted: true } } };
export const Disconnected: Story = {
  args: {
    view: {
      ...listening,
      phase: 'error',
      message: 'Voice disconnected. Start again to reconnect.',
    },
  },
};
export const AndroidVerification: Story = {
  args: { mobile: true, allowMobileTest: false, onAllowMobileTest: fn() },
  play: async ({ canvas, args }) => {
    await expect(canvas.getByRole('button', { name: 'Start voice' })).toBeDisabled();
    await userEvent.click(canvas.getByRole('checkbox', { name: 'Enable mobile audio testing' }));
    await expect(args.onAllowMobileTest).toHaveBeenCalledWith(true);
    await expect(args.onStart).not.toHaveBeenCalled();
  },
};
export const AndroidReady: Story = {
  args: { mobile: true, allowMobileTest: true, onAllowMobileTest: fn() },
};
export const IOSListening: Story = {
  args: { view: listening },
};
export const MobileUnsupported: Story = {
  args: { reason: 'Platform echo cancellation is unavailable. Use dictation or text.' },
};

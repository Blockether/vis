import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';
import { LiveVoicePanelContent } from './LiveVoicePanel';
import type { LiveVoiceView } from '../lib/live-voice';
import type { LiveVoiceState } from '../lib/live-voice-protocol';

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

const state: LiveVoiceState = {
  voice_session_id: 'v',
  capture_generation: 1,
  output_epoch: 1,
  capture: 'live',
  utterance: 'idle',
  agent: 'running',
  output: 'playing',
};
const speaking: LiveVoiceView = { ...listening, final: true, state };

export const Ready: Story = {};
export const Connecting: Story = { args: { connecting: true } };
export const Listening: Story = { args: { view: listening } };
export const Speaking: Story = {
  args: { view: speaking },
  play: async ({ canvas, args }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Stop speech' }));
    await expect(args.onStopSpeech).toHaveBeenCalled();
    await expect(args.onCancelTask).not.toHaveBeenCalled();
    await userEvent.click(canvas.getByRole('button', { name: 'Mute microphone' }));
    await expect(args.onMute).toHaveBeenCalledWith(true);
    await expect(canvas.getByLabelText('Final transcript')).toBeVisible();
  },
};
export const Muted: Story = { args: { view: { ...listening, muted: true, outputMuted: true } } };
export const Waiting: Story = {
  args: {
    view: {
      ...speaking,
      state: { ...state, agent: 'waiting_for_input', output: 'idle' },
    },
  },
};
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

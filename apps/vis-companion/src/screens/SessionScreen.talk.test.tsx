// @vitest-environment jsdom
import { screen } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { renderSessionScreen } from './session-screen-harness';

vi.mock('../lib/live-voice-audio', () => ({
  liveVoiceDeviceReason: () => null,
  liveVoiceNeedsVerification: () => false,
  createLiveVoiceAudio: vi.fn(),
}));

const voiceCapabilities = {
  version: 1,
  features: {
    voice: { enabled: true, model: { status: 'ready' } },
    attachments: { max_files: 8, media_types: ['image/*'] },
  },
};

// The `Talk to Vis` App Intent (the Action Button) opens a session with a Talk nonce.
describe('a session opened by a Talk link', () => {
  it('requests hands-free voice instead of arming the old conversation mode', async () => {
    renderSessionScreen({
      client: {
        cachedCapabilities: () => voiceCapabilities,
        capabilities: () => Promise.resolve(voiceCapabilities),
        liveVoiceCapabilities: async () => ({ enabled: false, available: false }),
      },
      talk: '1500',
    });

    expect(await screen.findByText('Hands-free voice is unavailable on this gateway.')).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /start voice utterance/i })).not.toBeInTheDocument();
  });

  it('stays in dictation without a Talk link', async () => {
    renderSessionScreen({
      client: {
        cachedCapabilities: () => voiceCapabilities,
        capabilities: () => Promise.resolve(voiceCapabilities),
      },
    });

    expect(await screen.findByRole('button', { name: /dictate message/i })).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /start voice utterance/i })).not.toBeInTheDocument();
  });
});

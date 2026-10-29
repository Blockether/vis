// @vitest-environment jsdom
import { screen } from '@testing-library/react';
import { describe, expect, it } from 'vitest';
import { renderSessionScreen } from './session-screen-harness';

const voiceCapabilities = {
  version: 1,
  features: {
    voice: { enabled: true, model: { status: 'ready' } },
    attachments: { max_files: 8, media_types: ['image/*'] },
  },
};

// The `Talk to Vis` App Intent (the Action Button) opens a session with a Talk nonce.
describe('a session opened by a Talk link', () => {
  it('arms voice conversation and asks for the tap that starts recording', async () => {
    renderSessionScreen({
      client: {
        cachedCapabilities: () => voiceCapabilities,
        capabilities: () => Promise.resolve(voiceCapabilities),
      },
      talk: '1500',
    });

    expect(await screen.findByRole('button', { name: /start voice utterance/i })).toBeInTheDocument();
  });
});

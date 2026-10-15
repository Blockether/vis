// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { act, fireEvent, screen, waitFor } from '@testing-library/react';

const voice = vi.hoisted(() => ({
  stop: vi.fn<() => Promise<Blob>>(),
  cancel: vi.fn<() => Promise<void>>(),
}));

vi.mock('../lib/voice', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../lib/voice')>()),
  beginVoiceAudioSession: vi.fn(() => Promise.resolve()),
  endVoiceAudioSession: vi.fn(() => Promise.resolve()),
  startWavRecording: vi.fn(() =>
    Promise.resolve({ stop: voice.stop, cancel: voice.cancel, isCapturing: () => true }),
  ),
}));

vi.mock('../lib/pending-voice', () => ({
  clearPendingVoice: vi.fn(() => Promise.resolve()),
  readPendingVoice: vi.fn(() => Promise.resolve(null)),
  savePendingVoice: vi.fn(() => Promise.resolve()),
}));

import { renderSessionScreen } from './session-screen-harness';

const capabilities = {
  version: 1,
  features: {
    voice: { enabled: true, model: { status: 'ready' } },
    attachments: { max_files: 8, media_types: ['image/*'] },
  },
};

async function startDictation() {
  const transcribeVoice = vi.fn(() => Promise.resolve({ text: 'Spoken words' }));
  renderSessionScreen({
    client: {
      cachedCapabilities: () => capabilities,
      capabilities: () => Promise.resolve(capabilities),
      voiceModel: () => Promise.resolve({ status: 'ready' }),
      transcribeVoice,
    },
  });
  fireEvent.click(await screen.findByRole('button', { name: /^Dictate message/ }));
  return { transcribeVoice, mic: await screen.findByRole('button', { name: 'Finish dictation' }) };
}

const editor = () => screen.getByLabelText('Message Vis') as HTMLTextAreaElement;

// Reported: a long press on the microphone during dictation switched to voice
// conversation, and the dictated words were lost. Any press during a recording
// finishes it and keeps the mode.
describe('a press on the microphone during dictation', () => {
  beforeEach(() => {
    voice.stop.mockReset().mockResolvedValue(new Blob([new Uint8Array(44)], { type: 'audio/wav' }));
    voice.cancel.mockReset().mockResolvedValue(undefined);
  });

  it('finishes the dictation on a long press and does not switch the mode', async () => {
    const { transcribeVoice, mic } = await startDictation();

    fireEvent.pointerDown(mic, { button: 0 });
    await act(() => new Promise((resolve) => setTimeout(resolve, 600)));
    fireEvent.pointerUp(mic, { button: 0 });
    fireEvent.click(mic);

    await waitFor(() => expect(transcribeVoice).toHaveBeenCalledTimes(1));
    await waitFor(() => expect(editor().value).toBe('Spoken words'));
    expect(voice.cancel).not.toHaveBeenCalled();
    expect(screen.getByRole('button', { name: /^Dictate message/ })).toBeInTheDocument();
  });

  it('finishes the dictation on a context menu press and ignores its trailing click', async () => {
    const { transcribeVoice, mic } = await startDictation();

    fireEvent.contextMenu(mic);
    fireEvent.click(mic);

    await waitFor(() => expect(editor().value).toBe('Spoken words'));
    expect(transcribeVoice).toHaveBeenCalledTimes(1);
    expect(voice.cancel).not.toHaveBeenCalled();
    expect(screen.getByRole('button', { name: /^Dictate message/ })).toBeInTheDocument();
  });
});

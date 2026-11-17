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

vi.mock('../lib/live-voice-audio', () => ({
  liveVoiceDeviceReason: () => null,
  liveVoiceNeedsVerification: () => false,
  createLiveVoiceAudio: vi.fn(),
}));

import { renderSessionScreen } from './session-screen-harness';
import { startWavRecording } from '../lib/voice';

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

    fireEvent(mic, new MouseEvent('pointerdown', { button: 0, bubbles: true }));
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

function deferred<T>() {
  let resolve!: (value: T) => void;
  let reject!: (reason: Error) => void;
  const promise = new Promise<T>((done, fail) => {
    resolve = done;
    reject = fail;
  });
  return { promise, resolve, reject };
}

// Regression: pending dictation can release WebKit audio after native voice starts.
describe('dictation acquisition ownership', () => {
  beforeEach(() => {
    vi.mocked(startWavRecording).mockClear();
  });

  it('ignores a late microphone failure after navigation', async () => {
    const acquisition = deferred<Awaited<ReturnType<typeof startWavRecording>>>();
    vi.mocked(startWavRecording).mockReturnValueOnce(acquisition.promise);
    const view = renderSessionScreen({ client: {
      cachedCapabilities: () => capabilities,
      capabilities: async () => capabilities,
    } });
    fireEvent.click(await screen.findByRole('button', { name: /Dictate message/ }));
    view.rerenderSession('s2');
    await act(async () => acquisition.reject(new Error('Old microphone failed')));
    expect(screen.queryByText('Old microphone failed')).not.toBeInTheDocument();
    expect(screen.queryByText('Starting microphone…')).not.toBeInTheDocument();
    expect(screen.getByRole('button', { name: /Dictate message/ })).toBeInTheDocument();
  });

  it('waits for pending recorder cancellation before requesting hands-free', async () => {
    const acquisition = deferred<Awaited<ReturnType<typeof startWavRecording>>>();
    const cleanup = deferred<void>();
    const cancel = vi.fn(() => cleanup.promise);
    vi.mocked(startWavRecording).mockReturnValueOnce(acquisition.promise);
    const liveVoiceCapabilities = vi.fn(async () => ({ enabled: false, available: false }));
    renderSessionScreen({ client: {
      cachedCapabilities: () => capabilities,
      capabilities: async () => capabilities,
      liveVoiceCapabilities,
    } });
    const mic = await screen.findByRole('button', { name: /Dictate message/ });
    fireEvent.change(editor(), { target: { value: '' } });
    fireEvent.click(mic);
    fireEvent.contextMenu(mic);
    await act(async () => {});
    expect(liveVoiceCapabilities).not.toHaveBeenCalled();
    await act(async () => acquisition.resolve({ stop: voice.stop, cancel, isCapturing: () => true }));
    await waitFor(() => expect(cancel).toHaveBeenCalledTimes(1));
    expect(liveVoiceCapabilities).not.toHaveBeenCalled();
    await act(async () => cleanup.resolve());
    await waitFor(() => expect(liveVoiceCapabilities).toHaveBeenCalledTimes(1));
    fireEvent.click(mic);
    expect(startWavRecording).toHaveBeenCalledTimes(1);
    expect(screen.queryByRole('button', { name: 'Finish dictation' })).not.toBeInTheDocument();
  });

  it('waits for a pending model lookup and recorder release before native startup', async () => {
    const lookup = deferred<{ status: string }>();
    const cleanup = deferred<void>();
    const modelCapabilities = {
      ...capabilities,
      features: { ...capabilities.features, voice: { enabled: true, model: { status: 'absent' } } },
    };
    const cancel = vi.fn(() => cleanup.promise);
    vi.mocked(startWavRecording).mockResolvedValueOnce({ stop: voice.stop, cancel, isCapturing: () => true });
    const voiceModel = vi.fn(() => lookup.promise);
    const liveVoiceCapabilities = vi.fn(async () => ({ enabled: false, available: false }));
    renderSessionScreen({ client: {
      cachedCapabilities: () => modelCapabilities,
      capabilities: async () => modelCapabilities,
      voiceModel,
      liveVoiceCapabilities,
    } });
    const mic = await screen.findByRole('button', { name: /Dictate message/ });
    fireEvent.change(editor(), { target: { value: '' } });
    fireEvent.click(mic);
    await waitFor(() => expect(voiceModel).toHaveBeenCalledWith({ start: true, engine: null }));
    expect(screen.getByText('Starting microphone…')).toBeInTheDocument();
    fireEvent.contextMenu(mic);
    await act(async () => {});
    expect(liveVoiceCapabilities).not.toHaveBeenCalled();
    await act(async () => lookup.resolve({ status: 'ready' }));
    await waitFor(() => expect(cancel).toHaveBeenCalledTimes(1));
    expect(liveVoiceCapabilities).not.toHaveBeenCalled();
    await act(async () => cleanup.resolve());
    await waitFor(() => expect(liveVoiceCapabilities).toHaveBeenCalledTimes(1));
    expect(screen.queryByRole('button', { name: 'Finish dictation' })).not.toBeInTheDocument();
  });

  it('waits across unmount for old acquisition and cleanup before a Talk link starts native voice', async () => {
    const acquisition = deferred<Awaited<ReturnType<typeof startWavRecording>>>();
    const cleanup = deferred<void>();
    const cancel = vi.fn(() => cleanup.promise);
    vi.mocked(startWavRecording).mockReturnValueOnce(acquisition.promise);
    const client = {
      cachedCapabilities: () => capabilities,
      capabilities: async () => capabilities,
    };
    const previous = renderSessionScreen({ client });
    fireEvent.click(await screen.findByRole('button', { name: /Dictate message/ }));
    previous.unmount();
    const liveVoiceCapabilities = vi.fn(async () => ({ enabled: false, available: false }));
    renderSessionScreen({ client: { ...client, liveVoiceCapabilities }, talk: 'new-screen' });
    await act(async () => {});
    expect(liveVoiceCapabilities).not.toHaveBeenCalled();
    await act(async () => acquisition.resolve({ stop: voice.stop, cancel, isCapturing: () => true }));
    await waitFor(() => expect(cancel).toHaveBeenCalledTimes(1));
    expect(liveVoiceCapabilities).not.toHaveBeenCalled();
    await act(async () => cleanup.resolve());
    await waitFor(() => expect(liveVoiceCapabilities).toHaveBeenCalledTimes(1));
    expect(screen.queryByRole('button', { name: 'Finish dictation' })).not.toBeInTheDocument();
  });

  it('cancels late acquisition after navigation without publishing recording state', async () => {
    const acquisition = deferred<Awaited<ReturnType<typeof startWavRecording>>>();
    const cancel = vi.fn(async () => {});
    vi.mocked(startWavRecording).mockReturnValueOnce(acquisition.promise);
    const view = renderSessionScreen({ client: {
      cachedCapabilities: () => capabilities,
      capabilities: async () => capabilities,
    } });
    fireEvent.click(await screen.findByRole('button', { name: /Dictate message/ }));
    view.rerenderSession('s2');
    await act(async () => acquisition.resolve({ stop: voice.stop, cancel, isCapturing: () => true }));
    await waitFor(() => expect(cancel).toHaveBeenCalledTimes(1));
    expect(screen.queryByRole('button', { name: 'Finish dictation' })).not.toBeInTheDocument();
  });
});

// Regression: a hold replaces the old tap-to-talk mode without starting dictation.
describe('hands-free microphone gesture', () => {
  it('starts once after a hold and ignores the trailing click', async () => {
    const liveVoiceCapabilities = vi.fn(async () => ({ enabled: false, available: false }));
    renderSessionScreen({ client: {
      cachedCapabilities: () => capabilities,
      capabilities: async () => capabilities,
      liveVoiceCapabilities,
    } });
    const mic = await screen.findByRole('button', { name: /Dictate message/ });
    fireEvent.change(editor(), { target: { value: '' } });
    expect(screen.queryByRole('region', { name: 'Hands-free voice' })).not.toBeInTheDocument();
    fireEvent(mic, new MouseEvent('pointerdown', { button: 0, bubbles: true }));
    await act(() => new Promise((resolve) => setTimeout(resolve, 600)));
    fireEvent.pointerUp(mic, { button: 0 });
    fireEvent.click(mic);
    await waitFor(() => expect(liveVoiceCapabilities).toHaveBeenCalledTimes(1));
    expect(screen.queryByRole('button', { name: 'Finish dictation' })).not.toBeInTheDocument();
    expect(screen.queryByRole('button', { name: /Start voice utterance/ })).not.toBeInTheDocument();
  });
  it('does not start voice after a cancelled hold', async () => {
    const liveVoiceCapabilities = vi.fn();
    renderSessionScreen({ client: {
      cachedCapabilities: () => capabilities,
      capabilities: async () => capabilities,
      liveVoiceCapabilities,
    } });
    const mic = await screen.findByRole('button', { name: /Dictate message/ });
    fireEvent.change(editor(), { target: { value: '' } });
    fireEvent(mic, new MouseEvent('pointerdown', { button: 0, bubbles: true }));
    fireEvent.pointerCancel(mic);
    await act(() => new Promise((resolve) => setTimeout(resolve, 600)));
    expect(liveVoiceCapabilities).not.toHaveBeenCalled();
  });
});

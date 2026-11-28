import { describe, expect, it, vi } from 'vitest';
import { createDictationNotice } from './dictation-notice';
import type { NativeLiveVoicePlugin } from './live-voice-native-bridge';

function harness() {
  let control: ((event: { control: string }) => void) | null = null;
  const bridge = {
    setDictation: vi.fn(async () => undefined),
    addListener: vi.fn(async (_name: string, listener: (event: { control: string }) => void) => {
      control = listener;
      return { remove: async () => undefined };
    }),
  };
  const show = createDictationNotice(bridge as unknown as NativeLiveVoicePlugin);
  return { bridge, show, press: (name: string) => control?.({ control: name }) };
}

describe('dictation notice', () => {
  it('shows each phase once and Stop finishes the dictation', () => {
    const h = harness();
    const screen = {};
    const stop = vi.fn();
    h.show(screen, 'idle');
    h.show(screen, 'recording', stop);
    h.show(screen, 'recording', stop);
    h.press('stop_recording');
    h.press('end');
    h.show(screen, 'transcribing', stop);
    h.show(screen, 'idle');
    expect(stop).toHaveBeenCalledTimes(1);
    expect(h.bridge.setDictation.mock.calls).toEqual([
      [{ state: 'recording' }],
      [{ state: 'transcribing' }],
      [{ state: 'idle' }],
    ]);
  });

  it('shows the transcription progress and keeps the confirmation after idle', () => {
    const h = harness();
    const screen = {};
    h.show(screen, 'recording', vi.fn());
    h.show(screen, 'transcribing', undefined, { detail: 'Sending recording · 40%', progress: 40 });
    h.show(screen, 'transcribing', undefined, { detail: 'Sending recording · 40%', progress: 40 });
    h.show(screen, 'transcribing', undefined, { detail: 'Transcribing · 90%', progress: 90 });
    h.show(screen, 'transcribed');
    h.show(screen, 'idle');
    expect(h.bridge.setDictation.mock.calls).toEqual([
      [{ state: 'recording' }],
      [{ state: 'transcribing', detail: 'Sending recording · 40%', progress: 40 }],
      [{ state: 'transcribing', detail: 'Transcribing · 90%', progress: 90 }],
      [{ state: 'transcribed' }],
    ]);
  });

  it('keeps the dictation of one screen when another screen is idle', () => {
    const h = harness();
    const stop = vi.fn();
    h.show({}, 'recording', stop);
    h.show({}, 'idle');
    h.press('stop_recording');
    expect(stop).toHaveBeenCalledTimes(1);
    expect(h.bridge.setDictation.mock.calls).toEqual([[{ state: 'recording' }]]);
  });
});

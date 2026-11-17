import { registerPlugin } from '@capacitor/core';
import type { PluginListenerHandle } from '@capacitor/core';

/** Local PCM bridge. It does not perform speech recognition, TTS or network calls. */
export type NativeVoiceCapabilities = {
  available: boolean;
  echoCancellation: boolean;
  platform: 'ios' | 'android';
  requiresVerification: true;
  reason?: string;
};

export type NativeVoicePrepare = {
  sessionId: string;
  sampleRate: number;
  frameSamples: number;
  inputBufferFrames: number;
  outputBufferSamples: number;
};
export type NativeVoiceCapture = {
  sessionId: string;
  captureId: number;
  sequence: number;
  pcm: string;
};
export type NativeVoicePlayed = {
  sessionId: string;
  outputId: number;
  frameId: number;
  playedSamples: number;
};
export type NativeVoiceFailure = { sessionId: string; message: string };

/** Generations reject stale calls. PCM is mono signed 16-bit little-endian, encoded as base64. */
export interface NativeLiveVoicePlugin {
  getCapabilities(): Promise<NativeVoiceCapabilities>;
  prepare(options: NativeVoicePrepare): Promise<NativeVoiceCapabilities>;
  startCapture(options: { sessionId: string; captureId: number }): Promise<void>;
  stopCapture(options: { sessionId: string; captureId: number }): Promise<void>;
  acknowledgeCapture(options: {
    sessionId: string;
    captureId: number;
    sequence: number;
  }): Promise<void>;
  play(options: {
    sessionId: string;
    outputId: number;
    frameId: number;
    pcm: string;
  }): Promise<void>;
  stopOutput(options: { sessionId: string; outputId: number }): Promise<void>;
  close(options: { sessionId: string }): Promise<void>;
  addListener(
    event: 'capture',
    listener: (frame: NativeVoiceCapture) => void,
  ): Promise<PluginListenerHandle>;
  addListener(
    event: 'played',
    listener: (frame: NativeVoicePlayed) => void,
  ): Promise<PluginListenerHandle>;
  addListener(
    event: 'failure',
    listener: (event: NativeVoiceFailure) => void,
  ): Promise<PluginListenerHandle>;
}

export const nativeLiveVoice = registerPlugin<NativeLiveVoicePlugin>('LiveVoice');

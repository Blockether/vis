import type { PluginListenerHandle } from '@capacitor/core';
import type { LiveVoiceAudio } from './live-voice-audio';
import { LIVE_VOICE_LIMITS as L } from './live-voice-protocol';
import { speechOutput } from './speech';
import { nativeLiveVoice } from './live-voice-native-bridge';
import type {
  NativeLiveVoicePlugin,
  NativeVoiceCapabilities,
  NativeVoiceCapture,
  NativeVoicePlayed,
} from './live-voice-native-bridge';

const INPUT_FRAMES = Math.floor(L.input_buffer_ms / L.frame_ms);
const OUTPUT_SAMPLES = Math.floor((L.sample_rate * L.output_credit_ms) / 1000);
const MAX_PENDING_CALLS = INPUT_FRAMES + Math.ceil(OUTPUT_SAMPLES / L.frame_samples) + 8;

function encodePcm(samples: Int16Array): string {
  const bytes = new Uint8Array(samples.length * 2);
  const view = new DataView(bytes.buffer);
  samples.forEach((value, index) => view.setInt16(index * 2, value, true));
  return btoa(String.fromCharCode(...bytes));
}

function decodePcm(pcm: string): Int16Array {
  if (typeof pcm !== 'string' || pcm.length !== Math.ceil((L.frame_samples * 2) / 3) * 4)
    throw new Error('The native microphone frame is invalid.');
  const bytes = atob(pcm);
  if (bytes.length !== L.frame_samples * 2)
    throw new Error('The native microphone frame is invalid.');
  const samples = new Int16Array(L.frame_samples);
  for (let index = 0; index < samples.length; index++)
    samples[index] = bytes.charCodeAt(index * 2) | (bytes.charCodeAt(index * 2 + 1) << 8);
  return samples;
}

function checkCapabilities(value: NativeVoiceCapabilities): void {
  if (!value.available || !value.echoCancellation || value.requiresVerification !== true)
    throw new Error(value.reason || 'Platform echo cancellation is unavailable. Use dictation or text.');
}

type PendingFrame = {
  pcm: string | null;
  count: number;
  played: number;
  onPlayed: (count: number) => void;
};

/**
 * Mobile pilot. Enabled AEC is not proof of acoustic performance.
 * The native bridge owns the foreground rule: it starts only in the foreground
 * and continues behind a locked screen. Page visibility is not a stop signal.
 */
export class NativeLiveVoiceAudio implements LiveVoiceAudio {
  readonly captureRestartsOutput = true;
  private readonly sessionId = crypto.randomUUID();
  private readonly bridge: NativeLiveVoicePlugin;
  private readonly allowed: boolean;
  private closed = false;
  private preparing = false;
  private ready = false;
  private onFailure: ((message: string) => void) | null = null;
  private onFrame: ((samples: Int16Array) => void) | null = null;
  private captureId = 0;
  private captureStarting = false;
  private captureWatchdog: ReturnType<typeof setTimeout> | null = null;
  private outputId = 0;
  private sequence = 0;
  private frameId = 0;
  private pendingCalls = 0;
  private playing = new Map<number, PendingFrame>();
  private listeners: PluginListenerHandle[] = [];
  private releaseOutput: (() => void) | null = null;

  constructor(allowUnverifiedMobileAudio = false, bridge: NativeLiveVoicePlugin = nativeLiveVoice) {
    this.allowed = allowUnverifiedMobileAudio;
    this.bridge = bridge;
  }

  async prepare(onFailure: (message: string) => void): Promise<void> {
    if (!this.allowed) throw new Error('Enable mobile audio testing before starting voice.');
    if (this.closed || this.preparing) throw new Error('Voice audio is closed or already prepared.');
    this.preparing = true;
    this.onFailure = onFailure;
    try {
      checkCapabilities(await this.bridge.getCapabilities());
      if (this.closed) throw new Error('Voice start was cancelled.');
      this.releaseOutput = speechOutput.reserveLiveOutput();
      const listeners = [
        this.bridge.addListener('capture', (frame) => this.captured(frame)),
        this.bridge.addListener('played', (frame) => this.played(frame)),
        this.bridge.addListener('failure', (event) => {
          if (event.sessionId === this.sessionId) this.fail(event.message);
        }),
      ];
      await Promise.all(
        listeners.map(async (pending) => {
          const listener = await pending;
          if (this.closed) await listener.remove();
          else this.listeners.push(listener);
        }),
      );
      if (this.closed) throw new Error('Voice start was cancelled.');
      const capabilities = await this.bridge.prepare({
        sessionId: this.sessionId,
        sampleRate: L.sample_rate,
        frameSamples: L.frame_samples,
        inputBufferFrames: INPUT_FRAMES,
        outputBufferSamples: OUTPUT_SAMPLES,
      });
      if (this.closed) {
        void this.bridge.close({ sessionId: this.sessionId }).catch(() => undefined);
        throw new Error('Voice start was cancelled.');
      }
      checkCapabilities(capabilities);
      this.ready = true;
    } catch (error) {
      this.close();
      throw error;
    }
  }

  async capture(onFrame: (samples: Int16Array) => void): Promise<void> {
    if (!this.ready || this.closed) throw new Error('Voice audio is closed.');
    this.stopCapture();
    const captureId = ++this.captureId;
    this.sequence = 0;
    this.onFrame = onFrame;
    this.captureStarting = true;
    await this.command(
      () => this.bridge.startCapture({ sessionId: this.sessionId, captureId }),
      () => captureId === this.captureId,
    );
    if (!this.closed && captureId === this.captureId) {
      this.captureStarting = false;
      // Startup can restart the native graph. Submit output only after it settles.
      for (const [frameId, frame] of this.playing) this.submit(frameId, frame);
      this.watchCapture();
    }
  }

  private watchCapture(): void {
    if (this.captureWatchdog) clearTimeout(this.captureWatchdog);
    this.captureWatchdog = setTimeout(() => {
      this.captureWatchdog = null;
      if (!this.closed && this.onFrame)
        this.fail('Microphone stopped delivering audio. Start voice again.');
    }, 3000);
  }

  private captured(frame: NativeVoiceCapture): void {
    if (
      this.closed ||
      frame.sessionId !== this.sessionId ||
      frame.captureId !== this.captureId ||
      !this.onFrame
    )
      return;
    try {
      if (!Number.isSafeInteger(frame.sequence) || frame.sequence !== this.sequence)
        throw new Error('Native microphone frames arrived out of order.');
      const samples = decodePcm(frame.pcm);
      this.sequence++;
      this.watchCapture();
      const captureId = this.captureId;
      void this.command(
        () => this.bridge.acknowledgeCapture({
          sessionId: this.sessionId, captureId, sequence: frame.sequence,
        }),
        () => captureId === this.captureId,
      );
      this.onFrame?.(samples);
    } catch (error) {
      this.fail(error instanceof Error ? error.message : 'Native microphone input failed.');
    }
  }

  stopCapture(): void {
    if (this.closed || !this.onFrame) return;
    this.onFrame = null;
    this.captureStarting = false;
    for (const [frameId, frame] of this.playing) {
      if (frame.pcm !== null) this.playing.delete(frameId);
    }
    if (this.captureWatchdog) clearTimeout(this.captureWatchdog);
    this.captureWatchdog = null;
    const captureId = ++this.captureId;
    void this.command(
      () => this.bridge.stopCapture({ sessionId: this.sessionId, captureId }),
      () => captureId === this.captureId,
    );
  }

  play(samples: Int16Array, onPlayed: (count: number) => void): void {
    if (!this.ready || this.closed) throw new Error('Voice output is unavailable.');
    if (!samples.length || samples.length > L.frame_samples)
      throw new Error('The native output frame is invalid.');
    const pending = [...this.playing.values()].reduce(
      (total, frame) => total + frame.count - frame.played, 0,
    );
    if (pending + samples.length > OUTPUT_SAMPLES)
      throw new Error('Voice output exceeded its credit.');
    const frameId = this.frameId++;
    const frame = { pcm: encodePcm(samples), count: samples.length, played: 0, onPlayed };
    this.playing.set(frameId, frame);
    if (!this.captureStarting) this.submit(frameId, frame);
  }

  private submit(frameId: number, frame: PendingFrame): void {
    if (frame.pcm === null) return;
    const pcm = frame.pcm;
    frame.pcm = null;
    const outputId = this.outputId;
    void this.command(
      () => this.bridge.play({ sessionId: this.sessionId, outputId, frameId, pcm }),
      () => outputId === this.outputId,
    );
  }

  private played(event: NativeVoicePlayed): void {
    if (this.closed || event.sessionId !== this.sessionId || event.outputId !== this.outputId) return;
    const frame = this.playing.get(event.frameId);
    if (!frame || frame.pcm !== null) return;
    if (
      !Number.isSafeInteger(event.playedSamples) ||
      event.playedSamples < frame.played ||
      event.playedSamples > frame.count
    ) {
      this.fail('Native playback reported an invalid position.');
      return;
    }
    if (event.playedSamples === frame.played) return;
    frame.played = event.playedSamples;
    if (frame.played === frame.count) this.playing.delete(event.frameId);
    frame.onPlayed(frame.played);
  }

  stopOutput(): void {
    if (this.closed) return;
    this.playing.clear();
    const outputId = ++this.outputId;
    if (!this.ready) return;
    void this.command(
      () => this.bridge.stopOutput({ sessionId: this.sessionId, outputId }),
      () => outputId === this.outputId,
    );
  }

  private async command(run: () => Promise<void>, current: () => boolean): Promise<void> {
    if (this.closed) return;
    if (this.pendingCalls >= MAX_PENDING_CALLS) {
      this.fail('The native audio bridge exceeded its buffer limit.');
      return;
    }
    this.pendingCalls++;
    try {
      await run();
    } catch (error) {
      if (!this.closed && current())
        this.fail(error instanceof Error ? error.message : 'Native audio failed.');
    } finally {
      this.pendingCalls--;
    }
  }

  private fail(message: string): void {
    if (this.closed) return;
    const notify = this.onFailure;
    this.close();
    notify?.(message);
  }

  close(): void {
    if (this.closed) return;
    this.closed = true;
    this.ready = false;
    if (this.captureWatchdog) clearTimeout(this.captureWatchdog);
    this.captureWatchdog = null;
    this.onFrame = null;
    this.onFailure = null;
    this.playing.clear();
    this.captureId++;
    this.outputId++;
    for (const listener of this.listeners) void listener.remove().catch(() => undefined);
    this.listeners = [];
    this.releaseOutput?.();
    this.releaseOutput = null;
    void this.bridge.close({ sessionId: this.sessionId }).catch(() => undefined);
  }
}

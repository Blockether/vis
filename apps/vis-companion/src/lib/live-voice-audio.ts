import { Capacitor } from '@capacitor/core';
import { speechOutput } from './speech';
import { NativeLiveVoiceAudio } from './live-voice-native-audio';
import captureWorkletUrl from './live-voice-worklet?worker&url';
import { LIVE_VOICE_LIMITS as L } from './live-voice-protocol';

/** A control from outside the app, such as a lock screen. */
export const LIVE_VOICE_CONTROLS = ['mute', 'unmute', 'stop_speech', 'end'] as const;
export type LiveVoiceControl = (typeof LIVE_VOICE_CONTROLS)[number];
/** What a surface outside the app shows. It holds no transcript and no session name. */
export type LiveVoiceStatus = {
  state: 'connecting' | 'listening' | 'speaking' | 'muted' | 'waiting';
  muted: boolean;
  speaking: boolean;
  task: 'running' | 'queued' | 'cancelling' | null;
};

export interface LiveVoiceAudio {
  readonly captureRestartsOutput?: boolean;
  readonly outputReferenceStartMs?: number;
  /** Receive controls from outside the app. Absent where the platform has none. */
  remote?(onControl: (control: LiveVoiceControl) => void): void;
  /** Show the state outside the app. */
  present?(status: LiveVoiceStatus): void;
  prepare(onFailure: (message: string) => void): Promise<void>;
  capture(onFrame: (samples: Int16Array) => void): Promise<void>;
  stopCapture(): void;
  play(samples: Int16Array, onPlayed: (count: number) => void): void;
  stopOutput(): void;
  close(): void;
}
export function liveVoiceNeedsVerification(): boolean {
  return Capacitor.isNativePlatform();
}

export function createLiveVoiceAudio(allowUnverifiedMobileAudio = false): LiveVoiceAudio {
  return Capacitor.isNativePlatform()
    ? new NativeLiveVoiceAudio(allowUnverifiedMobileAudio)
    : new BrowserLiveVoiceAudio();
}

export function liveVoiceDeviceReason(): string | null {
  if (Capacitor.isNativePlatform()) {
    if (!['ios', 'android'].includes(Capacitor.getPlatform()))
      return 'This native platform does not support hands-free voice.';
    return Capacitor.isPluginAvailable('LiveVoice')
      ? null
      : 'Install an app build with native hands-free audio support.';
  }
  if (
    /Android|iPhone|iPad|iPod/i.test(navigator.userAgent) ||
    (/Mac/.test(navigator.userAgent) && navigator.maxTouchPoints > 1)
  ) {
    return 'Use the iOS or Android app to test mobile hands-free voice.';
  }
  if (
    !globalThis.isSecureContext ||
    !navigator.mediaDevices?.getUserMedia ||
    typeof AudioContext === 'undefined' ||
    typeof AudioWorkletNode === 'undefined'
  ) {
    return 'Hands-free voice needs a secure desktop browser with AudioWorklet support.';
  }
  return null;
}

type PlayingFrame = {
  node: AudioBufferSourceNode;
  start: number;
  count: number;
  reported: number;
  onPlayed: (count: number) => void;
};

/** Local media only. This class has no device TTS or network fallback. */
export class BrowserLiveVoiceAudio implements LiveVoiceAudio {
  private context: AudioContext | null = null;
  private contextOriginMs = 0;
  private releaseOutput: (() => void) | null = null;
  private onFailure: ((message: string) => void) | null = null;
  private stream: MediaStream | null = null;
  private source: MediaStreamAudioSourceNode | null = null;
  private captureNode: AudioWorkletNode | null = null;
  private captureRun = 0;
  private playing: PlayingFrame[] = [];
  private nextStart = 0;
  private outputRun = 0;
  private timer: ReturnType<typeof setInterval> | null = null;
  private disposeListeners: (() => void) | null = null;

  async prepare(onFailure: (message: string) => void): Promise<void> {
    if (Capacitor.isNativePlatform()) throw new Error('Use native audio in the mobile app.');
    const reason = liveVoiceDeviceReason();
    if (reason) throw new Error(reason);
    this.releaseOutput = speechOutput.reserveLiveOutput();
    const context = new AudioContext({ latencyHint: 'interactive' });
    this.context = context;
    this.onFailure = onFailure;
    const lost = () => onFailure('Voice stopped because the audio device or window changed.');
    const hidden = () => {
      if (document.hidden) lost();
    };
    const stateChanged = () => {
      if (context.state !== 'running' && this.context === context) lost();
    };
    window.addEventListener('blur', lost);
    document.addEventListener('visibilitychange', hidden);
    navigator.mediaDevices.addEventListener('devicechange', lost);
    this.disposeListeners = () => {
      window.removeEventListener('blur', lost);
      document.removeEventListener('visibilitychange', hidden);
      navigator.mediaDevices.removeEventListener('devicechange', lost);
      context.removeEventListener('statechange', stateChanged);
    };
    await context.resume();
    this.contextOriginMs = performance.now() - context.currentTime * 1000;
    await context.audioWorklet.addModule(captureWorkletUrl);
    if (this.context !== context) throw new Error('Voice start was cancelled.');
    if (context.state !== 'running') throw new Error('The audio context is not running.');
    context.addEventListener('statechange', stateChanged);
  }

  async capture(onFrame: (samples: Int16Array) => void): Promise<void> {
    this.stopCapture();
    const run = this.captureRun;
    const context = this.context;
    if (!context) throw new Error('Voice audio is closed.');
    const stream = await navigator.mediaDevices.getUserMedia({
      audio: {
        channelCount: 1,
        echoCancellation: true,
        noiseSuppression: true,
        autoGainControl: true,
      },
    });
    if (run !== this.captureRun || context !== this.context) {
      stream.getTracks().forEach((track) => track.stop());
      return;
    }
    const tracks = stream.getTracks();
    let echoCancellation = false;
    try {
      echoCancellation =
        tracks.length > 0 &&
        tracks.every((track) => track.getSettings?.().echoCancellation === true);
    } catch {
      /* Missing settings are not proof of echo cancellation. */
    }
    if (!echoCancellation) {
      tracks.forEach((track) => track.stop());
      throw new Error(
        'Hands-free voice requires verified echo cancellation. Use dictation or text instead.',
      );
    }
    // A true setting confirms the capability, not acoustic performance.
    this.stream = stream;
    stream.getTracks().forEach((track) =>
      track.addEventListener(
        'ended',
        () => {
          if (this.stream === stream) this.onFailure?.('The microphone disconnected.');
        },
        { once: true },
      ),
    );
    const node = new AudioWorkletNode(context, 'vis-live-voice-capture', {
      numberOfInputs: 1,
      numberOfOutputs: 1,
      outputChannelCount: [1],
    });
    this.captureNode = node;
    node.onprocessorerror = () => {
      if (run !== this.captureRun || this.captureNode !== node) return;
      this.onFailure?.('The microphone processor stopped.');
    };
    node.port.onmessage = (event: MessageEvent<Int16Array | { error: string }>) => {
      if (run !== this.captureRun || this.captureNode !== node) return;
      if (!(event.data instanceof Int16Array)) {
        this.onFailure?.(event.data.error);
        return;
      }
      node.port.postMessage({ type: 'ack' });
      onFrame(event.data);
    };
    this.source = context.createMediaStreamSource(stream);
    this.source.connect(node);
    node.connect(context.destination);
  }

  stopCapture(): void {
    this.captureRun++;
    if (this.captureNode) {
      this.captureNode.onprocessorerror = null;
      this.captureNode.port.onmessage = null;
      this.captureNode.port.postMessage({ type: 'stop' });
      this.captureNode.port.close();
      this.captureNode.disconnect();
      this.captureNode = null;
    }
    this.source?.disconnect();
    this.source = null;
    const stream = this.stream;
    this.stream = null;
    stream?.getTracks().forEach((track) => track.stop());
  }

  get outputReferenceStartMs(): number | undefined {
    const context = this.context;
    if (!context) return undefined;
    return this.contextOriginMs + 1000 * (
      Math.max(this.nextStart, context.currentTime + 0.12) +
      (context.baseLatency || 0) + (context.outputLatency || 0)
    );
  }

  play(samples: Int16Array, onPlayed: (count: number) => void): void {
    const context = this.context;
    if (!context || context.state !== 'running') throw new Error('Voice output is unavailable.');
    if (
      this.playing.reduce((total, frame) => total + frame.count - frame.reported, samples.length) >
      (L.sample_rate * L.output_credit_ms) / 1000
    )
      throw new Error('Voice output exceeded its credit.');
    const buffer = context.createBuffer(1, samples.length, L.sample_rate);
    const channel = buffer.getChannelData(0);
    for (let i = 0; i < samples.length; i++) channel[i] = samples[i] / 32768;
    const node = context.createBufferSource();
    node.buffer = buffer;
    node.connect(context.destination);
    const start = Math.max(this.nextStart, context.currentTime + 0.12);
    this.nextStart = start + samples.length / L.sample_rate;
    this.playing.push({
      node,
      start,
      count: samples.length,
      reported: 0,
      onPlayed,
    });
    node.start(start);
    if (!this.timer) this.timer = setInterval(() => this.reportPlayed(), 40);
  }

  private reportPlayed(): void {
    const context = this.context;
    const run = this.outputRun;
    if (!context || context.state !== 'running') return;
    // This estimates rendered samples minus latency. It cannot prove that a person heard them.
    const cursor =
      context.currentTime - (context.baseLatency || 0) - (context.outputLatency || 0) - 0.04;
    const remaining: PlayingFrame[] = [];
    for (const frame of this.playing) {
      const count = Math.max(
        0,
        Math.min(frame.count, Math.floor((cursor - frame.start) * L.sample_rate)),
      );
      if (count > frame.reported) {
        frame.reported = count;
        frame.onPlayed(count);
      }
      if (this.context !== context || run !== this.outputRun) return;
      if (count === frame.count) frame.node.disconnect();
      else remaining.push(frame);
    }
    this.playing = remaining;
    if (!remaining.length && this.timer) {
      clearInterval(this.timer);
      this.timer = null;
    }
  }

  stopOutput(): void {
    this.reportPlayed();
    this.outputRun++;
    for (const frame of this.playing) {
      frame.node.stop();
      frame.node.disconnect();
    }
    this.playing = [];
    this.nextStart = 0;
    if (this.timer) clearInterval(this.timer);
    this.timer = null;
  }

  close(): void {
    this.stopCapture();
    this.stopOutput();
    this.disposeListeners?.();
    this.disposeListeners = null;
    const context = this.context;
    this.context = null;
    this.onFailure = null;
    this.releaseOutput?.();
    this.releaseOutput = null;
    if (context && context.state !== 'closed') void context.close().catch(() => undefined);
  }
}

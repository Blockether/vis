import { Capacitor, registerPlugin } from "@capacitor/core";
import type { VoiceProgress } from "./types";

interface AndroidAudioRoutePlugin {
  startBluetoothMicrophone(): Promise<{ connected: boolean }>;
  stopBluetoothMicrophone(): Promise<void>;
}

const androidAudioRoute = registerPlugin<AndroidAudioRoutePlugin>("AudioRoute");

async function claimAndroidBluetoothMicrophone(): Promise<boolean> {
  if (Capacitor.getPlatform() !== "android") return false;
  try {
    await androidAudioRoute.startBluetoothMicrophone();
    return true;
  } catch {
    // Recording from the device microphone is still useful when no headset mic
    // is connected or Android refuses the communication route.
    return false;
  }
}

export interface WavRecording {
  stop: () => Promise<Blob>;
  cancel: () => Promise<void>;
  /**
   * Is the microphone STILL feeding this recording? False once the OS suspended
   * the audio context or released the track. Backgrounding no longer ends a
   * dictation on its own (see the audio-session note below), so the app needs a
   * way to ask, on return, whether capture actually survived the trip.
   */
  isCapturing: () => boolean;
}

export interface WavRecordingOptions {
  /**
   * Fired ONCE when the OS really takes the microphone away mid-recording: a
   * call arrives, another app claims the audio session, or the recording hits
   * its cap. Merely leaving the foreground is NOT one of these: the iOS target
   * declares the `audio` background mode (`ios/App/App/Info.plist`) AND every
   * dictation claims a `play-and-record` audio session (see below). Those two
   * TOGETHER are what keeps WKWebView's capture — and the graph that drains it —
   * alive while the app is backgrounded or the screen is locked. Whatever was
   * said up to the interruption is still buffered; the caller decides what to do
   * with it, but it must be told, because nothing else here ever fires again.
   */
  onInterrupted?: (reason: string) => void;
}

// A dictation nobody ends still buffers audio, and on iOS the OS answers a
// webview that grows without bound by killing it — the app simply vanishes.
// Cap the recording and end it like any other interruption instead.
const MAX_RECORDING_SECONDS = 900;

// Speech transcription runs at 16 kHz, while the capture device hands us 44.1 or
// 48 kHz. Downsampling AT CAPTURE is what makes a 15-minute dictation affordable
// on a phone: 48 kHz Int16 pins ~5.5 MB per minute (~83 MB at the cap, doubled
// for a moment while the WAV is encoded), 16 kHz pins ~1.9 MB. Nothing is lost —
// the transcription API resamples to 16 kHz regardless.

// The frames are buffered as Int16 — the WAV payload format itself. Holding the
// raw Float32 frames instead doubled the memory a long dictation pins, for a
// conversion that has to happen anyway.
const TARGET_SAMPLE_RATE = 16000;

function encodePcmWav(chunks: Int16Array[], sampleRate: number): Blob {
  const sampleCount = chunks.reduce((total, chunk) => total + chunk.length, 0);
  const buffer = new ArrayBuffer(44 + sampleCount * 2);
  const view = new DataView(buffer);
  const write = (offset: number, value: string) => {
    for (let index = 0; index < value.length; index += 1) {
      view.setUint8(offset + index, value.charCodeAt(index));
    }
  };

  write(0, 'RIFF');
  view.setUint32(4, 36 + sampleCount * 2, true);
  write(8, 'WAVE');
  write(12, 'fmt ');
  view.setUint32(16, 16, true);
  view.setUint16(20, 1, true);
  view.setUint16(22, 1, true);
  view.setUint32(24, sampleRate, true);
  view.setUint32(28, sampleRate * 2, true);
  view.setUint16(32, 2, true);
  view.setUint16(34, 16, true);
  write(36, 'data');
  view.setUint32(40, sampleCount * 2, true);

  let offset = 44;
  for (const chunk of chunks) {
    for (const sample of chunk) {
      view.setInt16(offset, sample, true);
      offset += 2;
    }
  }
  return new Blob([buffer], { type: 'audio/wav' });
}

// WHY A DICTATION USED TO DIE THE MOMENT THE APP LEFT THE FOREGROUND.
//
// `UIBackgroundModes = audio` (ios/App/App/Info.plist) is necessary but NOT
// sufficient. It only stops WebKit from MUTING the capture track (WebKit bug
// 226620: muting happens "in case UIBackgroundModes does not contain audio").
// The Web Audio graph that DRAINS that track is governed separately, and WebKit
// registers WebAudio with `BackgroundProcessPlaybackRestricted`
// (WebCore/platform/audio/ios/MediaSessionManagerIOS.mm). So backgrounding the
// app interrupts the AudioContext: `state` leaves `running`, `onaudioprocess`
// stops firing, and the rest of the sentence is lost while the track is still
// live and the UI still says "Listening…".
//
// The documented escape is in
// AudioContext::shouldOverrideBackgroundPlaybackRestriction
// (WebCore/Modules/webaudio/AudioContext.cpp): the restriction is overridden
// when the DOCUMENT declares an audio session of type `playback` or
// `play-and-record`. That is the Audio Session API — `navigator.audioSession`,
// WebKit-only, iOS 16.4+, which is exactly the platform with the problem. No
// native plugin and no extra Info.plist key can substitute for it: the decision
// is taken inside the web content process, from the document's own session type.
//
// A dictation therefore claims `play-and-record` for its lifetime and hands the
// session back as `auto` when it ends, so ordinary playback is not left on a
// recording route. Where the API is absent (every non-WebKit browser) this is a
// no-op and capture behaves exactly as before.
type AudioSessionType = 'auto' | 'playback' | 'transient' | 'transient-solo' | 'ambient' | 'play-and-record';

function claimAudioSession(type: AudioSessionType): boolean {
  const session = (navigator as Navigator & { audioSession?: { type: AudioSessionType } }).audioSession;
  if (!session) return false;
  try {
    session.type = type;
    return true;
  } catch {
    // A user agent that exposes the object but rejects the value is not a reason
    // to refuse the dictation — it only loses the background guarantee.
    return false;
  }
}

export async function startWavRecording(
  options: WavRecordingOptions = {},
): Promise<WavRecording> {
  if (!navigator.mediaDevices?.getUserMedia) {
    throw new Error('Microphone recording is unavailable on this device');
  }

  // Claimed BEFORE capture starts, inside the tap's own gesture window, so the
  // session type is already in force when the app is backgrounded a second later.
  const heldAudioSession = claimAudioSession('play-and-record');
  const releaseAudioSession = () => {
    if (heldAudioSession) claimAudioSession('auto');
  };

  // A2DP is playback-only. Android WebView does not promote a connected headset
  // to its HFP/SCO microphone for getUserMedia, so establish that native route
  // before WebView chooses an input device.
  const heldAndroidRoute = await claimAndroidBluetoothMicrophone();
  const releaseAndroidRoute = async () => {
    if (heldAndroidRoute) {
      await androidAudioRoute.stopBluetoothMicrophone().catch(() => undefined);
    }
  };

  let stream: MediaStream;
  try {
    stream = await navigator.mediaDevices.getUserMedia({
      audio: {
        channelCount: 1,
        echoCancellation: true,
        noiseSuppression: true,
        autoGainControl: true,
      },
    });
  } catch (cause) {
    await releaseAndroidRoute();
    releaseAudioSession();
    throw cause;
  }
  const context = new AudioContext({ latencyHint: 'interactive' });
  const source = context.createMediaStreamSource(stream);
  const processor = context.createScriptProcessor(4096, 1, 1);
  const silentOutput = context.createGain();
  const chunks: Int16Array[] = [];
  // Ratio is >= 1; a device already at or below 16 kHz passes through untouched.
  const ratio = Math.max(1, context.sampleRate / TARGET_SAMPLE_RATE);
  const outputRate = context.sampleRate / ratio;
  const limitSamples = MAX_RECORDING_SECONDS * outputRate;
  // Box-filter resampler state, carried ACROSS frames on purpose: a fractional
  // ratio (44100/16000 = 2.756…) never lines up with a 4096-sample frame, so
  // restarting the accumulator per frame would click every 85 ms.
  let acc = 0;
  let accCount = 0;
  let pending = 0;
  let sampleCount = 0;
  let peak = 0;
  let closed = false;
  // Late-bound on purpose: frames start arriving during the `resume()` await
  // below, before `interrupt` exists, and the callback must never reach into a
  // binding that is still in its temporal dead zone.
  let onLimit: ((reason: string) => void) | null = null;

  silentOutput.gain.value = 0;
  processor.onaudioprocess = (event) => {
    if (closed) return;
    const input = event.inputBuffer.getChannelData(0);
    const frame = new Int16Array(Math.ceil(input.length / ratio) + 1);
    let written = 0;
    for (let index = 0; index < input.length; index += 1) {
      const value = Math.max(-1, Math.min(1, input[index]));
      const level = Math.abs(value);
      if (level > peak) peak = level;
      acc += value;
      accCount += 1;
      pending += 1;
      if (pending >= ratio) {
        const mean = acc / accCount;
        frame[written] = mean < 0 ? mean * 0x8000 : mean * 0x7fff;
        written += 1;
        acc = 0;
        accCount = 0;
        pending -= ratio;
      }
    }
    if (!written) return;
    chunks.push(written === frame.length ? frame : frame.subarray(0, written));
    sampleCount += written;
    if (sampleCount >= limitSamples) {
      onLimit?.(`Dictation stopped at the ${Math.round(MAX_RECORDING_SECONDS / 60)}-minute limit — transcribing what was said.`);
    }
  };
  source.connect(processor);
  processor.connect(silentOutput);
  silentOutput.connect(context.destination);
  await context.resume();
  // A context that stays suspended captures nothing: iOS/WKWebView parks it when
  // resume() lands outside the tap's gesture window, or when another app owns the
  // audio session. Fail loudly here instead of shipping a silent WAV.
  if (context.state !== 'running') {
    for (const track of stream.getTracks()) track.stop();
    await context.close();
    await releaseAndroidRoute();
    releaseAudioSession();
    throw new Error('Microphone could not start — tap the mic again');
  }

  // Interruption is SILENT: the context is suspended, or the track goes muted
  // when another app grabs the mic. No error, no event on the recorder — capture
  // just stops while the UI still says "Listening…". These are the only signals
  // that it happened. With the `audio` background mode AND the `play-and-record`
  // session in place they no longer fire merely because the app left the
  // foreground.
  let interrupted = false;
  const interrupt = (reason: string) => {
    if (interrupted || closed) return;
    interrupted = true;
    options.onInterrupted?.(reason);
  };
  context.onstatechange = () => {
    if (context.state !== 'running') interrupt('Dictation stopped — the microphone was suspended.');
  };
  for (const track of stream.getTracks()) {
    track.addEventListener('ended', () => interrupt('Dictation stopped — the microphone was released.'));
    track.addEventListener('mute', () => interrupt('Dictation stopped — another app took the microphone.'));
  }
  onLimit = interrupt;

  const close = async () => {
    if (closed) return;
    closed = true;
    processor.onaudioprocess = null;
    context.onstatechange = null;
    source.disconnect();
    processor.disconnect();
    silentOutput.disconnect();
    for (const track of stream.getTracks()) track.stop();
    await context.close();
    await releaseAndroidRoute();
    releaseAudioSession();
  };

  return {
    stop: async () => {
      await close();
      if (!chunks.length) throw new Error('No audio was recorded');
      // Digital silence means the track was live but muted (another app holds the
      // mic, or the OS denied it after the fact). Transcribing it returns an empty
      // string the composer cannot explain, so name the cause here.
      if (peak < 1e-4) throw new Error('Microphone captured only silence — check that nothing else is using it');
      return encodePcmWav(chunks, outputRate);
    },
    cancel: close,
    isCapturing: () => !closed && !interrupted && context.state === 'running',
  };
}

/**
 * What the composer says while a dictation is in flight.
 *
 * "Transcribing on your machine…" was the only sentence the app had, from the
 * moment the microphone stopped until the words appeared: a slow upload on a
 * train, a voice model still downloading and a wedged engine all looked exactly
 * the same. Each phase now names itself, and the ones that can be measured carry
 * their percentage.
 */
export function voiceProgressLabel(progress: VoiceProgress | null): string {
  if (!progress) return "Sending recording…";
  const percent = Math.max(
    0,
    Math.min(100, Math.round(progress.progress || 0)),
  );
  switch (progress.phase) {
    case "uploading":
      return `Sending recording · ${percent}%`;
    case "queued":
      return "Recording received · waiting for the engine";
    case "preparing":
      return percent > 0
        ? `Preparing voice engine · ${percent}%`
        : "Preparing voice engine…";
    case "transcribing":
      return `Transcribing · ${percent}%`;
    case "done":
      return "Transcribing · 100%";
    case "failed":
      return "Transcription failed";
    default:
      return "Transcribing…";
  }
}

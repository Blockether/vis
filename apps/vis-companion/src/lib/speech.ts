import { Capacitor, registerPlugin } from "@capacitor/core";
import { getSpeechPrefs } from "./storage";
import { wavePeaks } from "./waveform";
import type { SpeechPrefs } from "./types";

interface NativeSpeechPlugin {
  speak(options: { text: string; voice?: string; rate?: number }): Promise<void>;
  stop(): Promise<void>;
  /** Every voice this phone can expose to applications, with the OS quality verdict. */
  getVoices(): Promise<{
    voices: {
      id: string;
      label?: string;
      language?: string;
      is_default?: boolean;
      quality?: number;
      is_network_required?: boolean;
    }[];
  }>;
}

/** ONE registration of the native plugin; `speech-voices.ts` asks it for the list. */
export const nativeSpeech = registerPlugin<NativeSpeechPlugin>("NativeSpeech");

/** Both mobile platforms expose a fuller, deterministic catalogue through the app bridge. */
export function usesNativeSpeech(): boolean {
  const platform = Capacitor.getPlatform();
  return platform === "android" || platform === "ios";
}

/** Device-local speech output: native TTS on iOS and Android, Web Speech elsewhere. */
class DeviceSpeechOutput {
  private active: object | null = null;

  speak(text: string, voiceId: string | null, rate: number): Promise<void> {
    this.stop();
    if (usesNativeSpeech()) {
      const active = {};
      this.active = active;
      return nativeSpeech
        .speak({ text, voice: voiceId ?? undefined, rate })
        .finally(() => {
          if (this.active === active) this.active = null;
        });
    }
    const synthesis =
      typeof window === "undefined" ? undefined : window.speechSynthesis;
    if (!synthesis || typeof SpeechSynthesisUtterance === "undefined") {
      return Promise.reject(
        new Error("Text-to-speech is unavailable on this device."),
      );
    }
    return new Promise((resolve, reject) => {
      const utterance = new SpeechSynthesisUtterance(text);
      utterance.rate = rate;
      // A voice the device no longer has is not an error: it was chosen on this
      // device and an OS update may have taken it away, so the reply is still
      // spoken - in whatever the engine calls its default.
      const chosen = voiceId
        ? synthesis
            .getVoices?.()
            ?.find((voice) => voice.voiceURI === voiceId || voice.name === voiceId)
        : undefined;
      if (chosen) utterance.voice = chosen;
      this.active = utterance;
      utterance.onend = () => {
        if (this.active === utterance) this.active = null;
        resolve();
      };
      utterance.onerror = (event) => {
        if (this.active === utterance) this.active = null;
        if (event.error === "canceled" || event.error === "interrupted") resolve();
        else reject(new Error(`Text-to-speech failed: ${event.error}`));
      };
      synthesis.speak(utterance);
    });
  }

  stop(): void {
    if (usesNativeSpeech()) {
      void nativeSpeech.stop().catch(() => undefined);
    } else if (typeof window !== "undefined" && window.speechSynthesis) {
      window.speechSynthesis.cancel();
    }
    this.active = null;
  }
}

/**
 * What speaks a line ON THE MACHINE, registered by the screen that knows which
 * gateway and which session the reply came from. `null` whenever no session is open,
 * which is exactly why a machine engine is a preference and not a promise: the router
 * falls back to this device rather than dropping the reply.
 */
export interface GatewaySpeaker {
  speak(text: string, voiceId: string | null, engineId: string): Promise<Blob>;
}

/**
 * WHAT THE SCREEN LEARNS WHILE THE MACHINE SPEAKS.
 *
 * The device route can say nothing at all - native TTS and Web Speech hand out no
 * buffer - so this is optional on purpose: a listener that hears nothing keeps its
 * own estimate, and the block draws no shape it cannot prove.
 */
export interface SpokenTrack {
  /** Real peaks of the audio that is playing, loudest bar normalised to 1. */
  peaks: number[];
  /** Its real length in seconds, measured by the player, not counted from words. */
  duration: number;
}

export interface SpeechListener {
  onTrack?(track: SpokenTrack): void;
  onProgress?(seconds: number): void;
}

/** Enough bars for a phone-width rail, and cheap to read from a reply-sized WAV. */
const WAVE_BARS = 240;
/**
 * Which TTS engine speaks a reply.
 *
 * `speak(text)` stays unchanged for transcript call sites. Settings stores one engine
 * choice: `null` is this device's system TTS, while an id asks the active machine for
 * that exact registered engine. If that machine or engine cannot speak, this device
 * says the line instead so a routing failure never loses the reply itself.
 */
class SpeechOutput {
  private readonly device = new DeviceSpeechOutput();
  private gateway: GatewaySpeaker | null = null;
  private notice: ((message: string) => void) | null = null;
  private prefs: SpeechPrefs | null = null;
  private playing: {
    element: HTMLAudioElement;
    interrupt: () => void;
  } | null = null;
  private activeRun: { owner: object | null } | null = null;

  /** The screen with a live session hands the machine's voice in, and takes it back. */
  setGateway(
    speaker: GatewaySpeaker | null,
    onNotice?: ((message: string) => void) | null,
  ): void {
    this.gateway = speaker;
    this.notice = onNotice ?? null;
  }

  /** Settings applies what it just saved, so the next reply obeys it without a reload. */
  apply(prefs: SpeechPrefs): void {
    this.prefs = prefs;
  }

  /** The stored choice, read once per app run. */
  async settings(): Promise<SpeechPrefs> {
    if (!this.prefs) this.prefs = await getSpeechPrefs();
    return this.prefs;
  }

  async speak(
    text: string,
    listener?: SpeechListener,
    owner?: object,
  ): Promise<void> {
    // Starting a line is an explicit global handoff. Its owner only scopes later
    // cleanup: a transcript block disappearing may stop its own replay, never a
    // voice-mode line that happened to start on the same singleton afterward.
    this.stop();
    const run = { owner: owner ?? null };
    this.activeRun = run;
    try {
      const prefs = await this.settings();
      if (this.activeRun !== run) return;
      if (prefs.ttsEngine && this.gateway) {
        try {
          const audio = await this.gateway.speak(
            text,
            prefs.gatewayVoice,
            prefs.ttsEngine,
          );
          if (this.activeRun !== run) return;
          await this.play(audio, listener);
          return;
        } catch (cause) {
          if (this.activeRun !== run) return;
          this.notice?.(
            `That machine could not speak (${(cause as Error).message}) - this device did.`,
          );
        }
      }
      if (this.activeRun !== run) return;
      await this.device.speak(text, prefs.deviceVoice, prefs.rate);
    } finally {
      if (this.activeRun === run) this.activeRun = null;
    }
  }

  /** Audition one exact system voice without changing the saved reply route. */
  async playDeviceSample(text: string, voiceId: string | null, rate: number): Promise<void> {
    this.stop();
    await this.device.speak(text, voiceId, rate);
  }

  /**
   * Play bytes the caller already holds: settings is auditioning a voice, not speaking a
   * reply, so no preference is read and the device never stands in — one press plays the
   * one voice that was pressed, and whatever was playing stops first. Replacing a sample is
   * an ordinary completion for its caller, never an audio failure.
   */
  async playSample(audio: Blob, listener?: SpeechListener): Promise<void> {
    this.stop();
    await this.play(audio, listener);
  }

  stop(owner?: object): void {
    if (owner !== undefined && this.activeRun?.owner !== owner) return;
    this.activeRun = null;
    const playing = this.playing;
    if (playing) {
      // Settle and detach the old element before changing its source. WebKit reports that
      // deliberate source change through `onerror`; to this API it is a successful handoff.
      playing.interrupt();
      playing.element.pause();
      playing.element.src = "";
    }
    this.device.stop();
  }

  /** Play the machine's audio, and let go of the object URL whatever happens. */
  private play(audio: Blob, listener?: SpeechListener): Promise<void> {
    if (typeof Audio === "undefined" || typeof URL.createObjectURL !== "function") {
      return Promise.reject(
        new Error("This device cannot play the audio the machine sent."),
      );
    }
    const url = URL.createObjectURL(audio);
    const element = new Audio(url);
    // The shape and the clock both come from THESE bytes: the peaks are read off the
    // audio that is about to play and the position is the player's own `currentTime`,
    // so nothing the reader sees is inferred from the length of the text.
    const measured = audio
      .arrayBuffer()
      .then((bytes) => wavePeaks(bytes, WAVE_BARS))
      .catch(() => [] as number[]);
    element.onloadedmetadata = () => {
      const duration = element.duration;
      if (!Number.isFinite(duration) || duration <= 0) return;
      void measured.then((peaks) => listener?.onTrack?.({ peaks, duration }));
    };
    element.ontimeupdate = () => listener?.onProgress?.(element.currentTime);
    return new Promise<void>((resolve, reject) => {
      let settled = false;
      const settle = (cause?: Error) => {
        if (settled) return;
        settled = true;
        element.onloadedmetadata = null;
        element.ontimeupdate = null;
        element.onended = null;
        element.onerror = null;
        if (this.playing?.element === element) this.playing = null;
        URL.revokeObjectURL(url);
        if (cause) reject(cause);
        else resolve();
      };
      const fail = (cause: unknown) =>
        settle(cause instanceof Error ? cause : new Error(String(cause)));
      this.playing = { element, interrupt: () => settle() };
      element.onended = () => settle();
      element.onerror = () =>
        fail(new Error("The audio the machine sent could not be played."));
      try {
        void Promise.resolve(element.play()).catch(fail);
      } catch (cause) {
        fail(cause);
      }
    });
  }
}

/** TTS is device-global, so every replay and automatic response shares it. */
export const speechOutput = new SpeechOutput();

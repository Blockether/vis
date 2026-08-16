import { Capacitor, registerPlugin } from "@capacitor/core";
import { getSpeechPrefs } from "./storage";
import type { SpeechPrefs } from "./types";

interface AndroidSpeechPlugin {
  speak(options: { text: string; voice?: string; rate?: number }): Promise<void>;
  stop(): Promise<void>;
  /** Every voice this phone's engine has installed, the current one marked. */
  getVoices(): Promise<{
    voices: {
      id: string;
      label?: string;
      language?: string;
      is_default?: boolean;
    }[];
  }>;
}

/** ONE registration of the native plugin; `speech-voices.ts` asks it for the list. */
export const androidSpeech = registerPlugin<AndroidSpeechPlugin>("NativeSpeech");

/** Device-local speech output: native Android TTS, Web Speech elsewhere. */
class DeviceSpeechOutput {
  private active: object | null = null;

  speak(text: string, voiceId: string | null, rate: number): Promise<void> {
    this.stop();
    if (Capacitor.getPlatform() === "android") {
      const active = {};
      this.active = active;
      return androidSpeech
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
    if (Capacitor.getPlatform() === "android") {
      void androidSpeech.stop().catch(() => undefined);
    }
    if (typeof window !== "undefined" && window.speechSynthesis) {
      window.speechSynthesis.cancel();
    }
    this.active = null;
  }
}

/**
 * What speaks a line ON THE MACHINE, registered by the screen that knows which
 * gateway and which session the reply came from. `null` whenever no session is open,
 * which is exactly why `gateway` is a preference and not a promise: the router falls
 * back to this device rather than dropping the reply.
 */
export interface GatewaySpeaker {
  speak(text: string, voiceId: string | null): Promise<Blob>;
}

/**
 * WHERE a reply is spoken: nowhere, on this device, or on the machine that answered.
 *
 * `speak(text)` is unchanged for every call site - the choice is a stored preference
 * and not an argument, because the reader makes it once in settings and the transcript
 * code has no opinion about audio. Two rules the routing keeps:
 *
 *   - `off` is SILENCE, not a failure: the answer is on screen and nothing is spoken.
 *   - a machine that cannot speak right now costs the reader nothing - this device
 *     says the line instead, and the reason is reported once through the notice the
 *     screen registered. Staying quiet because the chosen engine is unreachable would
 *     lose the reply itself, which is the one thing the reader asked for.
 */
class SpeechOutput {
  private readonly device = new DeviceSpeechOutput();
  private gateway: GatewaySpeaker | null = null;
  private notice: ((message: string) => void) | null = null;
  private prefs: SpeechPrefs | null = null;
  private playing: HTMLAudioElement | null = null;

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

  async speak(text: string): Promise<void> {
    const prefs = await this.settings();
    if (prefs.route === "gateway" && this.gateway) {
      try {
        const audio = await this.gateway.speak(text, prefs.gatewayVoice);
        await this.play(audio);
        return;
      } catch (cause) {
        this.notice?.(
          `That machine could not speak (${(cause as Error).message}) - this device did.`,
        );
      }
    }
    await this.device.speak(text, prefs.deviceVoice, prefs.rate);
  }

  stop(): void {
    const playing = this.playing;
    this.playing = null;
    if (playing) {
      playing.pause();
      playing.src = "";
    }
    this.device.stop();
  }

  /** Play the machine's audio, and let go of the object URL whatever happens. */
  private play(audio: Blob): Promise<void> {
    if (typeof Audio === "undefined" || typeof URL.createObjectURL !== "function") {
      return Promise.reject(
        new Error("This device cannot play the audio the machine sent."),
      );
    }
    const url = URL.createObjectURL(audio);
    const element = new Audio(url);
    this.playing = element;
    const done = () => {
      if (this.playing === element) this.playing = null;
      URL.revokeObjectURL(url);
    };
    return new Promise<void>((resolve, reject) => {
      element.onended = () => {
        done();
        resolve();
      };
      element.onerror = () => {
        done();
        reject(new Error("The audio the machine sent could not be played."));
      };
      void Promise.resolve(element.play()).catch((cause: unknown) => {
        done();
        reject(cause as Error);
      });
    });
  }
}

/** TTS is device-global, so every replay and automatic response shares it. */
export const speechOutput = new SpeechOutput();

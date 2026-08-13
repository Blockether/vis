import { Capacitor, registerPlugin } from "@capacitor/core";

interface AndroidSpeechPlugin {
  speak(options: { text: string }): Promise<void>;
  stop(): Promise<void>;
}

const androidSpeech = registerPlugin<AndroidSpeechPlugin>("NativeSpeech");

/** Device-local speech output: native Android TTS, Web Speech elsewhere. */
class DeviceSpeechOutput {
  private active: object | null = null;

  speak(text: string): Promise<void> {
    this.stop();
    if (Capacitor.getPlatform() === "android") {
      const active = {};
      this.active = active;
      return androidSpeech.speak({ text }).finally(() => {
        if (this.active === active) this.active = null;
      });
    }
    const synthesis = window.speechSynthesis;
    if (!synthesis || typeof SpeechSynthesisUtterance === "undefined") {
      return Promise.reject(new Error("Text-to-speech is unavailable on this device."));
    }
    return new Promise((resolve, reject) => {
      const utterance = new SpeechSynthesisUtterance(text);
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

/** TTS is device-global, so every replay and automatic response shares it. */
export const speechOutput = new DeviceSpeechOutput();

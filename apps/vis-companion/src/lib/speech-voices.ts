import { Capacitor } from "@capacitor/core";
import { androidSpeech } from "./speech";

/** ONE voice this device can speak in, in the shape the picker renders. */
export interface DeviceVoice {
  /** What `speech.ts` stores and speaks with: a `voiceURI` on the web, a name on Android. */
  id: string;
  label: string;
  language?: string;
  isDefault?: boolean;
}

/**
 * How long to wait for a web engine to admit it has voices. Chrome and WKWebView both
 * answer an EMPTY list on the first call and fire `voiceschanged` once the engine has
 * loaded them - so a picker that trusted the first answer showed "no voices" on the
 * very device that carries Siri.
 */
const VOICES_SETTLE_MS = 1500;

function fromWeb(synthesis: SpeechSynthesis): DeviceVoice[] {
  return synthesis.getVoices().map((voice) => ({
    id: voice.voiceURI || voice.name,
    label: voice.name,
    language: voice.lang || undefined,
    isDefault: voice.default,
  }));
}

/**
 * Every voice THIS DEVICE can speak in: the phone's own engine on Android, and the
 * Web Speech list everywhere else - which on iOS is the Siri voices already installed,
 * because the app runs in WKWebView.
 *
 * An empty list is an answer, not a failure: a Linux desktop with no speech engine
 * installed has no voices, and the picker says so instead of pretending.
 */
export async function deviceVoices(): Promise<DeviceVoice[]> {
  if (Capacitor.getPlatform() === "android") {
    const answer = await androidSpeech.getVoices();
    return (answer?.voices ?? []).map((voice) => ({
      id: voice.id,
      label: voice.label || voice.id,
      language: voice.language || undefined,
      isDefault: voice.is_default,
    }));
  }
  const synthesis =
    typeof window === "undefined" ? undefined : window.speechSynthesis;
  if (!synthesis?.getVoices) return [];
  const first = fromWeb(synthesis);
  if (first.length > 0) return first;
  return new Promise<DeviceVoice[]>((resolve) => {
    let settled = false;
    const finish = () => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      synthesis.removeEventListener?.("voiceschanged", finish);
      resolve(fromWeb(synthesis));
    };
    const timer = setTimeout(finish, VOICES_SETTLE_MS);
    synthesis.addEventListener?.("voiceschanged", finish);
  });
}

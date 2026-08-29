import { Capacitor } from "@capacitor/core";
import { nativeSpeech, usesNativeSpeech } from "./speech";

/** ONE voice this device can speak in, in the shape the picker renders. */
export interface DeviceVoice {
  /** What `speech.ts` stores and speaks with: an OS voice identifier or Web URI. */
  id: string;
  label: string;
  language?: string;
  isDefault?: boolean;
  /** The native OS's 100–500 quality verdict. Web voices use identifier markers. */
  quality?: number;
  /** `false` means the OS needs a network connection for this voice. */
  isLocal?: boolean;
}

/** Other platforms keep three ranked alternatives beside System default. */
export const BEST_DEVICE_VOICE_LIMIT = 3;

export const IOS_VOICE_DOWNLOAD_GUIDANCE =
  "In Settings, choose Accessibility → Read & Speak (or Spoken Content) → Voices. Download a Premium or Enhanced voice, then return here; this list refreshes automatically.";

/** Apple exposes no public API for an app to install its system voice assets. */
export function iosVoiceDownloadGuidance(
  platform: string = Capacitor.getPlatform(),
): string | null {
  return platform === "ios" ? IOS_VOICE_DOWNLOAD_GUIDANCE : null;
}

function normalizedLanguage(value: string | undefined): string {
  return (value ?? "").replaceAll("_", "-").toLowerCase();
}

function languageRank(voice: DeviceVoice, preferred: readonly string[]): number {
  const language = normalizedLanguage(voice.language);
  if (!language) return preferred.length * 2;
  const exact = preferred.indexOf(language);
  if (exact >= 0) return exact;
  const base = language.split("-")[0];
  const related = preferred.findIndex((one) => one.split("-")[0] === base);
  return related >= 0 ? preferred.length + related : preferred.length * 2 + 1;
}

function voiceQuality(voice: DeviceVoice): number {
  if (typeof voice.quality === "number") return voice.quality;
  const identity = `${voice.id} ${voice.label}`.toLowerCase();
  if (/\b(premium|neural|natural|siri)\b/.test(identity)) return 500;
  if (/\benhanced\b/.test(identity)) return 450;
  if (/\bcompact\b/.test(identity)) return 100;
  return 300;
}

function localRank(voice: DeviceVoice): number {
  return voice.isLocal === true ? 2 : voice.isLocal === undefined ? 1 : 0;
}

function preferredDeviceLanguages(): string[] {
  if (typeof navigator === "undefined") return [];
  return Array.from(new Set([...(navigator.languages ?? []), navigator.language]))
    .map(normalizedLanguage)
    .filter(Boolean);
}

function normalizedPreferredLanguages(values: readonly string[]): string[] {
  return Array.from(new Set(values.map(normalizedLanguage).filter(Boolean)));
}

function sortedDeviceVoices(
  voices: DeviceVoice[],
  preferred: readonly string[],
): DeviceVoice[] {
  return [...voices].sort((left, right) => {
    const byLanguage = languageRank(left, preferred) - languageRank(right, preferred);
    if (byLanguage !== 0) return byLanguage;
    const byQuality = voiceQuality(right) - voiceQuality(left);
    if (byQuality !== 0) return byQuality;
    const byLocal = localRank(right) - localRank(left);
    if (byLocal !== 0) return byLocal;
    if (!!left.isDefault !== !!right.isDefault) return left.isDefault ? -1 : 1;
    return left.label.localeCompare(right.label);
  });
}

function rankedDeviceVoices(
  voices: DeviceVoice[],
  selectedId: string | null,
  preferredLanguages: readonly string[],
): DeviceVoice[] {
  const preferred = normalizedPreferredLanguages(preferredLanguages);
  const matchesPreferred = (voice: DeviceVoice) => {
    const language = normalizedLanguage(voice.language);
    if (!language || preferred.length === 0) return true;
    const base = language.split("-")[0];
    return preferred.some(
      (one) => one === language || one.split("-")[0] === base,
    );
  };
  const matching = voices.filter(matchesPreferred);
  const pool = matching.some((voice) => voice.language) ? matching : voices;
  const ranked = sortedDeviceVoices(pool, preferred);
  const result = ranked.slice(0, BEST_DEVICE_VOICE_LIMIT);
  const selected = selectedId ? voices.find((voice) => voice.id === selectedId) : undefined;
  if (!selected || result.some((voice) => voice.id === selected.id)) return result;
  return [...result.slice(0, BEST_DEVICE_VOICE_LIMIT - 1), selected];
}

/**
 * iOS exposes every installed Premium and Enhanced voice as an explicit choice,
 * ranked with this device's languages first. Its automatic voice remains System default.
 * Other platforms keep a short list ranked by language, quality and availability.
 */
export function bestDeviceVoices(
  voices: DeviceVoice[],
  selectedId: string | null = null,
  preferredLanguages: readonly string[] = preferredDeviceLanguages(),
  platform: string = Capacitor.getPlatform(),
): DeviceVoice[] {
  const unique = Array.from(
    new Map(voices.filter((voice) => voice.id).map((voice) => [voice.id, voice])).values(),
  );
  if (platform !== "ios") {
    return rankedDeviceVoices(unique, selectedId, preferredLanguages);
  }

  // iOS already exposes each language's `isDefault` voice through the automatic
  // System default row. Listing that same identifier again creates two controls
  // which speak with the same voice.
  const explicit = unique.filter(
    (voice) => !voice.isDefault && voiceQuality(voice) >= 450,
  );
  return sortedDeviceVoices(
    explicit,
    normalizedPreferredLanguages(preferredLanguages),
  );
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
    isLocal: voice.localService,
  }));
}

/**
 * Every voice THIS DEVICE can speak in. Native iOS exposes Apple's public
 * `AVSpeechSynthesizer` catalogue (not Siri's private numbered voices), native Android
 * exposes the active TTS engine, and browsers use Web Speech.
 *
 * An empty list is an answer, not a failure: a Linux desktop with no speech engine
 * installed has no voices, and the picker says so instead of pretending.
 */
export async function deviceVoices(): Promise<DeviceVoice[]> {
  if (usesNativeSpeech()) {
    const answer = await nativeSpeech.getVoices();
    return (answer?.voices ?? []).map((voice) => ({
      id: voice.id,
      label: voice.label || voice.id,
      language: voice.language || undefined,
      isDefault: voice.is_default,
      quality: voice.quality,
      isLocal:
        voice.is_network_required === undefined
          ? undefined
          : !voice.is_network_required,
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

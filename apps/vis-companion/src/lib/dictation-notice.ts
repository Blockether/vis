import { Capacitor } from '@capacitor/core';
import { nativeLiveVoice } from './live-voice-native-bridge';
import type {
  NativeDictationProgress,
  NativeDictationState,
  NativeLiveVoicePlugin,
} from './live-voice-native-bridge';

/**
 * Shows a web dictation outside the app: the iOS lock screen and the Android notice. Their Stop
 * button finishes the dictation like the microphone button does. One screen owns the notice, so
 * an idle screen cannot remove the dictation of another screen. Transcribed ends the ownership:
 * the native side removes that notice itself, so a later idle does not cut the confirmation.
 */
export function createDictationNotice(bridge: NativeLiveVoicePlugin) {
  let owner: object | null = null;
  let shown = JSON.stringify({ state: 'idle' });
  let stop: (() => void) | null = null;
  let listening = false;

  return (
    who: object,
    state: NativeDictationState,
    onStop?: () => void,
    transcription?: NativeDictationProgress,
  ): void => {
    if (state === 'idle' || state === 'transcribed') {
      if (owner !== who) return;
      owner = null;
      stop = null;
    } else {
      owner = who;
      stop = onStop ?? null;
    }
    if (!listening && state !== 'idle') {
      listening = true;
      void bridge
        .addListener('dictation', (event) => {
          if (event.control === 'stop_recording') stop?.();
        })
        .catch(() => undefined);
    }
    const options = state === 'transcribing' && transcription ? { state, ...transcription } : { state };
    const key = JSON.stringify(options);
    if (key === shown) return;
    shown = key;
    // An app build without the method keeps dictation without the outside controls.
    void bridge.setDictation(options).catch(() => undefined);
  };
}

const nativeNotice = createDictationNotice(nativeLiveVoice);

export function showDictation(
  who: object,
  state: NativeDictationState,
  onStop?: () => void,
  transcription?: NativeDictationProgress,
): void {
  if (!Capacitor.isNativePlatform() || !Capacitor.isPluginAvailable('LiveVoice')) return;
  nativeNotice(who, state, onStop, transcription);
}

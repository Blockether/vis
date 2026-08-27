import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

const here = dirname(fileURLToPath(import.meta.url));
const prepare = readFileSync(join(here, 'ios-prepare.mjs'), 'utf8');
const between = (name) => prepare.split(`const ${name} = \``)[1]?.split('\n`;')[0] ?? '';
const speech = between('speechSource');

describe('iOS public speech bridge', () => {
  it('enumerates Apple public voices with their real quality tier', () => {
    expect(speech).toContain('@objc(NativeSpeechPlugin)');
    expect(speech).toContain('AVSpeechSynthesisVoice.speechVoices()');
    expect(speech).toContain('voice.identifier');
    expect(speech).toContain('voice.quality == .premium');
    expect(speech).toContain('voice.quality == .enhanced');
  });

  it('opens the system voice catalogue from the app', () => {
    expect(speech).toContain('CAPPluginMethod(name: "openVoiceSettings"');
    expect(speech).toContain('App-Prefs:root=ACCESSIBILITY&path=SPEECH_TITLE/QuickSpeakAccents');
    expect(speech).toContain('UIApplication.shared.open');
  });

  it('speaks the exact selected identifier and settles on finish or cancellation', () => {
    expect(speech).toContain('voice.identifier == requestedVoice');
    expect(speech).toContain('AVSpeechUtteranceDefaultSpeechRate');
    expect(speech).toContain('didFinish utterance: AVSpeechUtterance');
    expect(speech).toContain('didCancel utterance: AVSpeechUtterance');
  });

  it('compiles and registers the generated plugin in the app target', () => {
    expect(prepare).toContain('NativeSpeech.swift in Sources');
    expect(prepare).toContain("'NativeSpeechPlugin'");
    expect(prepare).toContain('speechOk');
  });
});

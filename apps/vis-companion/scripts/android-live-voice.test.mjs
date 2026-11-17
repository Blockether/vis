import { mkdtempSync, mkdirSync, cpSync, readFileSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { prepareAndroidLiveVoice } from '../native/android/prepare.mjs';

const root = join(dirname(fileURLToPath(import.meta.url)), '..');
describe('Android live voice preparation', () => {
  it.each([
    'registerPlugin(OAuthLoopbackPlugin.class);\nsuper.onCreate(savedInstanceState);',
    'public class MainActivity extends BridgeActivity {}',
  ])('copies sources and registers idempotently in %s', (initialActivity) => {
    const fixture = mkdtempSync(join(tmpdir(), 'vis-live-voice-'));
    try {
      cpSync(join(root, 'native'), join(fixture, 'native'), { recursive: true });
      const main = join(fixture, 'android/app/src/main');
      const java = join(main, 'java/com/blockether/viscompanion');
      mkdirSync(java, { recursive: true });
      const activity = join(java, 'MainActivity.java');
      writeFileSync(activity, initialActivity);
      writeFileSync(join(main, 'AndroidManifest.xml'), '<manifest>\n    <application>\n    </application>\n</manifest>');
      prepareAndroidLiveVoice(fixture, 'com.blockether.viscompanion');
      const once = readFileSync(activity, 'utf8');
      prepareAndroidLiveVoice(fixture, 'com.blockether.viscompanion');
      expect(readFileSync(activity, 'utf8')).toBe(once);
      if (initialActivity.includes('OAuthLoopback'))
        expect(once).toContain('registerPlugin(OAuthLoopbackPlugin.class);');
      expect(once.match(/registerPlugin\(LiveVoicePlugin.class\)/g)).toHaveLength(1);
      for (const file of ['LiveVoicePlugin.java', 'LiveVoiceService.java', 'LiveVoiceState.java'])
        expect(readFileSync(join(java, file), 'utf8')).toBe(readFileSync(join(root, 'native/android', file), 'utf8'));
      const manifest = readFileSync(join(main, 'AndroidManifest.xml'), 'utf8');
      expect(manifest.match(/android.permission.RECORD_AUDIO/g)).toHaveLength(1);
      expect(manifest.match(/android.permission.MODIFY_AUDIO_SETTINGS/g)).toHaveLength(1);
      // Without the service entry, Android stops the microphone behind a locked screen.
      for (const name of ['FOREGROUND_SERVICE', 'FOREGROUND_SERVICE_MICROPHONE', 'FOREGROUND_SERVICE_MEDIA_PLAYBACK', 'WAKE_LOCK'])
        expect(manifest.match(new RegExp(`android.permission.${name}"`, 'g'))).toHaveLength(1);
      expect(manifest.match(/<service android:name="\.LiveVoiceService"[^>]*android:foregroundServiceType="mediaPlayback\|microphone"/g)).toHaveLength(1);
      expect(manifest.indexOf('<service')).toBeLessThan(manifest.indexOf('</application>'));
    } finally { rmSync(fixture, { recursive: true, force: true }); }
  });
});

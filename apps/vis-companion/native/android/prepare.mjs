import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

/** Install the local PCM bridge without release credentials or version changes. */
export function prepareAndroidLiveVoice(root, appId) {
  if (!/^[a-zA-Z][\w]*(?:\.[a-zA-Z][\w]*)+$/.test(appId))
    throw new Error('Invalid Android package');
  const main = join(root, 'android/app/src/main');
  const java = join(main, 'java', ...appId.split('.'));
  const activityPath = join(java, 'MainActivity.java');
  let activity = readFileSync(activityPath, 'utf8');
  const registration = 'registerPlugin(LiveVoicePlugin.class);';
  if (!activity.includes(registration)) {
    const marker = 'super.onCreate(savedInstanceState);';
    if (!activity.includes(marker)) {
      const scaffold = 'public class MainActivity extends BridgeActivity {}';
      if (!activity.includes(scaffold)) throw new Error('Android activity has no registration seam');
      activity = activity.replace(scaffold, `public class MainActivity extends BridgeActivity {
    @Override public void onCreate(android.os.Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }
}`);
    }
    activity = activity.replace(marker, `${registration}\n        ${marker}`);
    writeFileSync(activityPath, activity);
  }
  for (const [directory, target, files] of [
    ['android', java, ['LiveVoicePlugin.java', 'LiveVoiceState.java']],
    ['android-test', join(root, 'android/app/src/test/java', ...appId.split('.')), ['LiveVoiceStateTest.java']],
    ['android-test', join(root, 'android/app/src/androidTest/java', ...appId.split('.')), ['LiveVoiceSmokeTest.java']],
  ]) {
    mkdirSync(target, { recursive: true });
    for (const file of files) {
      const source = readFileSync(join(root, 'native', directory, file), 'utf8')
        .replace('package com.blockether.viscompanion;', `package ${appId};`);
      writeFileSync(join(target, file), source);
    }
  }
  const manifestPath = join(main, 'AndroidManifest.xml');
  let manifest = readFileSync(manifestPath, 'utf8');
  for (const name of ['RECORD_AUDIO', 'MODIFY_AUDIO_SETTINGS']) {
    const permission = `android.permission.${name}`;
    if (!manifest.includes(`android:name="${permission}"`))
      manifest = manifest.replace('<application', `<uses-permission android:name="${permission}" />\n    <application`);
  }
  writeFileSync(manifestPath, manifest);
}

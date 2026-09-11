import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

/** The generated host gets the same native sources exercised by the socket tests.
 * Also callable for local native verification, without running release/keychain preparation.
 */
export function prepareAndroidOAuth(root, appId) {
  if (!/^[a-zA-Z][\w]*(?:\.[a-zA-Z][\w]*)+$/.test(appId))
    throw new Error('Invalid Android package');
  const app = join(root, 'android/app/src');
  const packagePath = appId.split('.');
  const main = join(app, 'main/java', ...packagePath);
  const activity = join(main, 'MainActivity.java');
  const before = readFileSync(activity, 'utf8');
  if (!before.includes('registerPlugin(OAuthLoopbackPlugin.class);')) {
    const marker = 'super.onCreate(savedInstanceState);';
    if (!before.includes(marker))
      throw new Error('Android activity has no plugin registration seam');
    writeFileSync(
      activity,
      before.replace(marker, 'registerPlugin(OAuthLoopbackPlugin.class);\n        ' + marker),
    );
  }
  for (const [directory, target, files] of [
    ['android', main, ['OAuthLoopback.java', 'OAuthLoopbackPlugin.java']],
    ['android-test', join(app, 'test/java', ...packagePath), ['OAuthLoopbackTest.java']],
  ]) {
    mkdirSync(target, { recursive: true });
    for (const file of files) {
      const source = readFileSync(join(root, 'native', directory, file), 'utf8').replace(
        'package com.blockether.viscompanion;',
        `package ${appId};`,
      );
      const path = join(target, file);
      if (!existsSync(path) || readFileSync(path, 'utf8') !== source) writeFileSync(path, source);
    }
  }
}

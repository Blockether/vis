import { mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync, cpSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { expect, it } from 'vitest';
import { prepareAndroidOAuth } from './oauth-native.mjs';

it('stamps and registers the tested Android receiver idempotently, without release secrets', () => {
  const root = mkdtempSync(join(tmpdir(), 'vis-oauth-'));
  try {
    const java = join(root, 'android/app/src/main/java/com/blockether/viscompanion');
    mkdirSync(java, { recursive: true });
    const activity = join(java, 'MainActivity.java');
    writeFileSync(activity, 'super.onCreate(savedInstanceState);');
    cpSync(new URL('../native', import.meta.url), join(root, 'native'), {
      recursive: true,
      filter: (path) => !String(path).includes('.build'),
    });
    prepareAndroidOAuth(root, 'com.blockether.viscompanion');
    const before = readFileSync(activity, 'utf8');
    prepareAndroidOAuth(root, 'com.blockether.viscompanion');
    expect(readFileSync(activity, 'utf8')).toBe(before);
    expect(before.match(/registerPlugin\(OAuthLoopbackPlugin.class\)/g)).toHaveLength(1);
    expect(readFileSync(join(java, 'OAuthLoopback.java'), 'utf8')).toContain('class OAuthLoopback');
    expect(readFileSync(join(java, 'OAuthLoopbackPlugin.java'), 'utf8')).toContain(
      'name = "OAuthLoopback"',
    );
    expect(
      readFileSync(
        join(root, 'android/app/src/test/java/com/blockether/viscompanion/OAuthLoopbackTest.java'),
        'utf8',
      ),
    ).toContain('@Test');
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});

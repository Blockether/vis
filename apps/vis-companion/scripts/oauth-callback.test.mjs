import { readFileSync } from 'node:fs';
import { expect, it } from 'vitest';
const source = name => readFileSync(new URL(name, import.meta.url), 'utf8');
it('registers the same private-use OAuth scheme on both mobile hosts, with no HTTPS callback', () => {
  const ios = source('./ios-prepare.mjs'); const android = source('./android-prepare.mjs');
  expect(ios).toContain('<string>com.blockether.viscompanion</string>');
  expect(android).toContain('android:scheme="com.blockether.viscompanion" android:host="oauth" android:path="/callback"');
});
it('does not log native bridge payloads containing authorization callbacks', () => {
  expect(source('../capacitor.config.mts')).toContain("loggingBehavior: 'none'");
});

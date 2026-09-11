// @vitest-environment jsdom
import { readFileSync } from 'node:fs';
import { runInNewContext } from 'node:vm';
import { expect, it } from 'vitest';
const source = (name) => readFileSync(new URL(name, import.meta.url), 'utf8');
it('registers the same private-use OAuth scheme on both mobile hosts, with no HTTPS callback', () => {
  const ios = source('./ios-prepare.mjs');
  const android = source('./android-prepare.mjs');
  expect(ios).toContain('<string>com.blockether.viscompanion</string>');
  expect(android).toContain(
    'android:scheme="com.blockether.viscompanion" android:host="oauth" android:path="/callback"',
  );
});
it('does not log native bridge payloads containing authorization callbacks', () => {
  expect(source('../capacitor.config.mts')).toContain("loggingBehavior: 'none'");
});

// Regression: an existing iOS project kept only vis:// registered. Safari could
// authorize Linear, but the private-use callback had no installed URL handler.
function preparePlist(input) {
  const script = source('./ios-prepare.mjs');
  const start = script.indexOf('const plistEntries =');
  const end = script.indexOf('// ── 4.', start);
  expect(start).toBeGreaterThan(-1);
  expect(end).toBeGreaterThan(start);
  return runInNewContext(`${script.slice(start, end)}\n({ preparedPlist, plistOk })`, {
    bundleId: 'com.blockether.viscompanion',
    infoPlist: 'Info.plist',
    readFileSync: () => input,
    die: (message) => {
      throw new Error(message);
    },
  });
}
function parsePlist(xml) {
  const document = new DOMParser().parseFromString(xml, 'application/xml');
  expect(document.querySelector('parsererror')).toBeNull();
  return document;
}
function schemeArrays(document) {
  return [...document.querySelectorAll('key')]
    .filter((key) => key.textContent === 'CFBundleURLSchemes')
    .map((key) => key.nextElementSibling);
}
function registeredSchemes(xml) {
  return schemeArrays(parsePlist(xml)).flatMap((array) =>
    [...array.children].map((item) => item.textContent),
  );
}
const emptyPlist =
  '<plist version="1.0"><dict><key>Unrelated</key><string>keep</string></dict></plist>';
it('registers pairing and OAuth callbacks in the generated iOS plist, idempotently', () => {
  const result = preparePlist(emptyPlist);
  expect(result.plistOk).toBe(false);
  expect(registeredSchemes(result.preparedPlist)).toEqual(['vis', 'com.blockether.viscompanion']);
  expect(preparePlist(result.preparedPlist)).toEqual({ ...result, plistOk: true });
});
it('repairs callback registration in an already prepared iOS project, not just a fresh scaffold', () => {
  const fresh = preparePlist(emptyPlist).preparedPlist;
  const document = parsePlist(fresh);
  schemeArrays(document)[0].lastElementChild.remove();
  const stale = new XMLSerializer().serializeToString(document);
  // CFBundleURLName already contains the bundle ID: searching the whole file
  // for that string is not proof the operating system has a matching scheme.
  expect(stale).toContain('<string>com.blockether.viscompanion</string>');
  expect(registeredSchemes(stale)).toEqual(['vis']);
  const result = preparePlist(stale);
  expect(result.plistOk).toBe(false);
  expect(registeredSchemes(result.preparedPlist)).toEqual(['vis', 'com.blockether.viscompanion']);
  expect(result.preparedPlist).toContain('<key>Unrelated</key><string>keep</string>');
  expect(preparePlist(result.preparedPlist)).toEqual({ ...result, plistOk: true });
});
it('keeps iOS release callback registration in the same prepare step', () => {
  const release = source('./ios-release.mjs');
  expect(release).toContain("run('node', ['scripts/ios-prepare.mjs']);");
  expect(release).not.toContain('<key>CFBundleURLSchemes</key>');
});
it('preserves other schemes and URL types when adding the OAuth callback', () => {
  const fresh = preparePlist(emptyPlist).preparedPlist;
  const document = parsePlist(fresh);
  const array = schemeArrays(document)[0];
  array.lastElementChild.textContent = 'another-test-scheme';
  const other = array.parentElement.cloneNode(true);
  other.querySelector('array').innerHTML = '<string>unrelated-test-scheme</string>';
  array.parentElement.parentElement.prepend(other);
  const result = preparePlist(new XMLSerializer().serializeToString(document));
  expect(registeredSchemes(result.preparedPlist)).toEqual([
    'unrelated-test-scheme',
    'vis',
    'another-test-scheme',
    'com.blockether.viscompanion',
  ]);
  expect(preparePlist(result.preparedPlist)).toEqual({ ...result, plistOk: true });
});

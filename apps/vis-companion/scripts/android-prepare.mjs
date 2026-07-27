#!/usr/bin/env node
/**
 * Stamp the Android push config into the generated `android/` project.
 *
 * `android/` is gitignored (Capacitor regenerates it), and `google-services.json`
 * is per-project Firebase config that should not sit in the repo either. So it
 * lives in the login keychain next to the FCM service account, and this script
 * materialises it into `android/app/` right before a build. Capacitor's
 * `app/build.gradle` already applies the google-services plugin *only* when that
 * file exists, so without this step the app builds fine and simply never gets a
 * push token.
 *
 * Usage:
 *   node scripts/android-prepare.mjs [--file google-services.json]
 */
import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const args = process.argv.slice(2);
const flag = (name) => {
  const i = args.indexOf(`--${name}`);
  return i === -1 ? undefined : args[i + 1];
};

const die = (msg) => {
  console.error(`\n\u2717 ${msg}\n`);
  process.exit(1);
};

// `security -w` prints hex for anything that is not plain printable ASCII.
const unhex = (s) => (/^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s);

const fromKeychain = () => {
  if (process.platform !== 'darwin') return undefined;
  const res = spawnSync('security', ['find-generic-password', '-s', 'vis-fcm', '-a', 'google_services', '-w'], {
    encoding: 'utf8',
  });
  return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
};

const home = process.env.HOME ?? '';
const expand = (p) => resolve(p.replace(/^~/, home));

const explicit = flag('file');
const fallback = `${home}/.vis/fcm/google-services.json`;
const raw =
  (explicit && readFileSync(expand(explicit), 'utf8')) ||
  fromKeychain() ||
  (existsSync(fallback) && readFileSync(fallback, 'utf8')) ||
  undefined;

if (!raw) {
  console.log('\n\u00b7 no google-services.json (keychain vis-fcm/google_services, ~/.vis/fcm/, or --file)');
  console.log('  Android push stays off; everything else builds normally.');
  console.log('  Firebase console \u25b8 Project settings \u25b8 your Android app \u25b8 download, then:');
  console.log('    npm run secrets android <google-services.json>\n');
  process.exit(0);
}

let parsed;
try {
  parsed = JSON.parse(raw);
} catch {
  die('that google-services.json is not valid JSON');
}

const appId = JSON.parse(readFileSync(resolve(root, 'capacitor.config.json'), 'utf8')).appId;
const packages = (parsed.client ?? []).map((c) => c?.client_info?.android_client_info?.package_name).filter(Boolean);
if (!packages.length) die('that JSON has no client[].client_info.android_client_info.package_name — wrong file?');
if (!packages.includes(appId)) {
  die(`google-services.json is for ${packages.join(', ')}, but this app is ${appId}\n  Add an Android app with that exact package name in the Firebase console.`);
}

const androidApp = resolve(root, 'android', 'app');
if (!existsSync(androidApp)) die('no android/app — run `npm run add:android` first');

const dest = resolve(androidApp, 'google-services.json');
writeFileSync(dest, raw, { mode: 0o600 });
console.log(`\u2713 google-services.json \u2192 android/app  (${appId}, project ${parsed.project_info?.project_id ?? '?'})`);

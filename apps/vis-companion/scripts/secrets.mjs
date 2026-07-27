#!/usr/bin/env node
/**
 * Release + push secrets, kept in the macOS login keychain instead of dotfiles.
 *
 * Two services, because two different processes read them:
 *
 *   vis-ios    read by scripts/ios-release.mjs  — App Store Connect API key,
 *              team id, optional Apple-ID fallback. Used to archive and upload.
 *   vis-apns   read by the GATEWAY (gateway/push.clj) — the APNs auth key that
 *              signs every notification.
 *
 * Nothing is ever written to a file in this repo, nothing lands in shell
 * history when values are piped on stdin, and a locked keychain revokes access
 * immediately. `security` prompts on first read by a new binary; hit "Always
 * Allow" once per tool.
 *
 * Usage:
 *   node scripts/secrets.mjs list
 *   node scripts/secrets.mjs set  <name> [value]      # value on stdin if omitted
 *   node scripts/secrets.mjs file <name> <path>       # store a file's contents
 *   node scripts/secrets.mjs rm   <name>
 *   node scripts/secrets.mjs asc  <AuthKey_XXXX.p8> --issuer <uuid> [--team <id>]
 *   node scripts/secrets.mjs apns <AuthKey_XXXX.p8> --team <id> --topic <bundle id>
 *                                                    [--env sandbox|production]
 *   node scripts/secrets.mjs fcm  <service-account.json> [--project <id>]
 *   node scripts/secrets.mjs android <google-services.json>
 *   node scripts/secrets.mjs doctor
 */
import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import { basename, resolve } from 'node:path';

if (process.platform !== 'darwin') {
  console.error('\n✗ keychain storage is macOS-only; use environment variables elsewhere\n');
  process.exit(1);
}

const IOS = 'vis-ios';
const APNS = 'vis-apns';

const FCM = 'vis-fcm';

// name -> [service, account, what it is]. The account names are the contract the
// readers use; changing one here means changing it in the reader too.
const SECRETS = {
  team_id: [IOS, 'team_id', 'Apple Developer team id (10 chars)'],
  asc_key_id: [IOS, 'asc_key_id', 'App Store Connect API key id'],
  asc_issuer_id: [IOS, 'asc_issuer_id', 'App Store Connect issuer id (uuid)'],
  asc_key: [IOS, 'asc_key', 'App Store Connect API key, the .p8 contents'],
  apple_id: [IOS, 'apple_id', 'Apple ID (only for the app-password upload path)'],
  app_password: [IOS, 'app_password', 'app-specific password for that Apple ID'],
  apns_key: [APNS, 'key', 'APNs auth key, the .p8 contents'],
  apns_key_id: [APNS, 'key_id', 'APNs key id (from the AuthKey_<id>.p8 name)'],
  apns_team_id: [APNS, 'team_id', 'Apple Developer team id'],
  apns_topic: [APNS, 'topic', 'bundle id APNs delivers to'],
  apns_env: [APNS, 'environment', 'sandbox | production'],
  fcm_service_account: [FCM, 'service_account', 'Firebase service-account JSON (Android push)'],
  fcm_project_id: [FCM, 'project_id', 'Firebase project id (inferred from the JSON)'],
  fcm_google_services: [FCM, 'google_services', 'google-services.json stamped into the Android build'],
};

const args = process.argv.slice(2);
const cmd = args[0];
const flag = (name) => {
  const i = args.indexOf(`--${name}`);
  return i === -1 ? undefined : args[i + 1];
};

const die = (msg) => {
  console.error(`\n✗ ${msg}\n`);
  process.exit(1);
};

const entry = (name) => SECRETS[name] ?? die(`unknown secret "${name}"\n  known: ${Object.keys(SECRETS).join(', ')}`);

const put = (name, value) => {
  const [service, account] = entry(name);
  if (!value || !value.trim()) die(`refusing to store an empty ${name}`);
  const res = spawnSync(
    'security',
    ['add-generic-password', '-U', '-s', service, '-a', account, '-l', `${service}:${account}`, '-w', value],
    { stdio: ['ignore', 'ignore', 'inherit'] },
  );
  if (res.status !== 0) die(`could not store ${name} in the keychain`);
  console.log(`· stored ${name}  (${service} / ${account})`);
};

// `security -w` prints hex for anything that is not plain printable ASCII.
const unhex = (s) => (/^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s);

const peek = (name) => {
  const [service, account] = entry(name);
  const res = spawnSync('security', ['find-generic-password', '-s', service, '-a', account, '-w'], {
    encoding: 'utf8',
  });
  return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
};

const stdin = () => {
  if (process.stdin.isTTY) die('no value given and stdin is a terminal — pipe it in, or pass it as an argument');
  return readFileSync(0, 'utf8').trim();
};

// Apple names the download AuthKey_<keyid>.p8, so the id needs no extra typing.
const keyIdOf = (path) => {
  const m = /^AuthKey_(.+)\.p8$/i.exec(basename(path));
  return m?.[1];
};

const readP8 = (path) => {
  const p = resolve(path.replace(/^~/, process.env.HOME ?? '~'));
  if (!existsSync(p)) die(`no such key file: ${p}`);
  const pem = readFileSync(p, 'utf8');
  if (!pem.includes('BEGIN PRIVATE KEY')) die(`${p} is not a PKCS#8 .p8 private key`);
  return { p, pem, keyId: keyIdOf(p) };
};

switch (cmd) {
  case 'list': {
    for (const [name, [service, account, what]] of Object.entries(SECRETS)) {
      const v = peek(name);
      const shown = v ? (v.includes('PRIVATE KEY') ? `${v.length} bytes of key material` : `${v.slice(0, 4)}…`) : '—';
      console.log(`${v ? '✓' : '·'} ${name.padEnd(14)} ${shown.padEnd(28)} ${service}/${account}  ${what}`);
    }
    break;
  }

  case 'set':
    put(args[1], args[2] ?? stdin());
    break;

  case 'file': {
    if (!args[2]) die('usage: secrets.mjs file <name> <path>');
    put(args[1], readFileSync(resolve(args[2]), 'utf8'));
    break;
  }

  case 'rm': {
    const [service, account] = entry(args[1]);
    spawnSync('security', ['delete-generic-password', '-s', service, '-a', account], { stdio: 'inherit' });
    break;
  }

  case 'asc': {
    // App Store Connect API key: what xcodebuild/altool authenticate with, so a
    // release never needs an interactive 2FA prompt.
    const { pem, keyId } = readP8(args[1] ?? die('usage: secrets.mjs asc <AuthKey_XXXX.p8> --issuer <uuid>'));
    const issuer = flag('issuer') ?? die('--issuer is required (App Store Connect > Users and Access > Integrations)');
    put('asc_key', pem);
    put('asc_key_id', flag('key-id') ?? keyId ?? die('cannot infer the key id — pass --key-id'));
    put('asc_issuer_id', issuer);
    if (flag('team')) put('team_id', flag('team'));
    console.log('\n✓ App Store Connect key stored. `npm run release:ios` now uploads headlessly.\n');
    break;
  }

  case 'apns': {
    // APNs auth key: read by the gateway, never by this app.
    const { pem, keyId } = readP8(args[1] ?? die('usage: secrets.mjs apns <AuthKey_XXXX.p8> --team <id> --topic <bundle id>'));
    put('apns_key', pem);
    put('apns_key_id', flag('key-id') ?? keyId ?? die('cannot infer the key id — pass --key-id'));
    put('apns_team_id', flag('team') ?? die('--team is required'));
    put('apns_topic', flag('topic') ?? die('--topic is required (your bundle id, exactly)'));
    put('apns_env', flag('env') ?? 'production');
    console.log('\n✓ APNs key stored. Restart the gateway; Settings ▸ Notifications flips to ready.');
    console.log('  The .p8 on disk is now redundant — shred it:  rm -P <the file>\n');
    break;
  }

  case 'fcm': {
    // Firebase service account: the Android half of push, read by the gateway.
    // Same trust model as the APNs key — it never lands in a file in this repo.
    const path = args[1] ?? die('usage: secrets.mjs fcm <service-account.json>');
    const raw = readFileSync(resolve(path), 'utf8');
    let parsed;
    try {
      parsed = JSON.parse(raw);
    } catch {
      die(`${path} is not JSON — download it from Firebase console ▸ Project settings ▸ Service accounts`);
    }
    if (parsed.type !== 'service_account' || !parsed.private_key || !parsed.client_email) {
      die('that JSON is not a service-account key (needs type, client_email, private_key)');
    }
    put('fcm_service_account', raw);
    put('fcm_project_id', flag('project') ?? parsed.project_id ?? die('cannot infer the project id — pass --project'));
    console.log(`\n✓ FCM service account stored for project ${parsed.project_id}.`);
    console.log('  Restart the gateway; Android devices can now be notified.');
    console.log('  The JSON on disk is now redundant — shred it:  rm -P <the file>\n');
    break;
  }

  case 'android': {
    // google-services.json is app config, not a credential, but it is per-project
    // and `android/` is gitignored — so it rides along here and is stamped in by
    // scripts/android-prepare.mjs at build time.
    const path = args[1] ?? die('usage: secrets.mjs android <google-services.json>');
    const raw = readFileSync(resolve(path.replace(/^~/, process.env.HOME ?? '~')), 'utf8');
    let parsed;
    try {
      parsed = JSON.parse(raw);
    } catch {
      die('that file is not valid JSON');
    }
    const pkgs = (parsed.client ?? []).map((c) => c?.client_info?.android_client_info?.package_name).filter(Boolean);
    if (!pkgs.length) die('that JSON is not a google-services.json (no client[].client_info.android_client_info)');
    put('fcm_google_services', raw);
    console.log(`\n✓ google-services.json stored for ${pkgs.join(', ')}.`);
    console.log('  Run `npm run prepare:android` (or any android script) to stamp it in.\n');
    break;
  }

  case 'doctor': {
    const need = (names) => names.filter((n) => !peek(n));
    const asc = need(['asc_key', 'asc_key_id', 'asc_issuer_id']);
    const apns = need(['apns_key', 'apns_key_id', 'apns_team_id', 'apns_topic']);
    const fcm = need(['fcm_service_account', 'fcm_project_id']);
    console.log(asc.length ? `· release: falls back to Xcode's signed-in account (missing ${asc.join(', ')})` : '✓ release: App Store Connect API key ready');
    console.log(apns.length ? `· push iOS:     NOT configured (missing ${apns.join(', ')})` : '✓ push iOS:     APNs key ready');
    console.log(fcm.length ? `· push Android: NOT configured (missing ${fcm.join(', ')})` : '✓ push Android: FCM service account ready');
    if (!peek('team_id')) console.log('· team_id unset — VIS_IOS_TEAM_ID or the script default is used');
    break;
  }

  default:
    console.log(readFileSync(new URL(import.meta.url), 'utf8').split('\n').slice(1, 27).join('\n'));
    process.exit(cmd ? 1 : 0);
}

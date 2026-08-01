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
 *   node scripts/secrets.mjs play <service-account.json> [--package <id>]
 *   node scripts/secrets.mjs keystore <upload.jks> --alias <a> [--store-pass <p>] [--key-pass <p>]
 *   node scripts/secrets.mjs keystore create [--alias upload]   # generate one, keep it here
 *   node scripts/secrets.mjs keystore adopt                     # import android/keystore.properties
 *   node scripts/secrets.mjs export-keystore > upload.jks       # the only way key bytes leave
 *   node scripts/secrets.mjs doctor
 */
import { spawnSync } from 'node:child_process';
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { randomBytes } from 'node:crypto';
import { tmpdir } from 'node:os';
import { basename, join, resolve } from 'node:path';

if (process.platform !== 'darwin') {
  console.error('\n✗ keychain storage is macOS-only; use environment variables elsewhere\n');
  process.exit(1);
}

const IOS = 'vis-ios';
const APNS = 'vis-apns';

const FCM = 'vis-fcm';
// Google Play: the release path (upload keystore + Play Developer API), the Android
// mirror of vis-ios. Read by scripts/android-prepare.mjs and scripts/android-release.mjs.
const PLAY = 'vis-play';
const ANDROID = 'vis-android';

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
  play_service_account: [PLAY, 'service_account', 'Google Play Developer API service-account JSON'],
  play_package: [PLAY, 'package', 'Play package name (defaults to the Capacitor appId)'],
  keystore: [ANDROID, 'keystore', 'upload keystore (.jks), base64 — signs every release build'],
  keystore_password: [ANDROID, 'keystore_password', 'keystore password'],
  key_alias: [ANDROID, 'key_alias', 'key alias inside the keystore'],
  key_password: [ANDROID, 'key_password', 'key password (defaults to the keystore password)'],
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
      const shown = v
        ? v.includes('PRIVATE KEY') || v.length > 200
          ? `${v.length} bytes of key material`
          : `${v.slice(0, 4)}…`
        : '—';
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

  case 'play': {
    // Google Play Developer API service account: what android-release.mjs uploads with,
    // so a Play release never needs the console. Create it in Google Cloud (the project
    // linked to the Play developer account), then grant it access in
    // Play Console ▸ Users and permissions ▸ Invite → Releases: "Release to testing tracks".
    const path = args[1] ?? die('usage: secrets.mjs play <service-account.json> [--package <id>]');
    const raw = readFileSync(resolve(path.replace(/^~/, process.env.HOME ?? '~')), 'utf8');
    let parsed;
    try {
      parsed = JSON.parse(raw);
    } catch {
      die(`${path} is not JSON — download it from Google Cloud ▸ IAM ▸ Service accounts ▸ Keys`);
    }
    if (parsed.type !== 'service_account' || !parsed.private_key || !parsed.client_email) {
      die('that JSON is not a service-account key (needs type, client_email, private_key)');
    }
    put('play_service_account', raw);
    if (flag('package')) put('play_package', flag('package'));
    console.log(`\n✓ Play service account stored (${parsed.client_email}).`);
    console.log('  Grant it release access in Play Console ▸ Users and permissions if you have not.');
    console.log('  Then: npm run release:android  # triggers the matching two-store release');
    console.log('  The JSON on disk is now redundant — shred it:  rm -P <the file>\n');
    break;
  }

  case 'keystore': {
    // The upload key. Losing it is NOT fatal under Play App Signing (Google holds the app
    // signing key and you can request an upload-key reset), but it still costs days — so it
    // lives in the keychain, base64-encoded because a .jks is binary.
    const alias = flag('alias') ?? 'upload';
    if (args[1] === 'create') {
      // Generate one rather than making the user remember keytool's flag soup. The password
      // is random and only ever exists in the keychain — nobody has to type or store it.
      const password = randomBytes(24).toString('base64url');
      const dir = mkdtempSync(join(tmpdir(), 'vis-ks-'));
      const file = join(dir, 'upload.jks');
      try {
        const res = spawnSync(
          'keytool',
          ['-genkeypair', '-v', '-keystore', file, '-storetype', 'PKCS12', '-alias', alias, '-keyalg', 'RSA', '-keysize', '4096',
            '-validity', '10000', '-storepass', password, '-keypass', password,
            '-dname', flag('dname') ?? 'CN=Vis, OU=Vis Companion, O=Blockether, C=PL'],
          { stdio: ['ignore', 'ignore', 'inherit'] },
        );
        if (res.status !== 0) die('keytool failed — is a JDK on PATH? (brew install temurin)');
        put('keystore', readFileSync(file).toString('base64'));
        put('keystore_password', password);
        put('key_alias', alias);
        put('key_password', password);
      } finally {
        rmSync(dir, { recursive: true, force: true });
      }
      console.log(`\n✓ generated a fresh upload keystore (alias ${alias}, PKCS12, RSA 4096).`);
      console.log('  It exists ONLY in the keychain — back the keychain up, or export it with');
      console.log('    npm run secrets -- export-keystore > upload.jks   (then store it somewhere safe)\n');
      break;
    }
    if (args[1] === 'adopt') {
      // Import a keystore that already exists on disk — the usual case: a key was made by
      // hand before this script existed, has already signed an upload, and MUST NOT be
      // replaced (Play ties the upload key to the app). Read from android/keystore.properties
      // so no password is ever typed on a command line.
      const props = resolve(new URL('../android/keystore.properties', import.meta.url).pathname);
      if (!existsSync(props)) die('no android/keystore.properties to adopt — pass the .jks path instead');
      const kv = Object.fromEntries(
        readFileSync(props, 'utf8')
          .split('\n')
          .filter((l) => l.includes('='))
          .map((l) => [l.slice(0, l.indexOf('=')).trim(), l.slice(l.indexOf('=') + 1).trim()]),
      );
      const store = resolve(String(kv.storeFile ?? '').replace(/^~/, process.env.HOME ?? '~'));
      if (!existsSync(store)) die(`keystore.properties points at ${store}, which does not exist`);
      put('keystore', readFileSync(store).toString('base64'));
      put('keystore_password', kv.storePassword ?? die('keystore.properties has no storePassword'));
      put('key_alias', kv.keyAlias ?? 'upload');
      put('key_password', kv.keyPassword ?? kv.storePassword);
      console.log(`\n✓ adopted ${store} (alias ${kv.keyAlias}). It is now reproducible on any machine`);
      console.log('  with this keychain, and CI can take it as VIS_ANDROID_KEYSTORE (base64).\n');
      break;
    }
    const path = args[1] ?? die('usage: secrets.mjs keystore <upload.jks> --alias <a> [--store-pass <p>]  |  keystore create  |  keystore adopt');
    const p = resolve(path.replace(/^~/, process.env.HOME ?? '~'));
    if (!existsSync(p)) die(`no such keystore: ${p}`);
    const storePass = flag('store-pass') ?? die('--store-pass is required');
    put('keystore', readFileSync(p).toString('base64'));
    put('keystore_password', storePass);
    put('key_alias', alias);
    put('key_password', flag('key-pass') ?? storePass);
    console.log(`\n✓ upload keystore stored (alias ${alias}).\n`);
    break;
  }

  case 'export-keystore': {
    // The one way key material leaves the keychain: raw bytes on stdout, never a file this
    // script chooses. Redirect it yourself, to somewhere you actually want it.
    const b64 = peek('keystore') ?? die('no keystore stored');
    process.stdout.write(Buffer.from(b64, 'base64'));
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
    const play = need(['play_service_account']);
    const ks = need(['keystore', 'keystore_password', 'key_alias']);
    console.log(play.length ? '· Play:          NOT configured (missing play_service_account)' : '✓ Play:          Developer API service account ready');
    console.log(ks.length ? `· Android sign:  NOT configured (missing ${ks.join(', ')}) — \`npm run secrets keystore create\`` : '✓ Android sign:  upload keystore ready');
    if (!peek('team_id')) console.log('· team_id unset — VIS_IOS_TEAM_ID or the script default is used');
    break;
  }

  default:
    console.log(readFileSync(new URL(import.meta.url), 'utf8').split('\n').slice(1, 32).join('\n'));
    process.exit(cmd ? 1 : 0);
}

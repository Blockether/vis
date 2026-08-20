#!/usr/bin/env node
/**
 * One command: web bundle -> Capacitor sync -> signed archive -> .ipa -> TestFlight.
 *
 * Versioning has no hand-edited state. The iOS project's own MARKETING_VERSION /
 * CURRENT_PROJECT_VERSION are IGNORED and passed as build settings instead:
 *
 *   CFBundleShortVersionString = repo-root VIS_VERSION       (the release users see)
 *   CFBundleVersion            = `git rev-list --count HEAD`  (strictly monotonic)
 *
 * so `ios/` stays a disposable, regenerable Capacitor output (it is gitignored)
 * and two uploads can never collide on a build number.
 *
 * Usage (workflow/store recovery only; normal releases use `npm run release:ios`):
 *   npm run release:ios:store                 # build + archive + export + upload
 *   npm run release:ios:store -- --no-upload  # stop at the .ipa (no App Store Connect call)
 *   npm run release:ios:store -- --build 4711          # recovery-only build override
 *   npm run release:ios:store -- --skip-web   # reuse dist/ and the last `cap sync`
 *   npm run release:ios:store -- --prepare    # web + cap sync + stamp versions, then STOP
 *   npm run release:ios:store -- --prepare --dev  # same, but aps-environment=development
 *                                             # (archive by hand in Xcode: Product > Archive)
 *   npm run release:ios:store -- --audience internal   # team only: skip the public-link group
 *   npm run release:ios:store -- --audience all        # the default, spelled out: every tester
 *                                             # audience — internal groups AND public TestFlight
 *                                             # after beta review, the same fan-out `--track all`
 *                                             # gives Play
 *
 * Upload auth, in order of preference:
 *   1. App Store Connect API key (headless, CI-friendly, no 2FA prompt):
 *      VIS_ASC_KEY_ID, VIS_ASC_ISSUER_ID, VIS_ASC_KEY_PATH=/path/AuthKey_<KEYID>.p8
 *   2. Apple ID + app-specific password: VIS_ASC_APPLE_ID, VIS_ASC_APP_PASSWORD
 *   3. Nothing set -> re-export with destination=upload, which authenticates as the
 *      account signed into Xcode (Xcode > Settings > Accounts).
 *
 * Signing is Xcode-managed: the distribution certificate is cloud-managed for the
 * team, and `-allowProvisioningUpdates` creates any missing cert or App Store
 * profile.
 */
import { spawnSync } from 'node:child_process';
import { chmodSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';
import { buildNotes, publishNotes } from './release-notes.mjs';
import { exportArchiveArgs, exportOptionsPlist, signingPlan } from './ios-export.mjs';
import { distributionIdentity, ensureProfiles, installProfile, stampManualSigning } from './ios-signing.mjs';
import { distribute, planDistribution } from './testflight.mjs';
import { syncPackageVersion } from './version.mjs';

// Release credentials live in the macOS login keychain (scripts/secrets.mjs),
// never in a dotfile or this repo. An env var still wins, so CI can inject one.
// `security -w` prints hex whenever the stored password is not plain printable
// ASCII, which a multi-line PEM never is.
const unhex = (s) => (/^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s);
const keychain = (service, account) => {
  if (process.platform !== 'darwin') return undefined;
  const res = spawnSync('security', ['find-generic-password', '-s', service, '-a', account, '-w'], {
    encoding: 'utf8',
  });
  return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
};
const secret = (envName, account) => process.env[envName]?.trim() || keychain('vis-ios', account);

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), '..');

const appBundleId = JSON.parse(readFileSync(join(appDir, 'capacitor.config.json'), 'utf8')).appId;
const shareBundleId = `${appBundleId}.share`;
const notifyBundleId = `${appBundleId}.notify`;

const iosDir = join(appDir, 'ios');
const projectDir = join(iosDir, 'App');
const exportOptions = join(iosDir, 'ExportOptions.plist');
const teamId = secret('VIS_IOS_TEAM_ID', 'team_id') ?? 'JSZTFUBUBB';

const args = process.argv.slice(2);
const flag = (name) => {
  const i = args.indexOf(`--${name}`);
  return i === -1 ? undefined : args[i + 1];
};
// Repeatable flags: `--audience internal --audience public` reads as both, not as the last one.
const flags = (name) => args.flatMap((a, i) => (a === `--${name}` && args[i + 1] ? [args[i + 1]] : []));
const has = (name) => args.includes(`--${name}`);

const die = (msg) => {
  console.error(`\n✗ ${msg}\n`);
  process.exit(1);
};

const run = (cmd, cmdArgs, opts = {}) => {
  console.log(`\n$ ${cmd} ${cmdArgs.join(' ')}`);
  const res = spawnSync(cmd, cmdArgs, { stdio: 'inherit', cwd: appDir, ...opts });
  if (res.status !== 0) die(`${cmd} failed (exit ${res.status ?? 'signal'})`);
};

const capture = (cmd, cmdArgs, opts = {}) => {
  const res = spawnSync(cmd, cmdArgs, { encoding: 'utf8', cwd: appDir, ...opts });
  return res.status === 0 ? res.stdout.trim() : '';
};

// Repo-root VIS_VERSION is the source of truth; npm metadata mirrors it.
const marketingVersion = syncPackageVersion();
const buildNumber = flag('build') ?? capture('git', ['rev-list', '--count', 'HEAD']);
if (!/^\d+(\.\d+)*$/.test(marketingVersion)) die(`bad VIS_VERSION "${marketingVersion}"`);
if (!/^\d+$/.test(buildNumber)) die(`bad --build "${buildNumber}" (git rev-list unavailable?)`);

// Who gets this build is planned BEFORE the archive, exactly like the Play tracks: an unknown
// audience must cost a second, not a ten-minute signed .ipa (scripts/testflight.mjs).
let plan;
try {
  plan = planDistribution({ audiences: flags('audience'), group: flag('group'), review: !has('no-review') });
} catch (err) {
  die(err.message);
}

const needsIosScaffold = !existsSync(projectDir);

if (needsIosScaffold) {
  if (has('skip-web')) {
    die('no iOS project to reuse with --skip-web; run a normal release once to scaffold it');
  }
  console.log('· no ios/ — scaffolding it with `cap add ios`');
  run('npm', ['run', 'build']);
  run('npx', ['cap', 'add', 'ios']);
}

const outDir = join(appDir, 'build', 'ios');
const archivePath = join(outDir, `Vis-${marketingVersion}-${buildNumber}.xcarchive`);
const ipaDir = join(outDir, `export-${marketingVersion}-${buildNumber}`);
mkdirSync(outDir, { recursive: true });
rmSync(archivePath, { recursive: true, force: true });
rmSync(ipaDir, { recursive: true, force: true });

console.log(
  `\nVis Companion → TestFlight\n  version ${marketingVersion} (build ${buildNumber})\n  testers ${plan.audiences.join(', ')}\n  team    ${teamId}\n  archive ${archivePath}`,
);

// Generated BEFORE the archive so a bad/empty changelog fails fast rather than after a
// 10-minute build, and so `--prepare` / `--no-upload` still show what testers would read.
// `--no-changelog` keeps CHANGELOG.md untouched (dry runs, re-exports of the same build).
const notes = has('no-notes')
  ? { text: '', bullets: [] }
  : buildNotes({
      version: marketingVersion,
      build: buildNumber,
      scope: flag('notes-scope') ? [flag('notes-scope')] : undefined,
      write: !has('no-changelog'),
    });
if (notes.text) console.log(`\nWhat to Test${notes.reused ? ' (from CHANGELOG.md)' : ''}:\n${notes.text}\n`);

if (!has('skip-web')) {
  if (!needsIosScaffold) run('npm', ['run', 'build']);
  run('npx', ['cap', 'sync', 'ios']);
}

// `ios/` is gitignored and an existing one is never regenerated, so a machine
// that ever built the old native viewport bridge still has the class stamped into
// AppDelegate.swift and a storyboard pointing at it. This removes both. Idempotent,
// and it must run even with `--skip-web`: otherwise the archive ships a storyboard
// naming a class that no longer exists, which crashes on launch.
run('node', ['scripts/ios-prepare.mjs']);

// A hand-made Xcode archive reads the PROJECT, not our build settings, so stamp the
// pbxproj too — otherwise Product > Archive ships MARKETING_VERSION 1.0 / build 1.
const pbxproj = join(projectDir, 'App.xcodeproj', 'project.pbxproj');
const stamped = readFileSync(pbxproj, 'utf8')
  .replaceAll(/MARKETING_VERSION = [^;]+;/g, `MARKETING_VERSION = ${marketingVersion};`)
  .replaceAll(/CURRENT_PROJECT_VERSION = [^;]+;/g, `CURRENT_PROJECT_VERSION = ${buildNumber};`);
writeFileSync(pbxproj, stamped);
console.log(`· stamped App.xcodeproj  ${marketingVersion} (${buildNumber})`);

// Push notifications are a NATIVE capability, and `ios/` is gitignored and
// regenerable — so the entitlement, its wiring into the target, and the
// AppDelegate token forwarding are stamped here rather than committed.
//
// `aps-environment` must MATCH the provisioning profile the build is signed
// with, or codesign rejects it: an Xcode Run onto a cable-attached phone uses
// the *development* profile (sandbox APNs), an archive uses distribution
// (production). Default production; `--aps development` (or `--dev`) for the
// device-testing loop. The file is rewritten whenever the value differs.
//
// The same file carries the App Group the share extension stages a shared FILE
// into (scripts/ios-prepare.mjs adds it when this script has not run yet).
const apsEnvironment = flag('aps') ?? (has('dev') ? 'development' : 'production');
if (!['development', 'production'].includes(apsEnvironment)) {
  die(`bad --aps "${apsEnvironment}" (development | production)`);
}
const entitlementsPath = join(projectDir, 'App', 'App.entitlements');
const entitlements = `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>aps-environment</key>
  <string>${apsEnvironment}</string>
  <key>com.apple.security.application-groups</key>
  <array>
    <string>group.${appBundleId}</string>
  </array>
</dict>
</plist>
`;
if (!existsSync(entitlementsPath) || readFileSync(entitlementsPath, 'utf8') !== entitlements) {
  writeFileSync(entitlementsPath, entitlements);
  console.log(`· wrote ${entitlementsPath} (aps-environment ${apsEnvironment})`);
}

// Point every build configuration of the App target at it. Without this the
// archive is signed with no `aps-environment` and APNs rejects every token.
// Only the app target: the VisShare extension carries its OWN entitlements file
// — the App Group and nothing else — and must never inherit the app's push.
let project = readFileSync(pbxproj, 'utf8');
if (!project.includes('CODE_SIGN_ENTITLEMENTS = App/App.entitlements')) {
  const idPattern = appBundleId.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  project = project.replaceAll(
    new RegExp(`(\\n(\\s*)PRODUCT_BUNDLE_IDENTIFIER = "?${idPattern}"?;)`, 'g'),
    '$1\n$2CODE_SIGN_ENTITLEMENTS = App/App.entitlements;',
  );
  writeFileSync(pbxproj, project);
  console.log('· enabled Push Notifications capability (CODE_SIGN_ENTITLEMENTS)');
}

// Capacitor's push plugin receives the APNs token through NotificationCenter;
// the generated AppDelegate does not forward it, so registration would hang.
const appDelegate = join(projectDir, 'App', 'AppDelegate.swift');
const delegateSrc = readFileSync(appDelegate, 'utf8');
if (!delegateSrc.includes('didRegisterForRemoteNotificationsWithDeviceToken')) {
  const forwarding = `
    func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        NotificationCenter.default.post(name: .capacitorDidRegisterForRemoteNotifications, object: deviceToken)
    }

    func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: Error) {
        NotificationCenter.default.post(name: .capacitorDidFailToRegisterForRemoteNotifications, object: error)
    }
`;
  const at = delegateSrc.lastIndexOf('}');
  writeFileSync(appDelegate, delegateSrc.slice(0, at) + forwarding + delegateSrc.slice(at));
  console.log('· wired AppDelegate push-token forwarding');
}

// The `vis://gateway?url=…&token=…` pairing link is useless unless iOS knows
// this app owns the scheme: without CFBundleURLTypes the link is dead text
// everywhere (Messages, Notes, a scanned QR) and `appUrlOpen` never fires.
// `ios/` is gitignored and regenerated by `cap sync`, so stamp it here.
const bundleId =
  readFileSync(pbxproj, 'utf8').match(/PRODUCT_BUNDLE_IDENTIFIER = ([^;]+);/)?.[1]?.trim() ??
  'com.blockether.viscompanion';
const infoPlist = join(projectDir, 'App', 'Info.plist');
const infoSrc = readFileSync(infoPlist, 'utf8');
if (!infoSrc.includes('CFBundleURLTypes')) {
  const urlTypes = `\t<key>CFBundleURLTypes</key>
\t<array>
\t\t<dict>
\t\t\t<key>CFBundleURLName</key>
\t\t\t<string>${bundleId}</string>
\t\t\t<key>CFBundleTypeRole</key>
\t\t\t<string>Editor</string>
\t\t\t<key>CFBundleURLSchemes</key>
\t\t\t<array>
\t\t\t\t<string>vis</string>
\t\t\t</array>
\t\t</dict>
\t</array>
`;
  const at = infoSrc.lastIndexOf('</dict>');
  writeFileSync(infoPlist, infoSrc.slice(0, at) + urlTypes + infoSrc.slice(at));
  console.log('· registered the vis:// URL scheme (CFBundleURLTypes)');
}

// Dictation must survive the app going to the background, the screen locking or
// a call arriving. Without UIBackgroundModes=audio WebKit MUTES the WKWebView's
// getUserMedia capture the moment the app backgrounds (WebKit bug 226620), and
// the transcript is lost mid-sentence. This key is only HALF the fix — it keeps
// the microphone track live, while the `play-and-record` audio session claimed
// in src/lib/voice.ts is what keeps the AudioContext draining it from being
// interrupted. Same reason as above — `ios/` is gitignored, so stamp it every run.
{
  const src = readFileSync(infoPlist, 'utf8');
  if (!src.includes('UIBackgroundModes')) {
    const modes = `\t<key>UIBackgroundModes</key>
\t<array>
\t\t<string>audio</string>
\t</array>
`;
    const at = src.lastIndexOf('</dict>');
    writeFileSync(infoPlist, src.slice(0, at) + modes + src.slice(at));
    console.log('· kept audio capture alive in the background (UIBackgroundModes)');
  }
}

if (has('prepare')) {
  console.log(
    `\n✓ prepared ${marketingVersion} (${buildNumber}).\n` +
      `  Open:    open ${join(projectDir, 'App.xcworkspace')}\n` +
      '  Archive: scheme App, destination "Any iOS Device (arm64)", Product > Archive\n',
  );
  process.exit(0);
}

// App Store Connect API key: also lets xcodebuild create signing assets itself.
const keyId = secret('VIS_ASC_KEY_ID', 'asc_key_id');
const issuerId = secret('VIS_ASC_ISSUER_ID', 'asc_issuer_id');
const keyPem = process.env.VIS_ASC_KEY_PATH ? undefined : keychain('vis-ios', 'asc_key');

// xcodebuild and altool both insist on a FILE. Materialise the keychain copy in
// a private temp dir for the length of the run, then shred it — including when
// the run dies, so key material never outlives the process.
let keyPath = process.env.VIS_ASC_KEY_PATH;
if (keyPem && keyId) {
  const dir = mkdtempSync(join(tmpdir(), 'vis-asc-'));
  keyPath = join(dir, `AuthKey_${keyId}.p8`);
  writeFileSync(keyPath, keyPem.endsWith('\n') ? keyPem : `${keyPem}\n`, { mode: 0o600 });
  chmodSync(dir, 0o700);
  const shred = () => rmSync(dir, { recursive: true, force: true });
  process.on('exit', shred);
  for (const sig of ['SIGINT', 'SIGTERM', 'SIGHUP']) process.on(sig, () => process.exit(1));
}

const hasApiKey = Boolean(keyId && issuerId && keyPath);
if (hasApiKey && !existsSync(keyPath)) die(`App Store Connect key does not exist: ${keyPath}`);
console.log(
  hasApiKey
    ? `· signing in with App Store Connect key ${keyId}${keyPem ? ' (keychain)' : ''}`
    : "· no App Store Connect key — falling back to the Apple account signed into Xcode",
);

// Sign the archive BY HAND wherever that is possible.
//
// Automatic signing mints a brand new "Apple Development" certificate on every
// runner whose keychain starts empty, and the account caps them: build 3080 died
// on `Choose a certificate to revoke. Your account has reached the maximum number
// of certificates`, a dozen dead CI certificates holding every slot. Signing with
// the distribution certificate the workflow already imports asks the portal for
// nothing — but manual signing is all-or-nothing, so every bundle in the archive
// must be named, the share extension (`<app>.share`) included. Those profiles come
// from the App Store Connect API this release already authenticates with, and are
// created there when missing, so no new secret is ever needed.
//
// Only where it can actually work: without the distribution certificate's private
// key in a keychain — a laptop usually has only a development identity — manual
// signing would fail at codesign, so automatic signing stays.
let profileNames = {};
let manualArchive = false;
const identity = hasApiKey ? distributionIdentity() : undefined;
if (identity) {
  try {
    const profiles = await ensureProfiles({
      keyId,
      issuerId,
      privateKey: readFileSync(keyPath, 'utf8'),
      bundleIds: [appBundleId, shareBundleId, notifyBundleId],
      log: (message) => console.log(message),
    });
    for (const [id, profile] of Object.entries(profiles)) {
      installProfile(profile);
      console.log(`· ${id} → ${profile.name}`);
    }
    profileNames = Object.fromEntries(Object.entries(profiles).map(([id, p]) => [id, p.name]));
    const stamp = stampManualSigning(readFileSync(pbxproj, 'utf8'), { teamId, profileNames, identity });
    writeFileSync(pbxproj, stamp.text);
    manualArchive = stamp.stamped.length > 0;
    console.log(`· ${identity} signing for ${stamp.stamped.join(', ')}`);
  } catch (error) {
    // Never fatal: an expired key or a portal outage falls back to the signing
    // that has always worked, instead of failing a release outright.
    console.log(`· ${error.message}\n· signing automatically instead`);
  }
}

// `ios/` is gitignored and this plist is a pure function of the team and the
// resolved profiles, so it is rewritten every run instead of trusted once written:
// a stale one signs a new archive with yesterday's decision. It is written HERE,
// after the profiles above are resolved, so export reuses exactly what the archive
// was signed with.
const signing = signingPlan({ bundleIds: [appBundleId, shareBundleId, notifyBundleId], profileNames });
if (signing.unnamed.length > 0) {
  console.log(`· no profile for ${signing.unnamed.join(', ')} — exporting with automatic signing`);
}
writeFileSync(exportOptions, exportOptionsPlist({ teamId, ...signing }));
console.log(`· wrote ${exportOptions} (${signing.signingStyle} signing)`);

const archiveArgs = [
  '-project',
  'App.xcodeproj',
  '-scheme',
  'App',
  '-configuration',
  'Release',
  '-destination',
  'generic/platform=iOS',
  '-archivePath',
  archivePath,
  // Nothing left to resolve once every bundle is signed by hand — and nothing
  // that could mint another certificate.
  ...(manualArchive ? [] : ['-allowProvisioningUpdates']),
  `DEVELOPMENT_TEAM=${teamId}`,
  `MARKETING_VERSION=${marketingVersion}`,
  `CURRENT_PROJECT_VERSION=${buildNumber}`,
  'archive',
];
const authenticationArgs = hasApiKey
  ? [
      '-authenticationKeyPath',
      resolve(keyPath),
      '-authenticationKeyID',
      keyId,
      '-authenticationKeyIssuerID',
      issuerId,
    ]
  : [];
if (hasApiKey && archiveArgs.includes('-allowProvisioningUpdates')) {
  archiveArgs.splice(archiveArgs.indexOf('-allowProvisioningUpdates') + 1, 0, ...authenticationArgs);
}
run('xcodebuild', archiveArgs, { cwd: projectDir });

run(
  'xcodebuild',
  exportArchiveArgs({
    archivePath,
    exportOptions,
    ipaDir,
    hasApiKey,
    signingStyle: signing.signingStyle,
    authenticationArgs,
  }),
  { cwd: projectDir },
);

const ipa = capture('sh', ['-c', `ls ${JSON.stringify(ipaDir)}/*.ipa 2>/dev/null | head -1`]);
if (!ipa) die(`export produced no .ipa in ${ipaDir}`);
console.log(`\n· ipa ${ipa}`);

if (has('no-upload')) {
  console.log(`\n✓ stopped before upload (--no-upload). The signed .ipa is ${ipa}`);
  process.exit(0);
}

if (hasApiKey) {
  // `--wait` makes altool report App Store processing failures (missing privacy
  // declarations, invalid entitlements, and similar) instead of returning a false
  // green as soon as the bytes arrive.
  run('xcrun', ['altool', '--upload-package', ipa, '--wait', '--api-key', keyId, '--api-issuer', issuerId], {
    env: { ...process.env, API_PRIVATE_KEYS_DIR: dirname(resolve(keyPath)) },
  });
} else if (secret('VIS_ASC_APPLE_ID', 'apple_id') && secret('VIS_ASC_APP_PASSWORD', 'app_password')) {
  run(
    'xcrun',
    ['altool', '--upload-package', ipa, '--wait', '-u', secret('VIS_ASC_APPLE_ID', 'apple_id'), '-p', '@env:VIS_ASC_APP_PASSWORD'],
    // Through the environment, never argv: an app-specific password on a command
    // line is readable by every process on the machine.
    { env: { ...process.env, VIS_ASC_APP_PASSWORD: secret('VIS_ASC_APP_PASSWORD', 'app_password') } },
  );
} else {
  // No credentials in the environment: re-export with destination=upload, which
  // authenticates as the Apple account signed into Xcode (Xcode > Settings >
  // Accounts) — the same one that owns the cloud-managed distribution cert.
  const uploadOptions = join(outDir, `UploadOptions-${buildNumber}.plist`);
  writeFileSync(uploadOptions, readFileSync(exportOptions, 'utf8').replace('<string>export</string>', '<string>upload</string>'));
  run(
    'xcodebuild',
    [
      '-exportArchive',
      '-archivePath',
      archivePath,
      '-exportOptionsPlist',
      uploadOptions,
      '-exportPath',
      join(ipaDir, 'upload'),
      '-allowProvisioningUpdates',
    ],
    { cwd: projectDir },
  );
}

console.log(
  `\n✓ uploaded and processed ${marketingVersion} (${buildNumber}) in App Store Connect.\n`,
);

// TestFlight shows "What to Test" to every tester on the update card, so the changelog
// must ride along with the build. The build only exists in App Store Connect AFTER
// processing, hence the poll inside publishNotes.
// The one command that re-pushes notes for a build already in App Store Connect.
const notesRecovery = `npm run release:notes -- --build ${buildNumber}`;
/**
 * Publish the notes, and answer whether App Store Connect still owes the build a
 * "What to Test": false ONLY when Apple was asked and would not take them, so a caller can
 * fail the release on that without failing an upload that never had an API key to ask with.
 */
const publishWhatToTest = async (timeoutMs) => {
  if (has('no-notes')) return true;
  if (!notes.text) {
    console.log('· no release notes to publish (nothing quotable since the last entry)');
    return true;
  }
  if (!keyId || !issuerId || !(keyPem || process.env.VIS_ASC_KEY_PATH)) {
    // An Apple-ID/app-password upload (the branch above) has no API to publish notes with,
    // so this is a credential fact, not a failed publish — reportable, never fatal.
    console.log(
      '· no App Store Connect API key — What to Test not published.\n' +
        `  Notes are in ${join(appDir, 'CHANGELOG.md')}; paste them into TestFlight ▸ the build ▸ What to Test,\n` +
        `  or store a key (\`npm run secrets\`) and run \`${notesRecovery}\`.`,
    );
    return true;
  }

  const res = await publishNotes({
    keyId,
    issuerId,
    keyPem: keyPem ?? readFileSync(process.env.VIS_ASC_KEY_PATH, 'utf8'),
    bundleId,
    version: marketingVersion,
    build: buildNumber,
    notes: notes.text,
    timeoutMs,
    log: (m) => console.log(`· ${m}`),
  });
  if (res.ok) console.log(`\n✓ TestFlight "What to Test" set for build ${buildNumber}.\n`);
  else console.log(`\n! notes not published: ${res.reason}\n  They are in CHANGELOG.md — re-push them with: ${notesRecovery}\n`);
  return res.ok;
};

// A build whose testers see no "What to Test" is an unfinished release, not a warning: the
// public branch below has always said so, while this one printed a line that scrolls past in
// a build log — which is how 0.1.35 (4075) shipped with its notes stranded. Same rule now,
// and `asc` has already retried everything transient by the time we get here.
if (!plan.isPublic) {
  const notesOk = await publishWhatToTest(Number(flag('notes-timeout') ?? 15 * 60 * 1000));
  if (!notesOk) die(`TestFlight release notes were not published for build ${buildNumber}.\nThe build is uploaded; recover with: ${notesRecovery}`);
}

// PUBLIC TestFlight, by default. An upload only reaches the internal groups, so stopping here
// leaves the public link serving whatever it last got — the iOS half of the split that had Play
// on 4090 everywhere while the public TestFlight link still handed out 4042. `--audience
// internal` is the opt-out; everything this needs is already in scope, so it is one call, not a
// second pipeline.
if (plan.isPublic) {
  const res = await distribute({
    keyId,
    issuerId,
    keyPem: keyPem ?? (process.env.VIS_ASC_KEY_PATH ? readFileSync(process.env.VIS_ASC_KEY_PATH, 'utf8') : undefined),
    bundleId,
    build: buildNumber,
    group: plan.group,
    review: plan.review,
    timeoutMs: Number(flag('public-timeout') ?? 60 * 60 * 1000),
  });
  if (!res.ok) {
    die(`public distribution incomplete: ${res.reason}\nThe build is uploaded; recover with: npm run release:testflight -- --build ${buildNumber}`);
  }
  console.log(`\n✓ build ${buildNumber} is with public TestFlight.${res.publicLink ? `\n  Join link: ${res.publicLink}` : ''}\n`);

  const notesOk = await publishWhatToTest(Number(flag('notes-timeout') ?? 2 * 60 * 1000));
  if (!notesOk) {
    die(`TestFlight release notes were not published for build ${buildNumber}.\nThe build is uploaded; recover with: ${notesRecovery}`);
  }
}

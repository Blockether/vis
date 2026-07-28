#!/usr/bin/env node
/**
 * One command: web bundle -> Capacitor sync -> signed archive -> .ipa -> TestFlight.
 *
 * Versioning has no hand-edited state. The iOS project's own MARKETING_VERSION /
 * CURRENT_PROJECT_VERSION are IGNORED and passed as build settings instead:
 *
 *   CFBundleShortVersionString = package.json "version"   (the release users see)
 *   CFBundleVersion            = `git rev-list --count HEAD`  (strictly monotonic)
 *
 * so `ios/` stays a disposable, regenerable Capacitor output (it is gitignored)
 * and two uploads can never collide on a build number.
 *
 * Usage:
 *   npm run release:ios                 # build + archive + export + upload
 *   npm run release:ios -- --no-upload  # stop at the .ipa (no App Store Connect call)
 *   npm run release:ios -- --version 1.2.0 --build 4711
 *   npm run release:ios -- --skip-web   # reuse dist/ and the last `cap sync`
 *   npm run release:ios -- --prepare    # web + cap sync + stamp versions, then STOP
 *   npm run release:ios -- --prepare --dev  # same, but aps-environment=development
 *                                       # (archive by hand in Xcode: Product > Archive)
 *   npm run release:ios -- --public     # …and hand it to PUBLIC TestFlight afterwards:
 *                                       # beta review + public-link group (see testflight.mjs)
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
import { distribute } from './testflight.mjs';

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
const iosDir = join(appDir, 'ios');
const projectDir = join(iosDir, 'App');
const exportOptions = join(iosDir, 'ExportOptions.plist');
const teamId = secret('VIS_IOS_TEAM_ID', 'team_id') ?? 'JSZTFUBUBB';

const args = process.argv.slice(2);
const flag = (name) => {
  const i = args.indexOf(`--${name}`);
  return i === -1 ? undefined : args[i + 1];
};
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

const pkg = JSON.parse(readFileSync(join(appDir, 'package.json'), 'utf8'));
const marketingVersion = flag('version') ?? pkg.version;
const buildNumber = flag('build') ?? capture('git', ['rev-list', '--count', 'HEAD']);
if (!/^\d+(\.\d+)*$/.test(marketingVersion)) die(`bad --version "${marketingVersion}"`);
if (!/^\d+$/.test(buildNumber)) die(`bad --build "${buildNumber}" (git rev-list unavailable?)`);

if (!existsSync(projectDir)) die(`no iOS project at ${projectDir} — run \`npm run add:ios\` first`);

// ios/ is gitignored, so a freshly generated project has no export options.
if (!existsSync(exportOptions)) {
  writeFileSync(
    exportOptions,
    `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
\t<key>method</key>
\t<string>app-store-connect</string>
\t<key>destination</key>
\t<string>export</string>
\t<key>teamID</key>
\t<string>${teamId}</string>
\t<key>signingStyle</key>
\t<string>automatic</string>
\t<key>stripSwiftSymbols</key>
\t<true/>
\t<key>uploadSymbols</key>
\t<true/>
</dict>
</plist>
`,
  );
  console.log(`· wrote ${exportOptions}`);
}

const outDir = join(appDir, 'build', 'ios');
const archivePath = join(outDir, `Vis-${marketingVersion}-${buildNumber}.xcarchive`);
const ipaDir = join(outDir, `export-${marketingVersion}-${buildNumber}`);
mkdirSync(outDir, { recursive: true });
rmSync(archivePath, { recursive: true, force: true });
rmSync(ipaDir, { recursive: true, force: true });

console.log(
  `\nVis Companion → TestFlight\n  version ${marketingVersion} (build ${buildNumber})\n  team    ${teamId}\n  archive ${archivePath}`,
);

// Generated BEFORE the archive so a bad/empty changelog fails fast rather than after a
// 10-minute build, and so `--prepare` / `--no-upload` still show what testers would read.
// `--no-changelog` keeps CHANGELOG.md untouched (dry runs, re-exports of the same build).
const notes = has('no-notes')
  ? { text: '', bullets: [] }
  : buildNotes({
      version: marketingVersion,
      build: buildNumber,
      scope: flag('notes-scope') ? [flag('notes-scope')] : [],
      write: !has('no-changelog'),
    });
if (notes.text) console.log(`\nWhat to Test${notes.reused ? ' (from CHANGELOG.md)' : ''}:\n${notes.text}\n`);

if (!has('skip-web')) {
  run('npm', ['run', 'build']);
  run('npx', ['cap', 'sync', 'ios']);
}

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
</dict>
</plist>
`;
if (!existsSync(entitlementsPath) || readFileSync(entitlementsPath, 'utf8') !== entitlements) {
  writeFileSync(entitlementsPath, entitlements);
  console.log(`· wrote ${entitlementsPath} (aps-environment ${apsEnvironment})`);
}

// Point every build configuration of the App target at it. Without this the
// archive is signed with no `aps-environment` and APNs rejects every token.
let project = readFileSync(pbxproj, 'utf8');
if (!project.includes('CODE_SIGN_ENTITLEMENTS')) {
  project = project.replaceAll(
    /(\n(\s*)PRODUCT_BUNDLE_IDENTIFIER = [^;]+;)/g,
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
  '-allowProvisioningUpdates',
  `DEVELOPMENT_TEAM=${teamId}`,
  `MARKETING_VERSION=${marketingVersion}`,
  `CURRENT_PROJECT_VERSION=${buildNumber}`,
  'archive',
];
if (hasApiKey) {
  archiveArgs.splice(
    archiveArgs.indexOf('-allowProvisioningUpdates') + 1,
    0,
    '-authenticationKeyPath',
    resolve(keyPath),
    '-authenticationKeyID',
    keyId,
    '-authenticationKeyIssuerID',
    issuerId,
  );
}
run('xcodebuild', archiveArgs, { cwd: projectDir });

run(
  'xcodebuild',
  ['-exportArchive', '-archivePath', archivePath, '-exportOptionsPlist', exportOptions, '-exportPath', ipaDir, '-allowProvisioningUpdates'],
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
  // Explicit API key: works headless / on CI, never prompts for 2FA.
  run('xcrun', ['altool', '--upload-app', '-t', 'ios', '-f', ipa, '--apiKey', keyId, '--apiIssuer', issuerId], {
    env: { ...process.env, API_PRIVATE_KEYS_DIR: dirname(resolve(keyPath)) },
  });
} else if (secret('VIS_ASC_APPLE_ID', 'apple_id') && secret('VIS_ASC_APP_PASSWORD', 'app_password')) {
  run(
    'xcrun',
    ['altool', '--upload-app', '-t', 'ios', '-f', ipa, '-u', secret('VIS_ASC_APPLE_ID', 'apple_id'), '-p', '@env:VIS_ASC_APP_PASSWORD'],
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
  `\n✓ uploaded ${marketingVersion} (${buildNumber}) to App Store Connect.\n` +
    '  Processing takes a few minutes; then it appears in TestFlight.\n',
);

// TestFlight shows "What to Test" to every tester on the update card, so the changelog
// must ride along with the build. The build only exists in App Store Connect AFTER
// processing, hence the poll inside publishNotes.
if (!has('no-notes')) {
  if (!notes.text) {
    console.log('· no release notes to publish (nothing quotable since the last entry)');
  } else if (!keyId || !issuerId || !(keyPem || process.env.VIS_ASC_KEY_PATH)) {
    console.log(
      '· no App Store Connect API key — What to Test not published.\n' +
        `  Notes are in ${join(appDir, 'CHANGELOG.md')}; paste them into TestFlight ▸ the build ▸ What to Test,\n` +
        '  or store a key (`npm run secrets`) and run `npm run release:notes`.',
    );
  } else {
    const res = await publishNotes({
      keyId,
      issuerId,
      keyPem: keyPem ?? readFileSync(process.env.VIS_ASC_KEY_PATH, 'utf8'),
      bundleId,
      version: marketingVersion,
      build: buildNumber,
      notes: notes.text,
      timeoutMs: Number(flag('notes-timeout') ?? 15 * 60 * 1000),
      log: (m) => console.log(`· ${m}`),
    });
    if (res.ok) console.log(`\n✓ TestFlight "What to Test" set for build ${buildNumber}.\n`);
    else console.log(`\n! notes not published: ${res.reason}\n  They are in CHANGELOG.md — paste them in App Store Connect.\n`);
  }
}

// PUBLIC TestFlight. Off by default because it costs a Beta App Review round trip and
// exposes the build outside the team — both things a routine internal upload must not do
// implicitly. `--public` (or `--group <name>`) opts in; everything it needs is already in
// scope, so this is one call, not a second pipeline.
if (has('public') || flag('group')) {
  const res = await distribute({
    keyId,
    issuerId,
    keyPem: keyPem ?? (process.env.VIS_ASC_KEY_PATH ? readFileSync(process.env.VIS_ASC_KEY_PATH, 'utf8') : undefined),
    bundleId,
    build: buildNumber,
    group: flag('group') ?? 'Public',
    review: !has('no-review'),
    timeoutMs: Number(flag('public-timeout') ?? 30 * 60 * 1000),
  });
  if (res.ok) {
    console.log(`\n✓ build ${buildNumber} is with public TestFlight.${res.publicLink ? `\n  Join link: ${res.publicLink}` : ''}\n`);
  } else {
    console.log(`\n! public distribution incomplete: ${res.reason}\n  The build IS uploaded; re-run just this step with:  npm run release:testflight -- --build ${buildNumber}\n`);
  }
}

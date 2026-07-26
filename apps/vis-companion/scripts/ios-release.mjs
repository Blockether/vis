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
import { existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const iosDir = join(appDir, 'ios');
const projectDir = join(iosDir, 'App');
const exportOptions = join(iosDir, 'ExportOptions.plist');
const teamId = process.env.VIS_IOS_TEAM_ID ?? 'JSZTFUBUBB';

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

if (!has('skip-web')) {
  run('npm', ['run', 'build']);
  run('npx', ['cap', 'sync', 'ios']);
}

// App Store Connect API key: also lets xcodebuild create signing assets itself.
const keyId = process.env.VIS_ASC_KEY_ID;
const issuerId = process.env.VIS_ASC_ISSUER_ID;
const keyPath = process.env.VIS_ASC_KEY_PATH;
const hasApiKey = Boolean(keyId && issuerId && keyPath);
if (hasApiKey && !existsSync(keyPath)) die(`VIS_ASC_KEY_PATH does not exist: ${keyPath}`);

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
  ['-exportArchive', '-archivePath', archivePath, '-exportOptionsPlist', exportOptions, '-exportPath', ipaDir],
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
} else if (process.env.VIS_ASC_APPLE_ID && process.env.VIS_ASC_APP_PASSWORD) {
  run('xcrun', [
    'altool',
    '--upload-app',
    '-t',
    'ios',
    '-f',
    ipa,
    '-u',
    process.env.VIS_ASC_APPLE_ID,
    '-p',
    '@env:VIS_ASC_APP_PASSWORD',
  ]);
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

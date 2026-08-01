#!/usr/bin/env node
/**
 * Keep the generated `ios/` project on STOCK Capacitor.
 *
 * This used to install a tracked `CAPBridgeViewController` subclass
 * (`native/ios/VisBridgeViewController.swift`) that pushed UIKit's view size into
 * the web layer. That is gone: the app rides the plain Capacitor host, and a
 * rotation is handled where it is cheap — the web layer simply stops measuring
 * for the length of the flip (`src/lib/viewport.ts`, and the `ResizeObserver`s in
 * `SessionScreen`/`ChatContent` that skip while it is open).
 *
 * Deleting the Swift source is not enough on its own. `ios/` is gitignored but it
 * is NOT regenerated on a machine that already has it: an existing checkout still
 * carries the stamped class in `AppDelegate.swift` and a `Main.storyboard` whose
 * root controller is `customClass="VisBridgeViewController"`. Leaving that behind
 * means the storyboard names a class that no longer exists — a crash on launch.
 *
 * So the same hook now UN-stamps: drop the marker block, put
 * `CAPBridgeViewController` back. Idempotent, and a no-op on a fresh project.
 *
 * Usage:
 *   node scripts/ios-prepare.mjs
 *   node scripts/ios-prepare.mjs --check   # exit 1 if the project is still stamped
 */
import { copyFileSync, existsSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const check = process.argv.slice(2).includes('--check');

const BEGIN = '// vis:viewport-bridge:begin';
const CUSTOM_CLASS = 'VisBridgeViewController';

const appDir = join(root, 'ios', 'App', 'App');
const delegate = join(appDir, 'AppDelegate.swift');
const storyboard = join(appDir, 'Base.lproj', 'Main.storyboard');
const infoPlist = join(appDir, 'Info.plist');
const bundleId = JSON.parse(readFileSync(join(root, 'capacitor.config.json'), 'utf8')).appId;

const appIconSource = join(root, 'native-assets', 'ios', 'AppIcon-512@2x.png');
const appIconTarget = join(appDir, 'Assets.xcassets', 'AppIcon.appiconset', 'AppIcon-512@2x.png');
const appIcon = readFileSync(appIconSource);
const appIconOk = existsSync(appIconTarget) && readFileSync(appIconTarget).equals(appIcon);

const die = (msg) => {
  console.error(`\n\u2717 ${msg}\n`);
  process.exit(1);
};

// A machine that only builds Android (or web) has no ios/ at all; that is not a
// failure, it just has nothing to clean.
if (!existsSync(delegate)) {
  if (check) die('no ios/App/App/AppDelegate.swift — run `npm run add:ios` first');
  console.log('\u00b7 ios: no ios/ project — nothing to clean');
  process.exit(0);
}

// ── 1. the appended bridge controller, if this checkout still has one ─────────

const currentDelegate = readFileSync(delegate, 'utf8');
const cleanedDelegate = currentDelegate.includes(BEGIN)
  ? `${currentDelegate.slice(0, currentDelegate.indexOf(BEGIN)).trimEnd()}\n`
  : currentDelegate;
const delegateOk = cleanedDelegate === currentDelegate;

// ── 2. the storyboard, back to Capacitor's own controller ────────────────────

const currentBoard = readFileSync(storyboard, 'utf8');
const cleanedBoard = currentBoard.replace(
  /customClass="VisBridgeViewController" customModule="App" customModuleProvider="target"/,
  'customClass="CAPBridgeViewController" customModule="Capacitor"',
);
const boardOk = !currentBoard.includes(CUSTOM_CLASS);
if (!boardOk && cleanedBoard === currentBoard) {
  die(`Main.storyboard references ${CUSTOM_CLASS} in a shape this script cannot rewrite`);
}

// ── 3. capabilities that Capacitor's generated Info.plist does not carry ──────
//
// `ios/` is gitignored and CI creates it from scratch. Every runtime-sensitive
// plist entry therefore belongs here, not in a hand-edited local Xcode project.
const plistEntries = [
  [
    'UIBackgroundModes',
    `\t<key>UIBackgroundModes</key>
\t<array>
\t\t<string>audio</string>
\t</array>`,
  ],
  [
    'NSAppTransportSecurity',
    `\t<key>NSAppTransportSecurity</key>
\t<dict>
\t\t<key>NSAllowsArbitraryLoadsInWebContent</key>
\t\t<true/>
\t\t<key>NSAllowsLocalNetworking</key>
\t\t<true/>
\t</dict>`,
  ],
  [
    'NSLocalNetworkUsageDescription',
    `\t<key>NSLocalNetworkUsageDescription</key>
\t<string>Vis connects to your gateway running on your local network, Tailscale, or a tunnel.</string>`,
  ],
  [
    'NSCameraUsageDescription',
    `\t<key>NSCameraUsageDescription</key>
\t<string>Scan a gateway pairing QR code to connect Vis.</string>`,
  ],
  [
    'NSMicrophoneUsageDescription',
    `\t<key>NSMicrophoneUsageDescription</key>
\t<string>Dictate messages to Vis by voice.</string>`,
  ],
  [
    'NSPhotoLibraryUsageDescription',
    `\t<key>NSPhotoLibraryUsageDescription</key>
\t<string>Attach images from your photo library to a Vis conversation.</string>`,
  ],
  [
    'NSPhotoLibraryAddUsageDescription',
    `\t<key>NSPhotoLibraryAddUsageDescription</key>
\t<string>Save images shared from a Vis conversation.</string>`,
  ],
  [
    'ITSAppUsesNonExemptEncryption',
    `\t<key>ITSAppUsesNonExemptEncryption</key>
\t<false/>`,
  ],
  [
    'CFBundleURLTypes',
    `\t<key>CFBundleURLTypes</key>
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
\t</array>`,
  ],
];

const currentPlist = readFileSync(infoPlist, 'utf8');
const missingPlistEntries = plistEntries.filter(([key]) => !currentPlist.includes(`<key>${key}</key>`));
const plistOk = missingPlistEntries.length === 0;
let preparedPlist = currentPlist;
if (!plistOk) {
  const at = preparedPlist.lastIndexOf('</dict>');
  if (at < 0) die('Info.plist has no root </dict>');
  const additions = `${missingPlistEntries.map(([, xml]) => xml).join('\n')}\n`;
  preparedPlist = preparedPlist.slice(0, at) + additions + preparedPlist.slice(at);
}

if (check) {
  if (delegateOk && boardOk && plistOk && appIconOk) {
    console.log('· ios: stock Capacitor host with required app capabilities and branded icon');
    process.exit(0);
  }
  const missing = missingPlistEntries.map(([key]) => key).join(', ');
  die(
    !appIconOk
      ? 'ios: generated AppIcon is not the tracked Vis icon — run `node scripts/ios-prepare.mjs`'
      : !delegateOk || !boardOk
        ? 'ios: stale viewport bridge — run `node scripts/ios-prepare.mjs`'
        : `ios: Info.plist is missing ${missing} — run \`node scripts/ios-prepare.mjs\``,
  );
}

if (!appIconOk) copyFileSync(appIconSource, appIconTarget);

if (!delegateOk) writeFileSync(delegate, cleanedDelegate);
if (!boardOk) writeFileSync(storyboard, cleanedBoard);
if (!plistOk) writeFileSync(infoPlist, preparedPlist);
console.log(
  `· ios: ${delegateOk && boardOk ? 'stock Capacitor host' : 'removed the viewport bridge'}; ${
    plistOk ? 'app capabilities already present' : `stamped ${missingPlistEntries.map(([key]) => key).join(', ')}`
  }; ${appIconOk ? 'branded icon already present' : 'stamped branded app icon'}`,
);

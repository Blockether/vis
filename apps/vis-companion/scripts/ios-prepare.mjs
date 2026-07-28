#!/usr/bin/env node
/**
 * Re-install the native viewport bridge into the generated `ios/` project.
 *
 * `ios/` is gitignored: Capacitor recreates it from its own template with
 * `cap add ios`, so anything hand-edited there is lost on a fresh clone and on
 * every regeneration — silently, with the app still building. The one native
 * customisation this product needs is `native/ios/VisBridgeViewController.swift`
 * (tracked): a `CAPBridgeViewController` subclass that reports UIKit's real view
 * size to the web layer, because WKWebView's own layout viewport is stale for
 * frames after a rotation and indefinitely after a resume. See that file and
 * `src/lib/native-viewport.ts`.
 *
 * It is appended to the template's `AppDelegate.swift` rather than added as a
 * new file on purpose: a new source file would have to be registered in
 * `App.xcodeproj/project.pbxproj`, which is generated too, and patching that is
 * far more fragile than appending to a file the template already compiles.
 *
 * Both steps are idempotent and marker-guarded, so running this twice, or after
 * `cap sync`, changes nothing.
 *
 * Usage:
 *   node scripts/ios-prepare.mjs
 *   node scripts/ios-prepare.mjs --check   # exit 1 if the project is not stamped
 */
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const check = process.argv.slice(2).includes('--check');

const BEGIN = '// vis:viewport-bridge:begin — installed by scripts/ios-prepare.mjs';
const END = '// vis:viewport-bridge:end';

const appDir = join(root, 'ios', 'App', 'App');
const delegate = join(appDir, 'AppDelegate.swift');
const storyboard = join(appDir, 'Base.lproj', 'Main.storyboard');
const source = join(root, 'native', 'ios', 'VisBridgeViewController.swift');

const die = (msg) => {
  console.error(`\n\u2717 ${msg}\n`);
  process.exit(1);
};

// A machine that only builds Android (or web) has no ios/ at all; that is not a
// failure, it just has nothing to stamp.
if (!existsSync(delegate)) {
  if (check) die('no ios/App/App/AppDelegate.swift — run `npm run add:ios` first');
  console.log('\u00b7 ios: no ios/ project — nothing to install');
  process.exit(0);
}
if (!existsSync(source)) die(`missing tracked native source: ${source}`);

// ── 1. the bridge controller, appended between markers ────────────────────────

const swift = readFileSync(source, 'utf8').trim();
const current = readFileSync(delegate, 'utf8');
// Drop any previously stamped block, so an edited source replaces it instead of
// stacking a second copy of the class (which would not compile).
const stripped = current.includes(BEGIN)
  ? `${current.slice(0, current.indexOf(BEGIN)).trimEnd()}\n`
  : `${current.trimEnd()}\n`;
const stamped = `${stripped}\n${BEGIN}\n\n${swift}\n\n${END}\n`;
const delegateOk = current === stamped;

// ── 2. the storyboard pointing at it ──────────────────────────────────────────

const board = readFileSync(storyboard, 'utf8');
const wired = board.replace(
  /customClass="CAPBridgeViewController" customModule="Capacitor"/,
  'customClass="VisBridgeViewController" customModule="App" customModuleProvider="target"',
);
const boardOk = board.includes('customClass="VisBridgeViewController"');
if (!boardOk && wired === board) {
  die('Main.storyboard has neither CAPBridgeViewController nor VisBridgeViewController');
}

if (check) {
  if (delegateOk && boardOk) {
    console.log('\u00b7 ios: viewport bridge installed');
    process.exit(0);
  }
  die('ios: viewport bridge missing — run `node scripts/ios-prepare.mjs`');
}

if (!delegateOk) writeFileSync(delegate, stamped);
if (!boardOk) writeFileSync(storyboard, wired);
console.log(
  `\u00b7 ios: viewport bridge ${delegateOk && boardOk ? 'already installed' : 'installed'} (AppDelegate.swift + Main.storyboard)`,
);

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
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const check = process.argv.slice(2).includes('--check');

const BEGIN = '// vis:viewport-bridge:begin';
const CUSTOM_CLASS = 'VisBridgeViewController';

const appDir = join(root, 'ios', 'App', 'App');
const delegate = join(appDir, 'AppDelegate.swift');
const storyboard = join(appDir, 'Base.lproj', 'Main.storyboard');

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

if (check) {
  if (delegateOk && boardOk) {
    console.log('\u00b7 ios: stock Capacitor host');
    process.exit(0);
  }
  die('ios: stale viewport bridge — run `node scripts/ios-prepare.mjs`');
}

if (!delegateOk) writeFileSync(delegate, cleanedDelegate);
if (!boardOk) writeFileSync(storyboard, cleanedBoard);
console.log(
  `\u00b7 ios: ${delegateOk && boardOk ? 'already stock Capacitor' : 'removed the viewport bridge (AppDelegate.swift + Main.storyboard)'}`,
);

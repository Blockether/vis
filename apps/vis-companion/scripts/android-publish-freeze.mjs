#!/usr/bin/env node
/**
 * The single switch that stops every Android publication — locally AND in CI.
 *
 * The .aab now with Google Play review is the exact build testers were asked to judge, so
 * while that window is open nothing newer may reach a Play track or Firebase App
 * Distribution: a fresh beta would replace the very artefact under test. iOS is untouched
 * and keeps shipping on its own.
 *
 * Only STORE WRITES are refused. Everything that builds or reads stays available —
 * `release:android:store -- --no-upload`, `-- --tracks`, and the CI jobs that assemble and
 * verify the app — so Android never stops being built while it may not be shipped.
 *
 * Lifting is ONE edit here: `isFrozen: false`. Every path reads this file, so nothing else
 * has to be remembered:
 *   scripts/android-release.mjs               refuses before the build (local and CI upload)
 *   scripts/mobile-release.mjs                names the stores a companion tag will reach
 *   .github/workflows/mobile-release.yml      skips the whole `android` job
 *   .github/workflows/android-companion.yml   skips Firebase App Distribution
 *
 * CLI — the CI gate:
 *   node scripts/android-publish-freeze.mjs                  # print the state
 *   node scripts/android-publish-freeze.mjs --github-output  # android=frozen|allowed
 * It always exits 0: a frozen store is a SKIPPED job, never a red run.
 */
import { appendFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

export const ANDROID_PUBLISH_FREEZE = {
  isFrozen: false,
  since: '2026-08-17',
  reason:
    'the submitted Android build is with review so that testers judge THAT artefact, and publishing ' +
    'a beta over it would replace what they were asked to test',
  liftedBy:
    'the release owner, by word — at least 7 days, and the freeze is not a timer that expires on its own',
};

/** `frozen` or `allowed` — the whole state, in the one word a CI job can branch on. */
export const freezeState = (freeze = ANDROID_PUBLISH_FREEZE) => (freeze.isFrozen ? 'frozen' : 'allowed');

/** The refusal a caller should print, or `undefined` when publishing is allowed. */
export const androidPublishRefusal = (what = 'Android publishing', freeze = ANDROID_PUBLISH_FREEZE) =>
  freeze.isFrozen
    ? `${what} is frozen: ${freeze.reason}.\n` +
      `  Lifted by: ${freeze.liftedBy}.\n` +
      '  One switch: apps/vis-companion/scripts/android-publish-freeze.mjs (iOS is unaffected).'
    : undefined;

/** Throwing form for callers that are not a CLI. */
export const assertAndroidPublishAllowed = (what, freeze = ANDROID_PUBLISH_FREEZE) => {
  const refusal = androidPublishRefusal(what, freeze);
  if (refusal) throw new Error(refusal);
};

const invokedDirectly = process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (invokedDirectly) {
  const state = freezeState();
  if (process.argv.includes('--github-output') && process.env.GITHUB_OUTPUT) {
    appendFileSync(process.env.GITHUB_OUTPUT, `android=${state}\n`);
  }
  const line =
    state === 'frozen'
      ? `Android publishing is FROZEN — ${ANDROID_PUBLISH_FREEZE.reason}. iOS still ships.`
      : 'Android publishing is allowed.';
  console.log(process.env.GITHUB_ACTIONS ? `::notice::${line}` : line);
}

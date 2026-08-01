#!/usr/bin/env node
/**
 * Trigger a two-store companion release without inventing an app version.
 *
 * VIS_VERSION is always the marketing version. A regular `vX.Y.Z` tag releases
 * that exact commit automatically. App-only follow-ups keep X.Y.Z and move the
 * single `companion-vX.Y.Z` tag to a newer main commit; the tag push runs the
 * same mobile-release workflow again with a new git-derived store build number.
 */
import { spawnSync } from 'node:child_process';

import { repoRoot, visVersion } from './version.mjs';

const args = process.argv.slice(2);
const dryRun = args.includes('--dry-run');
const unknown = args.filter((arg) => arg !== '--dry-run');

function die(message) {
  console.error(`\nERROR: ${message}\n`);
  process.exit(1);
}

function command(cmd, cmdArgs, { allowFailure = false } = {}) {
  const result = spawnSync(cmd, cmdArgs, { cwd: repoRoot, encoding: 'utf8' });
  if (result.status !== 0 && !allowFailure) {
    const detail = result.stderr.trim() || result.stdout.trim() || `exit ${result.status}`;
    die(`${cmd} ${cmdArgs.join(' ')} failed: ${detail}`);
  }
  return result;
}

function capture(cmd, cmdArgs) {
  return command(cmd, cmdArgs).stdout.trim();
}

if (unknown.length > 0) die(`unknown argument(s): ${unknown.join(', ')}`);
if (capture('git', ['status', '--porcelain'])) {
  die('the working tree is not clean; commit and push the exact release first');
}
if (capture('git', ['branch', '--show-current']) !== 'main') {
  die('mobile releases must be cut from main');
}

command('git', ['fetch', '--force', '--tags', 'origin', 'main']);

const head = capture('git', ['rev-parse', 'HEAD']);
const remoteMain = capture('git', ['rev-parse', 'origin/main']);
if (head !== remoteMain) die('local main must exactly match origin/main');

const version = visVersion();
const regularTag = `v${version}`;
const companionTag = `companion-v${version}`;
const regularResult = command('git', ['rev-parse', '--verify', `${regularTag}^{commit}`], {
  allowFailure: true,
});
if (regularResult.status !== 0) {
  die(`${regularTag} does not exist; release regular Vis ${version} before its companion`);
}
const regularCommit = regularResult.stdout.trim();
if (regularCommit === head) {
  console.log(`Vis version:      ${version}`);
  console.log(`Store build:      ${capture('git', ['rev-list', '--count', 'HEAD'])}`);
  console.log(`Regular release:  ${regularTag} already points at HEAD`);
  console.log('\n✓ No companion retag needed: the regular Vis release triggers both stores automatically.');
  process.exit(0);
}
if (command('git', ['merge-base', '--is-ancestor', regularCommit, head], { allowFailure: true }).status !== 0) {
  die(`${regularTag} is not an ancestor of HEAD; VIS_VERSION cannot describe this branch`);
}

const currentCompanion = command(
  'git',
  ['rev-parse', '--verify', `${companionTag}^{commit}`],
  { allowFailure: true },
).stdout.trim();
if (currentCompanion === head) {
  die(`${companionTag} already points at HEAD; commit a new build before releasing again`);
}

const build = capture('git', ['rev-list', '--count', 'HEAD']);
const remoteTagObject = command(
  'git',
  ['ls-remote', '--refs', 'origin', `refs/tags/${companionTag}`],
).stdout.trim().split(/\s+/)[0] || '';

console.log(`Vis version:      ${version}`);
console.log(`Store build:      ${build}`);
console.log(`Regular release:  ${regularTag} (${regularCommit.slice(0, 10)})`);
console.log(`Companion trigger:${currentCompanion ? ' retag' : ' create'} ${companionTag} -> ${head.slice(0, 10)}`);

if (dryRun) {
  console.log('\nDry run only; no tag changed.');
  process.exit(0);
}

command('git', [
  'tag',
  '--force',
  '--annotate',
  companionTag,
  head,
  '--message',
  `Release Vis Companion ${version} (${build})`,
]);
const pushArgs = ['push'];
if (remoteTagObject) {
  pushArgs.push(`--force-with-lease=refs/tags/${companionTag}:${remoteTagObject}`);
}
pushArgs.push('origin', `refs/tags/${companionTag}`);
command('git', pushArgs);

console.log(`\n✓ ${companionTag} now triggers iOS + Android from ${head.slice(0, 10)}.`);

#!/usr/bin/env node
/**
 * Trigger a two-store companion release without inventing an app version.
 *
 * VIS_VERSION is always the marketing version. A regular `vX.Y.Z` tag releases
 * that exact commit automatically. App-only follow-ups keep X.Y.Z and create an
 * immutable `companion-vX.Y.Z-build.N` tag, where N is the git-derived store build
 * number. Each tag push runs the same mobile-release workflow exactly once.
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
const build = capture('git', ['rev-list', '--count', 'HEAD']);
const companionTag = `companion-v${version}-build.${build}`;
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
  console.log('\n✓ No companion tag needed: the regular Vis release triggers both stores automatically.');
  process.exit(0);
}
if (command('git', ['merge-base', '--is-ancestor', regularCommit, head], { allowFailure: true }).status !== 0) {
  die(`${regularTag} is not an ancestor of HEAD; VIS_VERSION cannot describe this branch`);
}

const remoteTagObject = command(
  'git',
  ['ls-remote', '--refs', 'origin', `refs/tags/${companionTag}`],
).stdout.trim().split(/\s+/)[0] || '';
if (remoteTagObject) {
  die(`${companionTag} already exists on origin; companion release tags are immutable`);
}


console.log(`Vis version:      ${version}`);
console.log(`Store build:      ${build}`);
console.log(`Regular release:  ${regularTag} (${regularCommit.slice(0, 10)})`);
console.log(`Companion trigger: create ${companionTag} -> ${head.slice(0, 10)}`);

if (dryRun) {
  console.log('\nDry run only; no tag changed.');
  process.exit(0);
}

command('git', [
  'tag',
  '--annotate',
  companionTag,
  head,
  '--message',
  `Release Vis Companion ${version} (${build})`,
]);
command('git', ['push', 'origin', `refs/tags/${companionTag}`]);

console.log(`\n✓ ${companionTag} triggers iOS + Android from ${head.slice(0, 10)}.`);

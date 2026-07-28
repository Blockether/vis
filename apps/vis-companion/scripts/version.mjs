#!/usr/bin/env node
/**
 * ONE version for the whole product.
 *
 * The repo-root `VERSION` file is the single source of truth: `build.clj` reads it
 * for the CLI/native image (and CI bumps it on every tag), so the companion must
 * read the SAME file instead of carrying its own number. A drifting
 * `package.json` "version" is how the app ended up shipping 1.0.1 while the
 * gateway was 0.1.13 — the version-mismatch screen then fires on a build that is
 * perfectly in sync.
 *
 * `package.json` still needs a valid "version" (npm metadata, and vite injects it
 * as `__VIS_APP_VERSION__`), so it is a MIRROR that `syncPackageVersion()` stamps
 * from `VERSION`; every prepare/release/build entry point calls it first.
 *
 * Run directly to sync + print:  node scripts/version.mjs
 */
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const scriptsDir = dirname(fileURLToPath(import.meta.url));
export const appDir = join(scriptsDir, '..');
export const repoRoot = join(appDir, '..', '..');
export const versionFile = join(repoRoot, 'VERSION');
const packageFile = join(appDir, 'package.json');

/** The product version, straight from the repo-root VERSION file. */
export function visVersion() {
  const raw = readFileSync(versionFile, 'utf8').trim();
  if (!/^\d+\.\d+\.\d+(-[0-9A-Za-z.-]+)?$/.test(raw)) {
    throw new Error(`${versionFile} holds "${raw}", which is not a version`);
  }
  // Store-facing marketing versions are numeric-only; a `-SNAPSHOT`-style
  // qualifier is a local build, so drop it rather than fail the release.
  return raw.split('-')[0];
}

/** Mirror VERSION into package.json. Returns the version; logs only on a change. */
export function syncPackageVersion({ quiet = false } = {}) {
  const version = visVersion();
  const text = readFileSync(packageFile, 'utf8');
  const pkg = JSON.parse(text);
  if (pkg.version !== version) {
    const next = text.replace(/^(\s*"version":\s*)"[^"]*"/m, `$1"${version}"`);
    if (JSON.parse(next).version !== version) {
      throw new Error(`could not rewrite "version" in ${packageFile}`);
    }
    writeFileSync(packageFile, next);
    if (!quiet) console.log(`\u2713 package.json version ${pkg.version} \u2192 ${version} (from VERSION)`);
  }
  return version;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  console.log(syncPackageVersion());
}

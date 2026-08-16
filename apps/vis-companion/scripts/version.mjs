#!/usr/bin/env node
/**
 * ONE version for the whole product.
 *
 * The repo-root `VIS_VERSION` file is the single source of truth. A regular Vis
 * release tag must match it exactly, and the CLI/native image and companion app
 * all read that same value. App-only store rebuilds keep the marketing version
 * and get a new git-derived build number instead of inventing an app version.
 *
 * `package.json` and the root package in `package-lock.json` still need valid
 * npm versions because Vite injects the package value as `__VIS_APP_VERSION__`.
 * They are MIRRORS that `syncPackageVersion()` stamps from `VIS_VERSION`; every
 * prepare/release/build entry point calls it first.
 *
 * Run directly to sync + print:  node scripts/version.mjs
 */
import { readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const scriptsDir = dirname(fileURLToPath(import.meta.url));
export const appDir = join(scriptsDir, '..');
export const repoRoot = join(appDir, '..', '..');
export const visVersionFile = join(repoRoot, 'VIS_VERSION');
const packageFile = join(appDir, 'package.json');
const packageLockFile = join(appDir, 'package-lock.json');
// Both Python distributions, in dependency order: the declaration and the API that
// is written against it. `vis-agent` also pins `vis-contract==<version>`, so the
// same number appears a third time and is stamped here too.
const pyprojectFiles = [
  join(repoRoot, 'packages', 'vis-contract', 'python', 'pyproject.toml'),
  join(repoRoot, 'packages', 'vis-agent', 'pyproject.toml'),
];

/** The product version, straight from the repo-root VIS_VERSION file. */
export function visVersion() {
  const raw = readFileSync(visVersionFile, 'utf8').trim();
  if (!/^\d+\.\d+\.\d+(-[0-9A-Za-z.-]+)?$/.test(raw)) {
    throw new Error(`${visVersionFile} holds "${raw}", which is not a version`);
  }
  // Store-facing marketing versions are numeric-only; a `-SNAPSHOT`-style
  // qualifier is a local build, so drop it rather than fail the release.
  return raw.split('-')[0];
}

/** Mirror VIS_VERSION into npm metadata. Returns the version. */
export function syncPackageVersion({ quiet = false } = {}) {
  const version = visVersion();
  const text = readFileSync(packageFile, 'utf8');
  const pkg = JSON.parse(text);
  let changed = false;
  if (pkg.version !== version) {
    const next = text.replace(/^(\s*"version":\s*)"[^"]*"/m, `$1"${version}"`);
    if (JSON.parse(next).version !== version) {
      throw new Error(`could not rewrite "version" in ${packageFile}`);
    }
    writeFileSync(packageFile, next);
    changed = true;
  }

  const lockText = readFileSync(packageLockFile, 'utf8');
  const lock = JSON.parse(lockText);
  if (lock.version !== version || lock.packages?.['']?.version !== version) {
    let nextLock = lockText.replace(/^(  "version":\s*)"[^"]*"/m, `$1"${version}"`);
    nextLock = nextLock.replace(/^(      "version":\s*)"[^"]*"/m, `$1"${version}"`);
    const parsed = JSON.parse(nextLock);
    if (parsed.version !== version || parsed.packages?.['']?.version !== version) {
      throw new Error(`could not rewrite root versions in ${packageLockFile}`);
    }
    writeFileSync(packageLockFile, nextLock);
    changed = true;
  }

  if (changed && !quiet) console.log(`✓ npm version mirrors ${version} (from VIS_VERSION)`);
  return version;
}

/**
 * Mirror VIS_VERSION into the Python distributions published to PyPI — the
 * `vis-contract` declaration, the `vis-agent` API, and the `==` pin between them.
 * They are MIRRORS exactly like the npm metadata above — `python_package_test`
 * fails the build when one drifts from VIS_VERSION, so never hand-edit them.
 */
export function syncPythonVersion({ quiet = false } = {}) {
  const version = visVersion();
  for (const file of pyprojectFiles) {
    const text = readFileSync(file, 'utf8');
    let next = text.replace(/^(version = )"[^"]*"/m, `$1"${version}"`);
    if (!next.includes(`\nversion = "${version}"\n`)) {
      throw new Error(`could not rewrite "version" in ${file}`);
    }
    next = next.replace(/"vis-contract==[^"]*"/g, `"vis-contract==${version}"`);
    if (next !== text) {
      writeFileSync(file, next);
      const dist = (next.match(/^name = "([^"]+)"/m) ?? [, file])[1];
      if (!quiet) console.log(`✓ ${dist} (PyPI) version mirrors ${version} (from VIS_VERSION)`);
    }
  }
  return version;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1]) {
  const version = syncPackageVersion();
  syncPythonVersion();
  console.log(version);
}

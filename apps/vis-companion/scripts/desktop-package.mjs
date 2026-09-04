#!/usr/bin/env node
/**
 * Package the built companion (`dist/`) as a DESKTOP app with Pake.
 *
 * The companion is one web bundle; Pake (https://github.com/tw93/pake) wraps that
 * local bundle in a Tauri window, so the desktop app is the SAME build the phone
 * gets and needs no hosted web origin — the app talks to a gateway the user pairs,
 * exactly like the mobile app. `--use-local-file` is what makes it self-contained.
 *
 * ONE installer per (platform, target) lands in `build/desktop/` under a release
 * asset name that carries the product version, platform and arch:
 *
 *   macOS    vis-companion-<v>-macos-universal.dmg     (Apple silicon + Intel)
 *   Windows  vis-companion-<v>-windows-x64.msi
 *   Linux    vis-companion-<v>-linux-x64.deb / .AppImage
 *
 * A release tag runs this on each OS runner (.github/workflows/desktop-companion.yml)
 * and attaches the files to the GitHub Release. Locally:
 *
 *   npm run build && npm run package:desktop          # this OS only
 *
 * Pake needs Node >= 20 and a Rust toolchain (>= 1.85); on Linux the webkit2gtk-4.1
 * dev packages the workflow installs. The native window keeps its title bar on
 * purpose: with `--hide-title-bar` the macOS traffic lights sit on the app bar's
 * logo. Pake's `--app-version` is what the OS shows as the app version.
 */
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, renameSync, rmSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { appDir, syncPackageVersion } from './version.mjs';

export const PAKE_VERSION = '3.15.7';
export const APP_NAME = 'Vis';
export const ICON = join(appDir, 'native-assets', 'ios', 'AppIcon-512@2x.png');
export const OUT_DIR = join(appDir, 'build', 'desktop');

/**
 * What one OS packages. `targets` is Pake's `--targets` word, `ext` the file it
 * writes as `<name>.<ext>` in the working directory, `asset` the release suffix.
 */
export const DESKTOP_TARGETS = {
  darwin: [{ targets: 'universal', ext: 'dmg', asset: 'macos-universal' }],
  win32: [{ targets: 'x64', ext: 'msi', asset: 'windows-x64' }],
  linux: [
    { targets: 'deb', ext: 'deb', asset: 'linux-x64' },
    { targets: 'appimage', ext: 'AppImage', asset: 'linux-x64' },
  ],
};

/** Release asset name for one installer: `vis-companion-<version>-<platform-arch>.<ext>`. */
export const assetName = (version, target) => `vis-companion-${version}-${target.asset}.${target.ext}`;

/** The Pake invocation for one target: every flag the desktop app is built with. */
export function pakeArgs({ distDir, version, target, icon = ICON }) {
  return [
    distDir,
    '--use-local-file',
    '--name', APP_NAME,
    '--identifier', 'com.blockether.viscompanion.desktop',
    '--app-version', version,
    '--icon', icon,
    '--width', '1280',
    '--height', '800',
    '--targets', target.targets,
    ...(target.targets === 'universal' ? ['--multi-arch'] : []),
  ];
}

/** Package every target of THIS platform; returns the asset paths written. */
export function packageDesktop({ platform = process.platform, log = console.log } = {}) {
  const targets = DESKTOP_TARGETS[platform];
  if (!targets) throw new Error(`no desktop target for platform ${platform}`);
  const distDir = join(appDir, 'dist');
  if (!existsSync(join(distDir, 'index.html'))) {
    throw new Error(`${distDir} has no index.html — run \`npm run build\` first`);
  }
  const version = syncPackageVersion({ quiet: true });
  mkdirSync(OUT_DIR, { recursive: true });
  const written = [];
  for (const target of targets) {
    const args = pakeArgs({ distDir, version, target });
    log(`▸ pake ${args.join(' ')}`);
    const npx = platform === 'win32' ? 'npx.cmd' : 'npx';
    const run = spawnSync(npx, ['-y', `pake-cli@${PAKE_VERSION}`, ...args], {
      cwd: OUT_DIR,
      stdio: 'inherit',
      shell: platform === 'win32',
    });
    if (run.status !== 0) throw new Error(`pake failed for --targets ${target.targets}`);
    const produced = join(OUT_DIR, `${APP_NAME}.${target.ext}`);
    if (!existsSync(produced)) throw new Error(`pake reported success but ${produced} is missing`);
    const asset = join(OUT_DIR, assetName(version, target));
    rmSync(asset, { force: true });
    renameSync(produced, asset);
    log(`✓ ${asset}`);
    written.push(asset);
  }
  return written;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1])) {
  try {
    packageDesktop();
  } catch (error) {
    console.error(`\n✗ ${error.message}\n`);
    process.exit(1);
  }
}

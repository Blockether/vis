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
 *   Linux    vis-companion-<v>-linux-x64.deb / .AppImage
 *   Linux    vis-companion-<v>-linux-arm64.deb / .AppImage
 *
 * A release tag runs this on each OS runner (.github/workflows/desktop-companion.yml)
 * and attaches the files to the GitHub Release. Locally:
 *
 *   npm run build && npm run package:desktop         # release installers
 *   npm run build && npm run package:desktop -- --dev # unsigned host-only app
 *
 * Dev uses a separate app identifier and `build/desktop-dev/` output directory.
 * It does not need release signing credentials or cross-compilation targets.
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

/** Release installers, or a single host-native dev installer; refuse other hosts. */
export function desktopTargets(platform, arch, dev = false) {
  if (!['darwin', 'linux'].includes(platform) || !['x64', 'arm64'].includes(arch)) {
    throw new Error(`no desktop target for platform ${platform}/${arch}`);
  }
  if (platform === 'darwin')
    return dev
      ? [{ targets: 'dmg', ext: 'dmg', asset: `macos-${arch}` }]
      : [{ targets: 'universal', ext: 'dmg', asset: 'macos-universal' }];
  if (dev) return [{ targets: 'appimage', ext: 'AppImage', asset: `linux-${arch}` }];
  return [
    { targets: 'deb', ext: 'deb', asset: `linux-${arch}` },
    { targets: 'appimage', ext: 'AppImage', asset: `linux-${arch}` },
  ];
}

/** Release asset name for one installer: `vis-companion-<version>-<platform-arch>.<ext>`. */
export const assetName = (version, target) =>
  `vis-companion-${version}-${target.asset}.${target.ext}`;

/** The Pake invocation for one target: every flag the desktop app is built with. */
export function pakeArgs({ distDir, version, target, icon = ICON, dev = false }) {
  return [
    distDir,
    '--use-local-file',
    '--name',
    APP_NAME,
    '--identifier',
    `com.blockether.viscompanion.desktop${dev ? '.dev' : ''}`,
    '--app-version',
    version,
    '--icon',
    icon,
    '--width',
    '1280',
    '--height',
    '800',
    '--targets',
    target.targets,
    ...(target.targets === 'universal' ? ['--multi-arch'] : []),
  ];
}

/** Package release installers or the local dev app; return the asset paths written. */
export function packageDesktop({
  platform = process.platform,
  arch = process.arch,
  dev = false,
  log = console.log,
} = {}) {
  const targets = desktopTargets(platform, arch, dev);
  const distDir = join(appDir, 'dist');
  if (!existsSync(join(distDir, 'index.html'))) {
    throw new Error(`${distDir} has no index.html — run \`npm run build\` first`);
  }
  const version = syncPackageVersion({ quiet: true });
  const outDir = dev ? join(appDir, 'build', 'desktop-dev') : OUT_DIR;
  const env = { ...process.env };
  if (dev) {
    for (const name of [
      'APPLE_SIGNING_IDENTITY',
      'APPLE_CERTIFICATE',
      'APPLE_CERTIFICATE_PASSWORD',
      'APPLE_API_KEY',
      'APPLE_API_KEY_PATH',
      'APPLE_API_ISSUER',
      'APPLE_ID',
      'APPLE_PASSWORD',
      'APPLE_TEAM_ID',
    ]) {
      delete env[name];
    }
  }
  mkdirSync(outDir, { recursive: true });
  const written = [];
  for (const target of targets) {
    const args = pakeArgs({ distDir, version, target, dev });
    // Pake normalizes Linux package names to lowercase, unlike macOS.
    const bundleName = platform === 'linux' ? APP_NAME.toLowerCase() : APP_NAME;
    const produced = join(outDir, `${bundleName}.${target.ext}`);
    rmSync(produced, { force: true });
    log(`▸ pake ${args.join(' ')}`);
    const run = spawnSync('npx', ['-y', `pake-cli@${PAKE_VERSION}`, ...args], {
      cwd: outDir,
      stdio: 'inherit',
      env,
    });
    if (run.status !== 0) throw new Error(`pake failed for --targets ${target.targets}`);
    if (!existsSync(produced)) throw new Error(`pake reported success but ${produced} is missing`);
    const asset = join(outDir, assetName(version, target));
    rmSync(asset, { force: true });
    renameSync(produced, asset);
    log(`✓ ${asset}`);
    written.push(asset);
  }
  return written;
}

if (process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1])) {
  try {
    const args = process.argv.slice(2);
    if (args.some((arg) => arg !== '--dev')) throw new Error('usage: desktop-package.mjs [--dev]');
    packageDesktop({ dev: args.includes('--dev') });
  } catch (error) {
    console.error(`\n✗ ${error.message}\n`);
    process.exit(1);
  }
}

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
 *   Windows  vis-companion-<v>-windows-x64.msi
 *
 * A release tag runs this on each OS runner (.github/workflows/desktop-companion.yml)
 * and attaches the files to the GitHub Release. Locally:
 *
 *   npm run build && npm run package:desktop         # release installers
 *   npm run build && npm run package:desktop -- --dev # unsigned host-only app
 *   npm run package:desktop -- --live                 # dev app that shows `npm run dev`
 *
 * Dev uses a separate app identifier and `build/desktop-dev/` output directory.
 * Live has its own identifier and `build/desktop-live/`, so it runs next to them.
 * It does not need release signing credentials or cross-compilation targets.
 * The live app is a dev app whose window loads the Vite dev server (LIVE_URL)
 * instead of `dist/`, so web changes show in the window without a new build.
 * Rust changes in `native/desktop/` still need a new build.
 *
 * Pake needs Node >= 20 and a Rust toolchain (>= 1.85); on Linux the webkit2gtk-4.1
 * dev packages the workflow installs. Windows needs the MSVC C++ build tools and
 * WebView2. Invoke this script through npm on Windows. The native window keeps
 * its title bar on purpose: with `--hide-title-bar` the macOS traffic lights sit
 * on the app bar's logo. Pake's `--app-version` is what the OS shows as the app version.
 */
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, renameSync, rmSync, writeFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { appDir, syncPackageVersion } from './version.mjs';

export const PAKE_VERSION = '3.15.7';
export const APP_NAME = 'Vis';
export const ICON = join(appDir, 'native-assets', 'ios', 'AppIcon-512@2x.png');
export const OUT_DIR = join(appDir, 'build', 'desktop');
/** The Vite dev server; see `server` in `vite.config.ts`. */
export const LIVE_URL = 'http://127.0.0.1:5273';

/** Release installers, or a single host-native dev installer; refuse other hosts. */
export function desktopTargets(platform, arch, dev = false) {
  if (platform === 'win32' && arch === 'x64')
    return [{ targets: 'x64', ext: 'msi', asset: 'windows-x64' }];
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
export function pakeArgs({ distDir, version, target, icon = ICON, dev = false, live = false }) {
  return [
    ...(live ? [LIVE_URL] : [distDir, '--use-local-file']),
    '--name',
    APP_NAME,
    '--identifier',
    `com.blockether.viscompanion.desktop${live ? '.live' : dev ? '.dev' : ''}`,
    '--app-version',
    version,
    '--icon',
    icon,
    '--width',
    '1280',
    '--height',
    '800',
    // macOS signs the app with the hardened runtime, which denies capture unless the
    // build carries the matching entitlement. These flags make Pake write an
    // entitlements.plist for the microphone and the camera; other hosts ignore them.
    '--microphone',
    '--camera',
    '--targets',
    target.targets,
    ...(target.targets === 'universal' ? ['--multi-arch'] : []),
  ];
}

/**
 * The save panel. Tauri's own dialog plugin uses the same crate and features: GTK 3 on
 * Linux, the system panels on macOS and Windows.
 */
export const RFD_DEPENDENCY = 'rfd = { version = "0.15", default-features = false, features = ["gtk3"] }';

/** Vis's own commands in `native/desktop/artifact.rs`. */
export const ARTIFACT_COMMANDS = [
  'open_xlsx',
  'reveal_artifact',
  'reveal_kept_artifact',
  'save_artifact_as',
  'open_remote_artifact',
];

/** Vis's own commands in `native/desktop/engine.rs`. */
export const ENGINE_COMMANDS = [
  'engine_status',
  'engine_run',
  'engine_progress',
  'engine_stop',
  'engine_gateway_start',
  'engine_gateway_log',
  'engine_gateway_running',
  'engine_gateway_token',
  'engine_latest',
];

/** Vis's own commands in `native/desktop/proxy_browser.rs`: the browser for private links. */
export const PROXY_BROWSER_COMMANDS = ['proxy_browser_available', 'proxy_browser_open'];

/** Each Vis module with commands, in the order of its registration. */
const HOST_MODULES = [
  ['artifact', ARTIFACT_COMMANDS],
  ['engine', ENGINE_COMMANDS],
  ['proxy_browser', PROXY_BROWSER_COMMANDS],
];

/** The Pake line that creates the main window. */
export const WINDOW_SETUP =
  'let window = set_window(app.app_handle(), &pake_config, &tauri_config)?;';

/** Reloads a window after WebKit ends its page process; see `native/desktop/web_content.rs`. */
export const WEB_CONTENT_RELOAD = 'web_content::reload_after_web_content_exit(&window);';

const escapeRegExp = (text) => text.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

/** True when the tools directory has the pinned Pake from an earlier build. */
function hasPake(pakeDir) {
  try {
    return JSON.parse(readFileSync(join(pakeDir, 'package.json'), 'utf8')).version === PAKE_VERSION;
  } catch {
    return false;
  }
}

/** Install a private Pake template with Vis's native file commands and window hooks. */
export function prepareDesktopHost({ npmCli, toolsDir, env, windowsSigning = false, live = false }) {
  if (windowsSigning) {
    for (const name of ['ENDPOINT', 'ACCOUNT', 'PROFILE', 'PUBLISHER']) {
      if (!env[`WINDOWS_SIGNING_${name}`]?.trim()) {
        throw new Error(`Missing required signing configuration: WINDOWS_SIGNING_${name}`);
      }
    }
  }
  const pakeDir = join(toolsDir, 'node_modules', 'pake-cli');
  // Pake installs its own packages inside its directory at the first build, and npm
  // cannot install over them. A kept directory with the pinned Pake needs no install.
  if (!hasPake(pakeDir)) {
    const install = spawnSync(
      npmCli ? process.execPath : 'npm',
      [
        ...(npmCli ? [npmCli] : []),
        'install',
        '--prefix',
        toolsDir,
        '--no-save',
        '--package-lock=false',
        `pake-cli@${PAKE_VERSION}`,
      ],
      { stdio: 'inherit', env },
    );
    if (install.error) throw install.error;
    if (install.status !== 0) throw new Error('Failed to install desktop build tools');
  }
  const sourceDir = join(pakeDir, 'src-tauri', 'src');
  const libPath = join(sourceDir, 'lib.rs');
  let lib = readFileSync(libPath, 'utf8');
  const module = 'mod util;';
  const handler = '.invoke_handler(tauri::generate_handler![';
  if (!lib.includes(module) || !lib.includes(handler)) {
    throw new Error('Pake native command registration changed');
  }
  if (!lib.includes(WINDOW_SETUP)) throw new Error('Pake window setup changed');
  // A kept tools directory holds the registrations of earlier builds, also the older
  // `use name::command;` form, so drop all of them first.
  for (const [name] of HOST_MODULES) {
    lib = lib.replace(new RegExp(`\\nmod ${name};\\nuse ${name}::(?:\\{[^}]*\\}|\\w+);`, 'g'), '');
  }
  const commands = HOST_MODULES.flatMap(([, names]) => names);
  lib = lib
    .replace('\nmod web_content;', '')
    .replace(new RegExp(`\\n *${escapeRegExp(WEB_CONTENT_RELOAD)}`), '')
    .replace(
      new RegExp(`(${escapeRegExp(handler)})(?:\\n {12}(?:${commands.join('|')}),)+`),
      '$1',
    );
  // Each insertion goes first, so the last module goes in first.
  for (const [name, names] of [...HOST_MODULES].reverse()) {
    lib = lib.replace(module, `${module}\nmod ${name};\nuse ${name}::{${names.join(', ')}};`);
    lib = lib.replace(handler, `${handler}${names.map((command) => `\n            ${command},`).join('')}`);
  }
  lib = lib.replace(module, `${module}\nmod web_content;`);
  lib = lib.replace(WINDOW_SETUP, `${WINDOW_SETUP}\n            ${WEB_CONTENT_RELOAD}`);
  writeFileSync(libPath, lib);
  const cargoPath = join(pakeDir, 'src-tauri', 'Cargo.toml');
  const cargo = readFileSync(cargoPath, 'utf8');
  if (!/^rfd = /m.test(cargo)) {
    if (!cargo.includes('\n[dependencies]\n')) throw new Error('Pake Cargo.toml changed');
    writeFileSync(cargoPath, cargo.replace('\n[dependencies]\n', `\n[dependencies]\n${RFD_DEPENDENCY}\n`));
  }
  // Tauri gives plugin commands (shell, notification) only to the remote origins of a
  // capability. Pake allows HTTPS only, so the live app adds the HTTP dev server. The same
  // tools directory serves later release builds, so each build sets the list again.
  const capabilityPath = join(pakeDir, 'src-tauri', 'capabilities', 'default.json');
  const capability = JSON.parse(readFileSync(capabilityPath, 'utf8'));
  if (!Array.isArray(capability.remote?.urls)) throw new Error('Pake capability changed');
  capability.remote.urls = [
    ...capability.remote.urls.filter((url) => url !== LIVE_URL),
    ...(live ? [LIVE_URL] : []),
  ];
  writeFileSync(capabilityPath, `${JSON.stringify(capability, null, 2)}\n`);
  for (const name of ['artifact.rs', 'engine.rs', 'proxy_browser.rs', 'web_content.rs']) {
    writeFileSync(
      join(sourceDir, name),
      readFileSync(new URL(`../native/desktop/${name}`, import.meta.url), 'utf8'),
    );
  }
  const configPath = join(pakeDir, 'src-tauri', 'tauri.windows.conf.json');
  const config = JSON.parse(readFileSync(configPath, 'utf8'));
  if (windowsSigning) {
    config.bundle.windows.signCommand = {
      cmd: 'pwsh',
      args: [
        '-NoProfile',
        '-NonInteractive',
        '-File',
        join(appDir, 'scripts', 'windows-sign.ps1'),
        '%1',
      ],
    };
    writeFileSync(configPath, `${JSON.stringify(config, null, 2)}\n`);
  } else if (config.bundle.windows.signCommand) {
    // The same tools directory can have served a previous signed release.
    delete config.bundle.windows.signCommand;
    writeFileSync(configPath, `${JSON.stringify(config, null, 2)}\n`);
  }
  return join(pakeDir, 'dist', 'cli.js');
}

/** Package release installers or the local dev app; return the asset paths written. */
export function packageDesktop({
  platform = process.platform,
  arch = process.arch,
  dev = false,
  live = false,
  log = console.log,
} = {}) {
  dev ||= live;
  const targets = desktopTargets(platform, arch, dev);
  // npm exposes its JS entry point: launching it with Node avoids .cmd quoting
  // and preserves paths with spaces without executing a Windows command shell.
  const npmCli = process.env.npm_execpath;
  if (platform === 'win32' && !npmCli) {
    throw new Error('On Windows, run `npm run package:desktop` so npm supplies its CLI path');
  }
  const distDir = join(appDir, 'dist');
  if (!live && !existsSync(join(distDir, 'index.html'))) {
    throw new Error(`${distDir} has no index.html — run \`npm run build\` first`);
  }
  const version = syncPackageVersion({ quiet: true });
  const outDir = live
    ? join(appDir, 'build', 'desktop-live')
    : dev
      ? join(appDir, 'build', 'desktop-dev')
      : OUT_DIR;
  const env = {
    ...process.env,
    // Pake otherwise compiles under a disposable npx installation. Both Cargo
    // and Pake's artifact lookup honor this absolute directory.
    CARGO_TARGET_DIR: resolve(
      process.env.CARGO_TARGET_DIR || join(appDir, 'build', 'desktop-target'),
    ),
  };
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
  const pakeCli = prepareDesktopHost({
    npmCli,
    toolsDir: join(appDir, 'build', 'desktop-tools'),
    env,
    windowsSigning: platform === 'win32' && !dev,
    live,
  });
  mkdirSync(outDir, { recursive: true });
  const written = [];
  for (const target of targets) {
    const args = pakeArgs({ distDir, version, target, dev, live });
    // Pake normalizes Linux package names to lowercase, unlike macOS and Windows.
    const bundleName = platform === 'linux' ? APP_NAME.toLowerCase() : APP_NAME;
    const produced = join(outDir, `${bundleName}.${target.ext}`);
    rmSync(produced, { force: true });
    log(`▸ pake ${args.join(' ')}`);
    const run = spawnSync(process.execPath, [pakeCli, ...args], {
      cwd: outDir,
      stdio: 'inherit',
      env,
    });
    if (run.error) throw run.error;
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
    if (args.some((arg) => arg !== '--dev' && arg !== '--live')) {
      throw new Error('usage: desktop-package.mjs [--dev | --live]');
    }
    packageDesktop({ dev: args.includes('--dev'), live: args.includes('--live') });
  } catch (error) {
    console.error(`\n✗ ${error.message}\n`);
    process.exit(1);
  }
}

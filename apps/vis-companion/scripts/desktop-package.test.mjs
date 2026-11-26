import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync, renameSync, writeFileSync } from 'node:fs';
import { basename, join, resolve } from 'node:path';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
  desktopTargets,
  assetName,
  pakeArgs,
  packageDesktop,
  prepareDesktopHost,
  ARTIFACT_COMMANDS,
  ENGINE_COMMANDS,
  LIVE_URL,
  PAKE_VERSION,
  PROXY_BROWSER_COMMANDS,
  RFD_DEPENDENCY,
  DRAG_STRIP_ONLY,
  TITLE_BAR_STYLE,
  TRAFFIC_LIGHTS,
  WEB_CONTENT_RELOAD,
  WINDOW_SETUP,
} from './desktop-package.mjs';
import { syncPackageVersion } from './version.mjs';

vi.mock('node:child_process', () => ({ spawnSync: vi.fn(() => ({ status: 0 })) }));
vi.mock('node:fs', async (importOriginal) => {
  const original = await importOriginal();
  return {
    ...original,
    existsSync: vi.fn(() => true),
    mkdirSync: vi.fn(),
    renameSync: vi.fn(),
    rmSync: vi.fn(),
    writeFileSync: vi.fn(),
    readFileSync: vi.fn((...args) => {
      if (String(args[0]).endsWith('lib.rs')) {
        return 'mod util;\nfn run() { builder.invoke_handler(tauri::generate_handler![download_file,]); }\nfn setup() {\n            let window = set_window(app.app_handle(), &pake_config, &tauri_config)?;\n}\n';
      }
      if (String(args[0]).endsWith('style.js')) {
        return `  if (immersive) {\n    topPaddingStyleElement.textContent = isMac\n      ? topPaddingCSS\n      : \`#pake-top-dom {}\`;\n  }\n`;
      }
      if (String(args[0]).endsWith('window.rs')) {
        return `    {\n        window_builder = window_builder.title_bar_style(title_bar_style);\n    }\n`;
      }
      if (String(args[0]).endsWith('Cargo.toml')) {
        return '[package]\n\n[dependencies]\nserde = "1"\n';
      }
      if (String(args[0]).endsWith('tauri.windows.conf.json')) {
        return JSON.stringify({
          bundle: { targets: ['msi'], windows: { wix: { language: ['en-US'] } } },
        });
      }
      if (String(args[0]).endsWith('default.json')) {
        return JSON.stringify({ remote: { urls: ['https://*.*'] }, permissions: [] });
      }
      return original.readFileSync(...args);
    }),
  };
});
vi.mock('./version.mjs', () => ({ appDir: '/app', syncPackageVersion: vi.fn(() => '1.0.0') }));

beforeEach(() => {
  vi.stubEnv('npm_execpath', String.raw`C:\Program Files\Node & Tools\npm-cli.js`);
  for (const name of ['ENDPOINT', 'ACCOUNT', 'PROFILE', 'PUBLISHER']) {
    vi.stubEnv(`WINDOWS_SIGNING_${name}`, `test-${name.toLowerCase()}`);
  }
});
afterEach(() => vi.unstubAllEnvs());

// The installers are release assets: a name must say which OS and arch it is for,
// and every OS the workflow runs on must have at least one target to build.
describe('desktop package', () => {

  it('names an asset by version, platform and installer type', () => {
    expect(assetName('0.1.30', desktopTargets('darwin', 'arm64')[0])).toBe(
      'vis-companion-0.1.30-macos-universal.dmg',
    );
    expect(desktopTargets('linux', 'x64').map((t) => assetName('0.1.30', t))).toEqual([
      'vis-companion-0.1.30-linux-x64.deb',
      'vis-companion-0.1.30-linux-x64.AppImage',
    ]);
  });

  it('covers every release runner OS with distinct asset names', () => {
    const targets = [
      ['darwin', 'arm64'],
      ['linux', 'x64'],
      ['linux', 'arm64'],
      ['win32', 'x64'],
    ].flatMap(([os, arch]) => desktopTargets(os, arch));
    const names = targets.map((t) => assetName('1.0.0', t));
    expect(names).toHaveLength(6);
    expect(new Set(names).size).toBe(names.length);
  });

  it('packages the local bundle, lays the app bar under the macOS title bar and builds macOS universal', () => {
    const args = pakeArgs({
      distDir: '/tmp/dist',
      version: '0.1.30',
      target: desktopTargets('darwin', 'arm64')[0],
      icon: '/i.png',
    });
    expect(args[0]).toBe('/tmp/dist');
    expect(args).toContain('--use-local-file');
    expect(args).toContain('--multi-arch');
    // Without these the hardened-runtime app ships empty entitlements, so macOS refuses
    // the microphone as "The request is not allowed by the user agent" and never prompts.
    expect(args).toContain('--microphone');
    expect(args).toContain('--camera');
    expect(args).toContain('--hide-title-bar');
    expect(args.slice(args.indexOf('--app-version'), args.indexOf('--app-version') + 2)).toEqual([
      '--app-version',
      '0.1.30',
    ]);
    const linux = pakeArgs({
      distDir: '/tmp/dist',
      version: '0.1.30',
      target: desktopTargets('linux', 'arm64')[0],
      icon: '/i.png',
    });
    expect(linux).not.toContain('--multi-arch');
    expect(linux).not.toContain('--hide-title-bar');
    expect(linux.slice(linux.indexOf('--targets'), linux.indexOf('--targets') + 2)).toEqual([
      '--targets',
      'deb',
    ]);
  });

  it('registers the file commands and the save dialog crate in every private desktop template', () => {
    vi.clearAllMocks();
    const cli = prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
    expect(cli).toBe(join('/tools', 'node_modules', 'pake-cli', 'dist', 'cli.js'));
    const [libPath, lib] = writeFileSync.mock.calls.find(([path]) => String(path).endsWith('lib.rs'));
    expect(libPath).toBe(join('/tools', 'node_modules', 'pake-cli', 'src-tauri', 'src', 'lib.rs'));
    expect(lib).toContain(
      'mod artifact;\nuse artifact::{open_xlsx, reveal_artifact, reveal_kept_artifact, save_artifact_as, open_remote_artifact};',
    );
    expect(lib).toContain(
      '.invoke_handler(tauri::generate_handler![\n            open_xlsx,\n            reveal_artifact,\n            reveal_kept_artifact,\n            save_artifact_as,\n            open_remote_artifact,\n            engine_status,',
    );
    const [, cargo] = writeFileSync.mock.calls.find(([path]) => String(path).endsWith('Cargo.toml'));
    expect(cargo).toContain(`[dependencies]\n${RFD_DEPENDENCY}\nserde = "1"`);
    const [, command] = writeFileSync.mock.calls.find(([path]) =>
      String(path).endsWith('artifact.rs'),
    );
    expect(command).toContain('pub async fn open_xlsx(');
    expect(command).toContain('stage_workbook(&downloads, &filename, &data)');
    expect(command).toContain('.open_path(');
    expect(command).toContain('pub async fn reveal_artifact(');
    expect(command).toContain('.reveal_item_in_dir(&path)');
    expect(command).toContain('pub async fn reveal_kept_artifact(');
    expect(command).toContain('pub async fn save_artifact_as(');
    expect(command).toContain('pub async fn open_remote_artifact(');
    expect(
      writeFileSync.mock.calls.some(([path]) => String(path).endsWith('tauri.windows.conf.json')),
    ).toBe(false);
  });

  // A kept tools directory already holds the registrations of earlier builds, also the older
  // `use artifact::open_xlsx;` form. A second `mod artifact;`,
  // `mod web_content;` or `rfd` line fails the Rust build, and an older command list misses
  // the new commands.
  it('replaces the registration of an earlier build in a kept tools directory', () => {
    const fresh = readFileSync.getMockImplementation();
    readFileSync.mockImplementation((path, ...rest) => {
      if (String(path).endsWith('lib.rs')) {
        return 'mod util;\nmod web_content;\nmod artifact;\nuse artifact::{open_xlsx, reveal_artifact};\nmod artifact;\nuse artifact::open_xlsx;\nfn run() { builder.invoke_handler(tauri::generate_handler![\n            open_xlsx,\n            reveal_artifact,download_file,]); }\nfn setup() {\n            let window = set_window(app.app_handle(), &pake_config, &tauri_config)?;\n            web_content::reload_after_web_content_exit(&window);\n}\n';
      }
      if (String(path).endsWith('Cargo.toml')) {
        return `[package]\n\n[dependencies]\n${RFD_DEPENDENCY}\nserde = "1"\n`;
      }
      return fresh(path, ...rest);
    });
    try {
      vi.clearAllMocks();
      prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
      const [, lib] = writeFileSync.mock.calls.find(([path]) => String(path).endsWith('lib.rs'));
      expect(lib.match(/mod artifact;/g)).toHaveLength(1);
      expect(lib.match(/mod web_content;/g)).toHaveLength(1);
      expect(lib.split(WEB_CONTENT_RELOAD)).toHaveLength(2);
      expect(lib).toContain(`use artifact::{${ARTIFACT_COMMANDS.join(', ')}};`);
      expect(lib).toContain(`use engine::{${ENGINE_COMMANDS.join(', ')}};`);
      expect(lib).toContain(`use proxy_browser::{${PROXY_BROWSER_COMMANDS.join(', ')}};`);
      for (const name of [...ARTIFACT_COMMANDS, ...ENGINE_COMMANDS, ...PROXY_BROWSER_COMMANDS]) {
        expect(lib.match(new RegExp(`\\n {12}${name},`, 'g'))).toHaveLength(1);
      }
      expect(writeFileSync.mock.calls.some(([path]) => String(path).endsWith('Cargo.toml'))).toBe(
        false,
      );
    } finally {
      readFileSync.mockImplementation(fresh);
    }
  });

  // Without the hidden title, macOS draws the window name over the app bar; without the
  // position, the traffic lights sit above the logo row. A second block fails the Rust build.
  it('hides the window title and centres the traffic lights on the app bar once', () => {
    const fresh = readFileSync.getMockImplementation();
    readFileSync.mockImplementation((path, ...rest) => {
      if (String(path).endsWith('window.rs')) {
        const earlier = TRAFFIC_LIGHTS.replace('26.0', '9.0');
        return `    {\n        ${TITLE_BAR_STYLE}\n        ${earlier}\n    }\n`;
      }
      return fresh(path, ...rest);
    });
    try {
      vi.clearAllMocks();
      prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
      const [, source] = writeFileSync.mock.calls.find(([path]) => String(path).endsWith('window.rs'));
      expect(source).toBe(`    {\n        ${TITLE_BAR_STYLE}\n        ${TRAFFIC_LIGHTS}\n    }\n`);
      const [, capability] = writeFileSync.mock.calls.find(([path]) =>
        String(path).endsWith('default.json'),
      );
      expect(JSON.parse(capability).permissions).toContain('core:window:allow-internal-toggle-maximize');
    } finally {
      readFileSync.mockImplementation(fresh);
    }
  });

  // Pake's padding for other web apps matched the split shell and put a gap under the app bar.
  it('keeps only the drag strip of the padding Pake adds under the macOS title bar', () => {
    vi.clearAllMocks();
    prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
    const [, style] = writeFileSync.mock.calls.find(([path]) => String(path).endsWith('style.js'));
    expect(style).not.toContain('topPaddingCSS');
    expect(style).toContain(`${DRAG_STRIP_ONLY}#pake-top-dom {}\``);
    const fresh = readFileSync.getMockImplementation();
    readFileSync.mockImplementation((path, ...rest) =>
      String(path).endsWith('style.js') ? style : fresh(path, ...rest),
    );
    try {
      vi.clearAllMocks();
      prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
      expect(writeFileSync.mock.calls.some(([path]) => String(path).endsWith('style.js'))).toBe(false);
    } finally {
      readFileSync.mockImplementation(fresh);
    }
  });

  // Without this hook a page process that macOS ends leaves the window white until the
  // person reloads it or starts the app again.
  it('reloads the main window after its page process ends', () => {
    vi.clearAllMocks();
    prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
    const [, lib] = writeFileSync.mock.calls.find(([path]) => String(path).endsWith('lib.rs'));
    expect(lib).toContain('mod util;\nmod web_content;');
    expect(lib).toContain(`${WINDOW_SETUP}\n            ${WEB_CONTENT_RELOAD}\n}`);
    const [hookPath, hook] = writeFileSync.mock.calls.find(([path]) =>
      String(path).endsWith('web_content.rs'),
    );
    expect(hookPath).toBe(
      join('/tools', 'node_modules', 'pake-cli', 'src-tauri', 'src', 'web_content.rs'),
    );
    expect(hook).toContain('pub fn reload_after_web_content_exit(window: &tauri::WebviewWindow)');
  });

  // A Pake release that makes the window in a different way must stop the build. Else the
  // app ships without the reload, and a lost page process makes the window white again.
  it('refuses a Pake template without the main window line', () => {
    const fresh = readFileSync.getMockImplementation();
    readFileSync.mockImplementation((path, ...rest) =>
      String(path).endsWith('lib.rs')
        ? 'mod util;\nfn run() { builder.invoke_handler(tauri::generate_handler![download_file,]); }\n'
        : fresh(path, ...rest),
    );
    try {
      expect(() =>
        prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env }),
      ).toThrow('Pake window setup changed');
    } finally {
      readFileSync.mockImplementation(fresh);
    }
  });

  it('clears release signing before a later unsigned development build', () => {
    vi.clearAllMocks();
    const read = readFileSync.getMockImplementation();
    readFileSync.mockImplementation((...args) => {
      if (String(args[0]).endsWith('tauri.windows.conf.json')) {
        return JSON.stringify({ bundle: { windows: { signCommand: { cmd: 'pwsh' } } } });
      }
      return read(...args);
    });
    try {
      prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
      const configWrite = writeFileSync.mock.calls.find(([path]) =>
        String(path).endsWith('tauri.windows.conf.json'),
      );
      expect(configWrite).toBeDefined();
      expect(JSON.parse(configWrite[1]).bundle.windows.signCommand).toBeUndefined();
    } finally {
      readFileSync.mockImplementation(read);
    }
  });

  // Without the dev server origin, the live window cannot open links or notify. A release
  // built later in the same tools directory must not keep that HTTP origin.
  it('gives host plugins to the dev server origin only in the live app', () => {
    const remoteUrls = (live) => {
      vi.clearAllMocks();
      prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env, live });
      const [, capability] = writeFileSync.mock.calls.find(([path]) =>
        String(path).endsWith(join('capabilities', 'default.json')),
      );
      return JSON.parse(capability).remote.urls;
    };
    const read = readFileSync.getMockImplementation();
    readFileSync.mockImplementation((...args) =>
      String(args[0]).endsWith('default.json')
        ? JSON.stringify({ remote: { urls: ['https://*.*', LIVE_URL] }, permissions: [] })
        : read(...args),
    );
    try {
      expect(remoteUrls(true)).toEqual(['https://*.*', LIVE_URL]);
      expect(remoteUrls(false)).toEqual(['https://*.*']);
    } finally {
      readFileSync.mockImplementation(read);
    }
  });

  it('points the live app at the dev server instead of the bundle', () => {
    const args = pakeArgs({
      distDir: '/tmp/dist',
      version: '0.1.30',
      target: desktopTargets('darwin', 'arm64', true)[0],
      icon: '/i.png',
      dev: true,
      live: true,
    });
    expect(args[0]).toBe(LIVE_URL);
    // An installed dev app has the dev identifier; macOS would open that app instead.
    expect(args[args.indexOf('--identifier') + 1]).toBe('com.blockether.viscompanion.desktop.live');
    expect(args).not.toContain('/tmp/dist');
    expect(args).not.toContain('--use-local-file');
  });
});

// v0.1.43 ran before checkout on fresh Linux runners and had no ARM64 package job.
const workflow = readFileSync(
  new URL('../../../.github/workflows/desktop-companion.yml', import.meta.url),
  'utf8',
).replace(/\r\n/g, '\n');

describe('desktop release platforms', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    existsSync.mockReturnValue(true);
  });

  it('uses an absolute reusable Cargo target outside the build tool installation', () => {
    vi.stubEnv('CARGO_TARGET_DIR', '');
    packageDesktop({ platform: 'linux', arch: 'x64', log: vi.fn() });
    for (const [, , options] of spawnSync.mock.calls) {
      // Windows resolves a POSIX-absolute path onto the current drive, so the
      // expectation is the same `resolve` the script itself applies.
      expect(options.env.CARGO_TARGET_DIR).toBe(resolve('/app/build/desktop-target'));
    }
    vi.stubEnv('CARGO_TARGET_DIR', '/cache/custom-target');
    vi.clearAllMocks();
    packageDesktop({ platform: 'darwin', arch: 'arm64', log: vi.fn() });
    expect(spawnSync.mock.calls[0][2].env.CARGO_TARGET_DIR).toBe(
      resolve('/cache/custom-target'),
    );
  });

  it.each(['x64', 'arm64'])('packages native Linux %s with truthful asset names', (arch) => {
    // Pake lowercases Linux names; the runner filesystem is case-sensitive.
    existsSync.mockImplementation((path) =>
      ['index.html', 'vis.deb', 'vis.AppImage'].includes(basename(path)),
    );
    const assets = packageDesktop({ platform: 'linux', arch, log: vi.fn() });
    expect(assets.map((asset) => basename(asset))).toEqual([
      `vis-companion-1.0.0-linux-${arch}.deb`,
      `vis-companion-1.0.0-linux-${arch}.AppImage`,
    ]);
    expect(renameSync.mock.calls.map(([source]) => basename(source))).toEqual([
      'vis.deb',
      'vis.AppImage',
    ]);
    expect(spawnSync).toHaveBeenCalledTimes(3);
    for (const [command, args] of spawnSync.mock.calls.slice(1)) {
      expect(command).toBe(process.execPath);
      expect(args).not.toContain('--multi-arch');
    }
  });

  it.each(['x64', 'arm64'])('packages universal macOS on a %s host', (arch) => {
    const assets = packageDesktop({ platform: 'darwin', arch, log: vi.fn() });
    expect(assets.map((asset) => basename(asset))).toEqual([
      'vis-companion-1.0.0-macos-universal.dmg',
    ]);
    expect(renameSync.mock.calls.map(([source]) => basename(source))).toEqual(['Vis.dmg']);
    expect(spawnSync).toHaveBeenCalledTimes(2);
    expect(spawnSync.mock.calls[1][1]).toContain('--multi-arch');
  });

  it('installs isolated signing tools and packages Windows without invoking a command shell', () => {
    const assets = packageDesktop({ platform: 'win32', arch: 'x64', log: vi.fn() });
    expect(assets.map((asset) => basename(asset))).toEqual(['vis-companion-1.0.0-windows-x64.msi']);
    expect(spawnSync).toHaveBeenCalledTimes(2);
    const [installCommand, installArgs] = spawnSync.mock.calls[0];
    expect(installCommand).toBe(process.execPath);
    expect(installArgs).toContain('pake-cli@3.15.7');
    expect(installArgs).toContain(join('/app', 'build', 'desktop-tools'));
    const [command, args, options] = spawnSync.mock.calls[1];
    expect(command).toBe(process.execPath);
    expect(args[0]).toBe(
      join('/app', 'build', 'desktop-tools', 'node_modules', 'pake-cli', 'dist', 'cli.js'),
    );
    expect(args[1]).toBe(join('/app', 'dist'));
    expect(args[args.indexOf('--targets') + 1]).toBe('x64');
    expect(options.shell).toBeUndefined();
    const [configPath, content] = writeFileSync.mock.calls.find(([path]) =>
      String(path).endsWith('tauri.windows.conf.json'),
    );
    expect(configPath).toMatch(/tauri.windows.conf.json$/);
    const config = JSON.parse(content);
    expect(config.bundle.targets).toEqual(['msi']);
    expect(config.bundle.windows.wix.language).toEqual(['en-US']);
    expect(config.bundle.windows.signCommand).toEqual({
      cmd: 'pwsh',
      args: [
        '-NoProfile',
        '-NonInteractive',
        '-File',
        join('/app', 'scripts', 'windows-sign.ps1'),
        '%1',
      ],
    });
  });

  it.each(['ENDPOINT', 'ACCOUNT', 'PROFILE', 'PUBLISHER'])('refuses missing Windows %s', (name) => {
    vi.stubEnv(`WINDOWS_SIGNING_${name}`, '');
    expect(() => packageDesktop({ platform: 'win32', arch: 'x64' })).toThrow(
      `WINDOWS_SIGNING_${name}`,
    );
    expect(spawnSync).not.toHaveBeenCalled();
    expect(renameSync).not.toHaveBeenCalled();
  });

  // Regression: the second local `npm run package:desktop` failed with "Failed to install
  // desktop build tools". Pake installs its own packages inside its directory, and npm
  // cannot install over them. A kept tools directory with the pinned Pake needs no install.
  it('does not install the build tools again when the kept tools directory has them', () => {
    const fresh = readFileSync.getMockImplementation();
    let version = PAKE_VERSION;
    readFileSync.mockImplementation((path, ...rest) =>
      String(path).endsWith(join('pake-cli', 'package.json'))
        ? JSON.stringify({ version })
        : fresh(path, ...rest),
    );
    try {
      const cli = prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
      expect(cli).toBe(join('/tools', 'node_modules', 'pake-cli', 'dist', 'cli.js'));
      expect(spawnSync).not.toHaveBeenCalled();
      expect(writeFileSync.mock.calls.some(([path]) => String(path).endsWith('artifact.rs'))).toBe(true);

      version = '3.0.0';
      prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
      expect(spawnSync).toHaveBeenCalledOnce();
      expect(spawnSync.mock.calls[0][1]).toContain(`pake-cli@${PAKE_VERSION}`);
    } finally {
      readFileSync.mockImplementation(fresh);
    }
  });

  it('stops before packaging when desktop tool installation fails', () => {
    spawnSync.mockReturnValueOnce({ status: 1 });
    expect(() =>
      prepareDesktopHost({ npmCli: 'npm', toolsDir: '/tools', env: process.env }),
    ).toThrow(/install desktop/);
    expect(writeFileSync).not.toHaveBeenCalled();
  });

  it('explains how to invoke npm on Windows before touching the build', () => {
    vi.stubEnv('npm_execpath', '');
    expect(() => packageDesktop({ platform: 'win32', arch: 'x64' })).toThrow(
      /npm run package:desktop/,
    );
    expect(existsSync).not.toHaveBeenCalled();
    expect(spawnSync).not.toHaveBeenCalled();
  });

  it.each([
    ['win32', 'arm64'],
    ['linux', 'ia32'],
    ['linux', 'arm'],
    ['linux', 's390x'],
    ['darwin', 'ia32'],
    ['freebsd', 'x64'],
  ])('refuses %s/%s before inspecting or modifying the build', (platform, arch) => {
    expect(() => packageDesktop({ platform, arch, log: vi.fn() })).toThrow(/no desktop target/);
    expect(existsSync).not.toHaveBeenCalled();
    expect(syncPackageVersion).not.toHaveBeenCalled();
    expect(spawnSync).not.toHaveBeenCalled();
  });

});

describe('desktop development builds', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    existsSync.mockReturnValue(true);
  });

  it.each([
    ['darwin', 'arm64', 'dmg', 'macos-arm64'],
    ['darwin', 'x64', 'dmg', 'macos-x64'],
    ['linux', 'arm64', 'AppImage', 'linux-arm64'],
    ['linux', 'x64', 'AppImage', 'linux-x64'],
    ['win32', 'x64', 'msi', 'windows-x64'],
  ])('builds only the local %s/%s app with a distinct identity', (platform, arch, ext, asset) => {
    const assets = packageDesktop({ platform, arch, dev: true, log: vi.fn() });
    expect(assets).toEqual([
      join('/app', 'build', 'desktop-dev', `vis-companion-1.0.0-${asset}.${ext}`),
    ]);
    expect(spawnSync).toHaveBeenCalledTimes(2);
    const [command, args, options] = spawnSync.mock.calls[1];
    expect(command).toBe(process.execPath);
    expect(args).not.toContain('--multi-arch');
    expect(args).toContain(
      platform === 'darwin' ? 'dmg' : platform === 'win32' ? 'x64' : 'appimage',
    );
    expect(args[args.indexOf('--identifier') + 1]).toBe('com.blockether.viscompanion.desktop.dev');
    expect(options.cwd).toBe(join('/app', 'build', 'desktop-dev'));
  });

  it('does not inherit release signing or notarization credentials', () => {
    const credentials = [
      'APPLE_SIGNING_IDENTITY',
      'APPLE_CERTIFICATE',
      'APPLE_CERTIFICATE_PASSWORD',
      'APPLE_API_KEY',
      'APPLE_API_KEY_PATH',
      'APPLE_API_ISSUER',
      'APPLE_ID',
      'APPLE_PASSWORD',
      'APPLE_TEAM_ID',
    ];
    try {
      for (const name of credentials) vi.stubEnv(name, 'test-only');
      packageDesktop({ platform: 'darwin', arch: 'arm64', dev: true, log: vi.fn() });
      const { env } = spawnSync.mock.calls[0][2];
      for (const name of credentials) expect(env?.[name], name).toBeUndefined();
      expect(env.PATH).toBe(process.env.PATH);
      packageDesktop({ platform: 'darwin', arch: 'arm64', log: vi.fn() });
      const releaseEnv = spawnSync.mock.calls[2][2].env;
      for (const name of credentials) {
        expect(releaseEnv[name], name).toBe('test-only');
        expect(process.env[name], name).toBe('test-only');
      }
    } finally {
      vi.unstubAllEnvs();
    }
  });

  it.each(['linux', 'win32'])(
    'does not install a stale %s artifact when compilation fails',
    (platform) => {
      spawnSync.mockReturnValueOnce({ status: 0 }).mockReturnValueOnce({ status: 1 });
      expect(() => packageDesktop({ platform, arch: 'x64', dev: true, log: vi.fn() })).toThrow(
        /failed/,
      );
      expect(renameSync).not.toHaveBeenCalled();
    },
  );

  it.each(['linux', 'win32'])('rejects a successful %s build with no installer', (platform) => {
    const produced = platform === 'win32' ? 'Vis.msi' : 'vis.AppImage';
    existsSync.mockImplementation((path) => basename(path) !== produced);
    expect(() => packageDesktop({ platform, arch: 'x64', dev: true, log: vi.fn() })).toThrow(
      /missing/,
    );
    expect(renameSync).not.toHaveBeenCalled();
  });
});

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
        return 'mod util;\nfn run() { builder.invoke_handler(tauri::generate_handler![download_file,]); }\n';
      }
      if (String(args[0]).endsWith('tauri.windows.conf.json')) {
        return JSON.stringify({
          bundle: { targets: ['msi'], windows: { wix: { language: ['en-US'] } } },
        });
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

  it('packages the local bundle, keeps the native title bar and builds macOS universal', () => {
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
    expect(args).not.toContain('--hide-title-bar');
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
    expect(linux.slice(linux.indexOf('--targets'), linux.indexOf('--targets') + 2)).toEqual([
      '--targets',
      'deb',
    ]);
  });

  it('registers the workbook command in every private desktop template', () => {
    vi.clearAllMocks();
    const cli = prepareDesktopHost({ npmCli: '/npm-cli.js', toolsDir: '/tools', env: process.env });
    expect(cli).toBe(join('/tools', 'node_modules', 'pake-cli', 'dist', 'cli.js'));
    const [libPath, lib] = writeFileSync.mock.calls.find(([path]) => String(path).endsWith('lib.rs'));
    expect(libPath).toBe(join('/tools', 'node_modules', 'pake-cli', 'src-tauri', 'src', 'lib.rs'));
    expect(lib).toContain('mod artifact;\nuse artifact::{open_xlsx, reveal_artifact};');
    expect(lib).toContain(
      '.invoke_handler(tauri::generate_handler![\n            open_xlsx,\n            reveal_artifact,',
    );
    const [, command] = writeFileSync.mock.calls.find(([path]) =>
      String(path).endsWith('artifact.rs'),
    );
    expect(command).toContain('pub async fn open_xlsx(');
    expect(command).toContain('stage_workbook(&downloads, &filename, &data)');
    expect(command).toContain('.open_path(');
    expect(command).toContain('pub async fn reveal_artifact(');
    expect(command).toContain('.reveal_item_in_dir(&path)');
    expect(
      writeFileSync.mock.calls.some(([path]) => String(path).endsWith('tauri.windows.conf.json')),
    ).toBe(false);
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

import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync, renameSync, writeFileSync } from 'node:fs';
import { basename, join, resolve } from 'node:path';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
  desktopTargets,
  assetName,
  pakeArgs,
  packageDesktop,
  prepareWindowsSigning,
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
  it('keeps executable modules LF-terminated on Windows checkouts', () => {
    // CRLF hashbangs broke Vite's module loader before the Windows suite could run.
    const attributes = readFileSync(new URL('.gitattributes', import.meta.url), 'utf8');
    expect(attributes.split(/\r?\n/)).toContain('*.mjs text eol=lf');
  });

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
});

// v0.1.43 ran before checkout on fresh Linux runners and had no ARM64 package job.
const workflow = readFileSync(
  new URL('../../../.github/workflows/desktop-companion.yml', import.meta.url),
  'utf8',
).replace(/\r\n/g, '\n');

describe('desktop release signing', () => {
  it('requires macOS signing credentials and notarization before uploading', () => {
    for (const name of [
      'VIS_DESKTOP_P12',
      'VIS_DESKTOP_P12_PASSWORD',
      'VIS_ASC_KEY_ID',
      'VIS_ASC_ISSUER_ID',
      'VIS_ASC_KEY',
    ]) {
      expect(workflow).toContain(`secrets.${name}`);
    }
    expect(workflow).toContain('Missing required signing credential: $name');
    expect(workflow).toContain("APPLE_SIGNING_IDENTITY: 'Developer ID Application:");
    expect(workflow).toContain('export APPLE_API_KEY_PATH=');
    expect(workflow).toContain('trap cleanup EXIT');
    expect(workflow).toContain('security list-keychains -d user -s "${original_keychains[@]}"');
    expect(workflow).toContain('security delete-keychain "$keychain"');
    // The first signed CI run failed with errSecInternalComponent on the shared Mac.
    // Keep other applications' keychain credentials available while signing.
    expect(workflow).toContain(
      'security list-keychains -d user -s "$keychain" "${original_keychains[@]}"',
    );
    expect(workflow).toContain('security set-key-partition-list -S apple-tool:,apple:,codesign:');
    expect(workflow).toContain('for certificate in DeveloperIDCA DeveloperIDG2CA');
    expect(workflow).toContain('unset APPLE_CERTIFICATE APPLE_CERTIFICATE_PASSWORD VIS_ASC_KEY');
    expect(workflow.indexOf('codesign --force --timestamp')).toBeLessThan(
      workflow.lastIndexOf('npm run package:desktop'),
    );
    for (const check of [
      'codesign --verify --deep --strict',
      'xcrun stapler validate',
      'spctl --assess --type execute',
    ]) {
      expect(workflow.indexOf(check)).toBeGreaterThan(
        workflow.indexOf('name: Sign and notarize macOS'),
      );
      expect(workflow.indexOf(check)).toBeLessThan(
        workflow.indexOf('uses: actions/upload-artifact'),
      );
    }
    expect(workflow).toContain('name: Package Linux with Pake');
    expect(workflow).toContain("if: runner.os == 'Linux'");
  });

  it('grants release caller permissions for nested OIDC signing and asset upload', () => {
    const release = readFileSync(
      new URL('../../../.github/workflows/release.yml', import.meta.url),
      'utf8',
    );
    const desktop = release.match(
      /^  desktop:\r?\n([\s\S]*?)(?=^  [a-zA-Z_-]+:|$(?![\s\S]))/m,
    )?.[1];
    expect(desktop).toBeDefined();
    expect(desktop).toContain('uses: ./.github/workflows/desktop-companion.yml');
    expect(desktop).toMatch(/    permissions:\r?\n      contents: write\r?\n      id-token: write/);
  });

  it('signs with OIDC and verifies the MSI and extracted EXE before smoke and upload', () => {
    for (const name of [
      'CLIENT_ID',
      'TENANT_ID',
      'SUBSCRIPTION_ID',
      'ENDPOINT',
      'ACCOUNT',
      'PROFILE',
      'PUBLISHER',
    ]) {
      expect(workflow).toContain(`vars.WINDOWS_SIGNING_${name}`);
    }
    expect(workflow).toContain(
      "environment: ${{ matrix.asset == 'windows-x64' && 'windows-signing' || 'desktop-build' }}",
    );
    const login = workflow.indexOf('uses: azure/login@v3');
    const packaging = workflow.indexOf('name: Sign and package Windows with Pake');
    const installerCheck = workflow.indexOf('-FilePath $installers[0].FullName -VerifyOnly');
    const exeCheck = workflow.indexOf('-FilePath $executables[0].FullName -VerifyOnly');
    const launch = workflow.indexOf('$app = Start-Process');
    const upload = workflow.indexOf('uses: actions/upload-artifact');
    expect(login).toBeGreaterThan(0);
    expect(packaging).toBeGreaterThan(login);
    expect(installerCheck).toBeGreaterThan(packaging);
    expect(exeCheck).toBeGreaterThan(installerCheck);
    expect(launch).toBeGreaterThan(exeCheck);
    expect(upload).toBeGreaterThan(launch);
    const signing = readFileSync(new URL('windows-sign.ps1', import.meta.url), 'utf8');
    expect(signing).toContain("FileDigest = 'SHA256'");
    expect(signing).toContain("TimestampDigest = 'SHA256'");
    expect(signing).toContain("TimestampRfc3161 = 'http://timestamp.acs.microsoft.com'");
    expect(signing).toContain('ExcludeAzureCliCredential = $false');
    for (const credential of [
      'Environment',
      'WorkloadIdentity',
      'ManagedIdentity',
      'SharedTokenCache',
      'VisualStudio',
      'VisualStudioCode',
      'AzurePowerShell',
      'AzureDeveloperCli',
      'InteractiveBrowser',
    ]) {
      expect(signing).toContain(`Exclude${credential}Credential = $true`);
    }
    expect(signing.indexOf('Invoke-ArtifactSigning @parameters')).toBeLessThan(
      signing.indexOf('Assert-VisSignature -Path $Path -Publisher'),
    );
  });
});

describe('desktop release platforms', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    existsSync.mockReturnValue(true);
  });

  it('uses an absolute reusable Cargo target outside the disposable npx installation', () => {
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
    expect(spawnSync).toHaveBeenCalledTimes(2);
    for (const [command, args] of spawnSync.mock.calls) {
      expect(command).toBe('npx');
      expect(args).not.toContain('--multi-arch');
    }
  });

  it.each(['x64', 'arm64'])('packages universal macOS on a %s host', (arch) => {
    const assets = packageDesktop({ platform: 'darwin', arch, log: vi.fn() });
    expect(assets.map((asset) => basename(asset))).toEqual([
      'vis-companion-1.0.0-macos-universal.dmg',
    ]);
    expect(renameSync.mock.calls.map(([source]) => basename(source))).toEqual(['Vis.dmg']);
    expect(spawnSync).toHaveBeenCalledTimes(1);
    expect(spawnSync.mock.calls[0][1]).toContain('--multi-arch');
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
    const [configPath, content] = writeFileSync.mock.calls[0];
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

  it('stops before packaging when signing tool installation fails', () => {
    spawnSync.mockReturnValueOnce({ status: 1 });
    expect(() =>
      prepareWindowsSigning({ npmCli: 'npm', toolsDir: '/tools', env: process.env }),
    ).toThrow(/install Windows/);
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

  it('runs universal macOS, native x64/ARM64 Linux and Windows x64 builders', () => {
    expect([...workflow.matchAll(/^\s+label: (.+)$/gm)].map((match) => match[1])).toEqual([
      'macOS universal',
      'Linux x64',
      'Linux ARM64',
      'Windows x64',
    ]);
    // Only macOS is self-hosted; other platforms use native hosted runners.
    expect(workflow).toContain('runs-on: ${{ matrix.runner }}');
    expect([...workflow.matchAll(/^\s+- runner: (.+)$/gm)].map((match) => match[1])).toEqual([
      '[self-hosted, macOS, ARM64, vis-macos-arm64]',
      'ubuntu-24.04',
      'ubuntu-24.04-arm',
      'windows-2022',
    ]);
    expect(workflow).not.toMatch(/VIS_CONTAINER_|--linux|Podman|Docker/);
    expect(workflow).toContain('runner: ubuntu-24.04');
    expect(workflow).toContain('runner: ubuntu-24.04-arm');
    expect(workflow).toContain('asset: windows-x64');
    expect(workflow).toContain('uses: azure/login@v3');
    expect(workflow).toContain('id-token: write');
    expect(workflow).not.toContain('azure-client-secret');
    const smoke = workflow.indexOf('name: Smoke-test Windows installer');
    expect(smoke).toBeGreaterThan(workflow.indexOf('name: Sign and package Windows with Pake'));
    expect(smoke).toBeLessThan(workflow.indexOf('uses: actions/upload-artifact'));
    expect(workflow).toContain('shell: pwsh');
    expect(workflow).toContain('msiexec.exe');
    expect(workflow).toContain('MainWindowHandle');
    expect(workflow).toContain('Stop-Process');
    expect(workflow).toContain('aarch64-apple-darwin,x86_64-apple-darwin');
    expect(workflow).toContain('name: vis-companion-desktop-${{ matrix.asset }}');
  });

  it('installs xdg-open explicitly for Linux AppImage bundling', () => {
    // The ARM64 hosted runner lacks the xdg-utils package present on x64.
    const dependencies = workflow
      .split('name: Install Linux WebKit build dependencies')[1]
      .split('      - name:')[0];
    expect(dependencies).toContain("if: runner.os == 'Linux'");
    expect(dependencies).toMatch(/\bxdg-utils\b/);
  });

  it('checks out the project before any shell step uses its working directory', () => {
    const checkout = workflow.indexOf('uses: actions/checkout@');
    const firstRun = workflow.indexOf('\n        run:');
    expect(checkout).toBeGreaterThan(-1);
    expect(firstRun).toBeGreaterThan(checkout);
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
    expect(spawnSync).toHaveBeenCalledTimes(1);
    const [command, args, options] = spawnSync.mock.calls[0];
    expect(command).toBe(platform === 'win32' ? process.execPath : 'npx');
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
      const releaseEnv = spawnSync.mock.calls[1][2].env;
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
      spawnSync.mockReturnValueOnce({ status: 1 });
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

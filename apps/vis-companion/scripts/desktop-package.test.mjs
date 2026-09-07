import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync, renameSync } from 'node:fs';
import { basename } from 'node:path';
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { desktopTargets, assetName, pakeArgs, packageDesktop } from './desktop-package.mjs';
import { syncPackageVersion } from './version.mjs';

vi.mock('node:child_process', () => ({ spawnSync: vi.fn(() => ({ status: 0 })) }));
vi.mock('node:fs', async (importOriginal) => ({
  ...await importOriginal(),
  existsSync: vi.fn(() => true),
  mkdirSync: vi.fn(),
  renameSync: vi.fn(),
  rmSync: vi.fn(),
}));
vi.mock('./version.mjs', () => ({ appDir: '/app', syncPackageVersion: vi.fn(() => '1.0.0') }));

// The installers are release assets: a name must say which OS and arch it is for,
// and every OS the workflow runs on must have at least one target to build.
describe('desktop package', () => {
  it('names an asset by version, platform and installer type', () => {
    expect(assetName('0.1.30', desktopTargets('darwin', 'arm64')[0])).toBe('vis-companion-0.1.30-macos-universal.dmg');
    expect(desktopTargets('linux', 'x64').map((t) => assetName('0.1.30', t))).toEqual([
      'vis-companion-0.1.30-linux-x64.deb',
      'vis-companion-0.1.30-linux-x64.AppImage',
    ]);
  });

  it('covers every release runner OS with distinct asset names', () => {
    const targets = [['darwin', 'arm64'], ['linux', 'x64'], ['linux', 'arm64']].flatMap(([os, arch]) => desktopTargets(os, arch));
    const names = targets.map((t) => assetName('1.0.0', t));
    expect(names).toHaveLength(5);
    expect(new Set(names).size).toBe(names.length);
  });

  it('packages the local bundle, keeps the native title bar and builds macOS universal', () => {
    const args = pakeArgs({ distDir: '/tmp/dist', version: '0.1.30', target: desktopTargets('darwin', 'arm64')[0], icon: '/i.png' });
    expect(args[0]).toBe('/tmp/dist');
    expect(args).toContain('--use-local-file');
    expect(args).toContain('--multi-arch');
    expect(args).not.toContain('--hide-title-bar');
    expect(args.slice(args.indexOf('--app-version'), args.indexOf('--app-version') + 2)).toEqual(['--app-version', '0.1.30']);
    const linux = pakeArgs({ distDir: '/tmp/dist', version: '0.1.30', target: desktopTargets('linux', 'arm64')[0], icon: '/i.png' });
    expect(linux).not.toContain('--multi-arch');
    expect(linux.slice(linux.indexOf('--targets'), linux.indexOf('--targets') + 2)).toEqual(['--targets', 'deb']);
  });
});

// v0.1.43 ran before checkout on fresh Linux runners and had no ARM64 package job.
const workflow = readFileSync(new URL('../../../.github/workflows/desktop-companion.yml', import.meta.url), 'utf8');

describe('desktop release platforms', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    existsSync.mockReturnValue(true);
  });

  it.each(['x64', 'arm64'])('packages native Linux %s with truthful asset names', (arch) => {
    // Pake lowercases Linux names; the runner filesystem is case-sensitive.
    existsSync.mockImplementation((path) => ['index.html', 'vis.deb', 'vis.AppImage'].includes(basename(path)));
    const assets = packageDesktop({ platform: 'linux', arch, log: vi.fn() });
    expect(assets.map((asset) => basename(asset))).toEqual([
      `vis-companion-1.0.0-linux-${arch}.deb`,
      `vis-companion-1.0.0-linux-${arch}.AppImage`,
    ]);
    expect(renameSync.mock.calls.map(([source]) => basename(source))).toEqual(['vis.deb', 'vis.AppImage']);
    expect(spawnSync).toHaveBeenCalledTimes(2);
    for (const [command, args] of spawnSync.mock.calls) {
      expect(command).toBe('npx');
      expect(args).not.toContain('--multi-arch');
    }
  });

  it.each(['x64', 'arm64'])('packages universal macOS on a %s host', (arch) => {
    const assets = packageDesktop({ platform: 'darwin', arch, log: vi.fn() });
    expect(assets.map((asset) => basename(asset))).toEqual(['vis-companion-1.0.0-macos-universal.dmg']);
    expect(renameSync.mock.calls.map(([source]) => basename(source))).toEqual(['Vis.dmg']);
    expect(spawnSync).toHaveBeenCalledTimes(1);
    expect(spawnSync.mock.calls[0][1]).toContain('--multi-arch');
  });

  it.each([
    ['win32', 'x64'], ['win32', 'arm64'], ['linux', 'ia32'], ['linux', 'arm'],
    ['linux', 's390x'], ['darwin', 'ia32'], ['freebsd', 'x64'],
  ])('refuses %s/%s before inspecting or modifying the build', (platform, arch) => {
    expect(() => packageDesktop({ platform, arch, log: vi.fn() })).toThrow(/no desktop target/);
    expect(existsSync).not.toHaveBeenCalled();
    expect(syncPackageVersion).not.toHaveBeenCalled();
    expect(spawnSync).not.toHaveBeenCalled();
  });

  it('runs only universal macOS and native x64/ARM64 Linux builders', () => {
    expect([...workflow.matchAll(/^\s+label: (.+)$/gm)].map((match) => match[1])).toEqual([
      'macOS universal', 'Linux x64', 'Linux ARM64',
    ]);
    expect(workflow).toContain('runner: ubuntu-24.04');
    expect(workflow).toContain('runner: ubuntu-24.04-arm');
    expect(workflow).not.toMatch(/windows|win32/i);
    expect(workflow).toContain('aarch64-apple-darwin,x86_64-apple-darwin');
    expect(workflow).toContain('name: vis-companion-desktop-${{ matrix.asset }}');
  });

  it('installs xdg-open explicitly for Linux AppImage bundling', () => {
    // The ARM64 hosted runner lacks the xdg-utils package present on x64.
    const dependencies = workflow.split('name: Install Linux WebKit build dependencies')[1].split('      - name:')[0];
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

import { describe, expect, it } from 'vitest';
import { DESKTOP_TARGETS, assetName, pakeArgs } from './desktop-package.mjs';

// The installers are release assets: a name must say which OS and arch it is for,
// and every OS the workflow runs on must have at least one target to build.
describe('desktop package', () => {
  it('names an asset by version, platform and installer type', () => {
    expect(assetName('0.1.30', DESKTOP_TARGETS.darwin[0])).toBe('vis-companion-0.1.30-macos-universal.dmg');
    expect(assetName('0.1.30', DESKTOP_TARGETS.win32[0])).toBe('vis-companion-0.1.30-windows-x64.msi');
    expect(DESKTOP_TARGETS.linux.map((t) => assetName('0.1.30', t))).toEqual([
      'vis-companion-0.1.30-linux-x64.deb',
      'vis-companion-0.1.30-linux-x64.AppImage',
    ]);
  });

  it('covers every release runner OS with distinct asset names', () => {
    expect(Object.keys(DESKTOP_TARGETS).sort()).toEqual(['darwin', 'linux', 'win32']);
    const names = Object.values(DESKTOP_TARGETS).flat().map((t) => assetName('1.0.0', t));
    expect(new Set(names).size).toBe(names.length);
  });

  it('packages the local bundle, keeps the native title bar and builds macOS universal', () => {
    const args = pakeArgs({ distDir: '/tmp/dist', version: '0.1.30', target: DESKTOP_TARGETS.darwin[0], icon: '/i.png' });
    expect(args[0]).toBe('/tmp/dist');
    expect(args).toContain('--use-local-file');
    expect(args).toContain('--multi-arch');
    expect(args).not.toContain('--hide-title-bar');
    expect(args.slice(args.indexOf('--app-version'), args.indexOf('--app-version') + 2)).toEqual(['--app-version', '0.1.30']);
    const win = pakeArgs({ distDir: '/tmp/dist', version: '0.1.30', target: DESKTOP_TARGETS.win32[0], icon: '/i.png' });
    expect(win).not.toContain('--multi-arch');
    expect(win.slice(win.indexOf('--targets'), win.indexOf('--targets') + 2)).toEqual(['--targets', 'x64']);
  });
});

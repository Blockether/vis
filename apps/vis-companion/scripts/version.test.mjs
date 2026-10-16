import { readFileSync, writeFileSync } from 'node:fs';
import { basename } from 'node:path';
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { syncInstallerLinks, syncPythonVersion } from './version.mjs';

vi.mock('node:fs', () => ({ readFileSync: vi.fn(), writeFileSync: vi.fn() }));

beforeEach(() => vi.clearAllMocks());

function sdkSource(text) {
  readFileSync.mockImplementation((file) => (basename(file) === 'VIS_VERSION' ? '1.2.3\n' : text));
}

// Windows checkouts use CRLF in pyproject.toml; prebuild must accept that without
// rewriting the rest of the file or turning an unchanged version into an error.
describe.each([
  ['LF', '\n'],
  ['CRLF', '\r\n'],
])('Python version mirroring with %s', (_label, newline) => {
  it('updates the version while preserving line endings and unrelated content', () => {
    const original = ['[project]', 'version = "0.1.0"', 'name = "vis-agent"', ''].join(newline);
    sdkSource(original);
    expect(syncPythonVersion({ quiet: true })).toBe('1.2.3');
    expect(writeFileSync).toHaveBeenCalledExactlyOnceWith(
      expect.stringContaining('pyproject.toml'),
      original.replace('version = "0.1.0"', 'version = "1.2.3"'),
    );
  });

  it('does not write an already synchronized version', () => {
    sdkSource(['[project]', 'version = "1.2.3"', ''].join(newline));
    expect(syncPythonVersion({ quiet: true })).toBe('1.2.3');
    expect(writeFileSync).not.toHaveBeenCalled();
  });
});

it('accepts a final version line without a terminating newline', () => {
  sdkSource('[project]\nversion = "1.2.3"');
  expect(syncPythonVersion({ quiet: true })).toBe('1.2.3');
  expect(writeFileSync).not.toHaveBeenCalled();
});

it('rejects a missing version without writing the file', () => {
  sdkSource('[project]\nname = "vis-agent"\n');
  expect(() => syncPythonVersion({ quiet: true })).toThrow(/could not rewrite "version"/);
  expect(writeFileSync).not.toHaveBeenCalled();
});

describe('installer link mirroring', () => {
  const link = (v) => `https://github.com/Blockether/vis/releases/download/v${v}/vis-companion-${v}-windows-x64.msi`;
  const readme = (v) => `[Windows](${link(v)})\n<a href="${link(v)}">x</a>\n[latest](https://github.com/Blockether/vis/releases/latest)\n`;

  it('points every desktop installer link at the VIS_VERSION release', () => {
    sdkSource(readme('0.1.0'));
    expect(syncInstallerLinks({ quiet: true, files: ['/repo/README.md'] })).toBe('1.2.3');
    expect(writeFileSync).toHaveBeenCalledExactlyOnceWith('/repo/README.md', readme('1.2.3'));
  });

  it('does not write links that already point to the release', () => {
    sdkSource(readme('1.2.3'));
    expect(syncInstallerLinks({ quiet: true, files: ['/repo/README.md'] })).toBe('1.2.3');
    expect(writeFileSync).not.toHaveBeenCalled();
  });

  it('rejects a file without an installer link', () => {
    sdkSource('No installers here.\n');
    expect(() => syncInstallerLinks({ quiet: true, files: ['/repo/index.md'] })).toThrow(/no desktop installer link/);
    expect(writeFileSync).not.toHaveBeenCalled();
  });
});

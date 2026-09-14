import { readFileSync, writeFileSync } from 'node:fs';
import { basename } from 'node:path';
import { beforeEach, describe, expect, it, vi } from 'vitest';
import { syncPythonVersion } from './version.mjs';

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

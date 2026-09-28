import { spawnSync } from 'node:child_process';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { ascCredentials, keychain, secret, unhex } from './keychain.mjs';

vi.mock('node:child_process', () => ({ spawnSync: vi.fn() }));

const PEM = '-----BEGIN PRIVATE KEY-----\nMIGTAgEAMBMGByqGSM49\n-----END PRIVATE KEY-----';
const hex = (text) => Buffer.from(text, 'utf8').toString('hex');
const platform = Object.getOwnPropertyDescriptor(process, 'platform');
const onPlatform = (value) => Object.defineProperty(process, 'platform', { ...platform, value });

// `security find-generic-password -s <service> -a <account> -w`, answered from `items`.
const stored = (items) =>
  spawnSync.mockImplementation((_cmd, args) => {
    const value = items[`${args[2]}/${args[4]}`];
    return value === undefined ? { status: 44, stdout: '' } : { status: 0, stdout: `${value}\n` };
  });

beforeEach(() => {
  vi.clearAllMocks();
  onPlatform('darwin');
  for (const name of ['VIS_ASC_KEY', 'VIS_ASC_KEY_ID', 'VIS_ASC_ISSUER_ID', 'VIS_ASC_KEY_PATH']) {
    vi.stubEnv(name, undefined);
  }
  stored({});
});

afterEach(() => {
  Object.defineProperty(process, 'platform', platform);
  vi.unstubAllEnvs();
});

describe('keychain', () => {
  it('reads one item and decodes the hex `security` prints for a multi-line PEM', () => {
    stored({ 'vis-ios/asc_key': hex(PEM) });
    expect(keychain('vis-ios', 'asc_key')).toBe(PEM);
    expect(spawnSync).toHaveBeenCalledExactlyOnceWith(
      'security',
      ['find-generic-password', '-s', 'vis-ios', '-a', 'asc_key', '-w'],
      { encoding: 'utf8' },
    );
  });

  it('keeps plain values and short hex-looking values as stored', () => {
    stored({ 'vis-ios/team_id': 'JSZTFUBUBB', 'vis-android/key_alias': 'cafe01' });
    expect(keychain('vis-ios', 'team_id')).toBe('JSZTFUBUBB');
    expect(keychain('vis-android', 'key_alias')).toBe('cafe01');
    expect(unhex('abc')).toBe('abc');
  });

  it('answers undefined for a missing item and never asks outside macOS', () => {
    expect(keychain('vis-play', 'service_account')).toBeUndefined();
    onPlatform('linux');
    spawnSync.mockClear();
    expect(keychain('vis-play', 'service_account')).toBeUndefined();
    expect(spawnSync).not.toHaveBeenCalled();
  });
});

describe('secret', () => {
  it('prefers a trimmed environment variable and leaves the keychain alone', () => {
    vi.stubEnv('VIS_PLAY_SERVICE_ACCOUNT', '  {"type":"service_account"}\n');
    expect(secret('VIS_PLAY_SERVICE_ACCOUNT', 'vis-play', 'service_account')).toBe(
      '{"type":"service_account"}',
    );
    expect(spawnSync).not.toHaveBeenCalled();
  });

  it('falls back to the keychain when the variable is blank', () => {
    vi.stubEnv('VIS_ANDROID_KEY_ALIAS', '  ');
    stored({ 'vis-android/key_alias': 'upload' });
    expect(secret('VIS_ANDROID_KEY_ALIAS', 'vis-android', 'key_alias')).toBe('upload');
  });
});

describe('ascCredentials', () => {
  it('reads the whole App Store Connect key from the vis-ios keychain entries', () => {
    stored({
      'vis-ios/asc_key_id': 'KEY123',
      'vis-ios/asc_issuer_id': 'issuer',
      'vis-ios/asc_key': hex(PEM),
    });
    expect(ascCredentials()).toEqual({ keyId: 'KEY123', issuerId: 'issuer', keyPem: PEM });
  });

  it('reads the key from VIS_ASC_KEY_PATH, and prefers inline VIS_ASC_KEY over both', () => {
    const dir = mkdtempSync(join(tmpdir(), 'vis-keychain-test-'));
    try {
      const path = join(dir, 'AuthKey_KEY123.p8');
      writeFileSync(path, `${PEM}\n`);
      vi.stubEnv('VIS_ASC_KEY_ID', 'KEY123');
      vi.stubEnv('VIS_ASC_ISSUER_ID', 'issuer');
      vi.stubEnv('VIS_ASC_KEY_PATH', path);
      expect(ascCredentials()).toEqual({ keyId: 'KEY123', issuerId: 'issuer', keyPem: `${PEM}\n` });
      vi.stubEnv('VIS_ASC_KEY', `${PEM}\n`);
      expect(ascCredentials().keyPem).toBe(PEM);
      expect(spawnSync).not.toHaveBeenCalled();
    } finally {
      rmSync(dir, { recursive: true, force: true });
    }
  });
});

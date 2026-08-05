import { describe, expect, it } from 'vitest';
import {
  crashFileArgs,
  deviceCopyArgs,
  deviceFileCandidates,
  deviceFileListArgs,
  feedbackEntries,
  isMissingCrashLog,
  isVisAppDiagnostic,
  selectPhysicalDevice,
} from './ios-crashes.mjs';

describe('iOS crash collection', () => {
  it('normalizes TestFlight feedback with its build and filters old reports', () => {
    const response = {
      data: [
        {
          id: 'fresh',
          attributes: {
            createdDate: '2026-08-05T10:00:00Z',
            comment: 'froze',
            screenshots: [{ url: 'https://example.test/shot.jpg' }],
          },
          relationships: { build: { data: { id: 'build-1' } } },
        },
        {
          id: 'old',
          attributes: { createdDate: '2026-07-01T10:00:00Z' },
          relationships: { build: { data: { id: 'build-0' } } },
        },
      ],
      included: [
        { id: 'build-1', type: 'builds', attributes: { version: '3423' } },
        { id: 'build-0', type: 'builds', attributes: { version: '2800' } },
      ],
    };

    expect(feedbackEntries(response, 'screenshot', Date.parse('2026-08-01T00:00:00Z'))).toEqual([
      expect.objectContaining({
        id: 'fresh',
        kind: 'screenshot',
        build: '3423',
        createdDate: '2026-08-05T10:00:00Z',
        screenshots: [{ url: 'https://example.test/shot.jpg' }],
        attributes: { createdDate: '2026-08-05T10:00:00Z', comment: 'froze' },
      }),
    ]);
  });

  it('treats an absent Apple crash log as expected for a hang or OOM report', () => {
    expect(isMissingCrashLog(Object.assign(new Error('not found'), { status: 404 }))).toBe(true);
    expect(isMissingCrashLog(Object.assign(new Error('forbidden'), { status: 403 }))).toBe(false);
  });

  it('lists root and Retired diagnostics without copying the whole crash-log domain', () => {
    expect(deviceFileListArgs('device-id', undefined, '/tmp/root.json')).toEqual([
      'devicectl',
      'device',
      'info',
      'files',
      '--device',
      'device-id',
      '--domain-type',
      'systemCrashLogs',
      '--recurse',
      '--filter',
      "Name BEGINSWITH 'App-' OR Name BEGINSWITH 'JetsamEvent-' OR Name BEGINSWITH 'stacks-'",
      '--json-output',
      '/tmp/root.json',
    ]);
    expect(deviceFileListArgs('device-id', 'Retired', '/tmp/retired.json')).toContain('Retired');
    expect(deviceCopyArgs('device-id', 'Retired/App-2026-08-05.ips', '/tmp/report.ips', '/tmp/copy.json')).toEqual([
      'devicectl',
      'device',
      'copy',
      'from',
      '--device',
      'device-id',
      '--domain-type',
      'systemCrashLogs',
      '--source',
      'Retired/App-2026-08-05.ips',
      '--destination',
      '/tmp/report.ips',
      '--json-output',
      '/tmp/copy.json',
    ]);
  });

  it('keeps recent candidates, prefixes Retired paths, and deduplicates them', () => {
    const file = (name, date) => ({
      name,
      relativePath: name,
      metadata: { lastModDate: date, size: 42 },
      resources: { isDirectory: false },
    });
    const groups = [
      { subdirectory: undefined, files: [file('JetsamEvent-new.ips', '2026-08-05T09:00:00Z')] },
      {
        subdirectory: 'Retired',
        files: [
          file('App-new.ips', '2026-08-05T08:00:00Z'),
          file('App-old.ips', '2026-07-01T08:00:00Z'),
          file('notes.txt', '2026-08-05T08:00:00Z'),
        ],
      },
    ];

    expect(deviceFileCandidates(groups, Date.parse('2026-08-01T00:00:00Z'))).toEqual([
      expect.objectContaining({ source: 'JetsamEvent-new.ips', size: 42 }),
      expect.objectContaining({ source: 'Retired/App-new.ips', size: 42 }),
    ]);
  });

  it('selects only a paired physical iOS device and accepts identifier or UDID', () => {
    const physical = {
      identifier: 'core-device-id',
      deviceProperties: { name: 'My iPhone' },
      hardwareProperties: { platform: 'iOS', reality: 'physical', udid: 'phone-udid' },
      connectionProperties: { pairingState: 'paired' },
    };
    const simulator = {
      identifier: 'sim',
      hardwareProperties: { platform: 'iOS', reality: 'virtual' },
      connectionProperties: { pairingState: 'paired' },
    };

    expect(selectPhysicalDevice([simulator, physical], undefined)).toBe(physical);
    expect(selectPhysicalDevice([physical], 'phone-udid')).toBe(physical);
    expect(() => selectPhysicalDevice([physical], 'missing')).toThrow(/not found/);
  });

  it('identifies Vis app crashes but keeps jetsam and stacks as contextual diagnostics', () => {
    expect(isVisAppDiagnostic('header\n"bundleID":"com.blockether.viscompanion"')).toBe(true);
    expect(isVisAppDiagnostic('header\nBundle Identifier: com.example.other')).toBe(false);
    expect(crashFileArgs('JetsamEvent-2026-08-05.ips')).toEqual({ keep: true, reason: 'system-memory' });
    expect(crashFileArgs('stacks-2026-08-05.ips')).toEqual({ keep: true, reason: 'system-stacks' });
  });
});

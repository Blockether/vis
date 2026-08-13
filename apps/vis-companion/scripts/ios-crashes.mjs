#!/usr/bin/env node
/**
 * Collect the two iOS diagnostic sources that caught Vis' TestFlight failures:
 *
 *   1. tester feedback (and any Apple crash log/screenshots) from App Store Connect;
 *   2. App, jetsam, and stack diagnostics from a paired physical iPhone.
 *
 * The device side deliberately lists and copies individual candidate files. Never copy the
 * whole `systemCrashLogs` domain: it also contains sysdiagnose archives hundreds of MB large.
 *
 * Usage:
 *   npm run diagnostics:ios
 *   npm run diagnostics:ios -- --days 3 --out /tmp/vis-ios
 *   npm run diagnostics:ios -- --asc-only
 *   npm run diagnostics:ios -- --device-only --device <CoreDevice id or UDID>
 */
import { spawnSync } from 'node:child_process';
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, dirname, extname, join, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { appIdFor, asc, ascToken } from './asc.mjs';

const APP_DIR = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const DEFAULT_BUNDLE_ID = 'com.blockether.viscompanion';
const DEFAULT_DAYS = 14;
const DEFAULT_LIMIT = 50;
const DEFAULT_DEVICE_LIMIT = 100;
const DIAGNOSTIC_FILTER =
  "Name BEGINSWITH 'App-' OR Name BEGINSWITH 'JetsamEvent-' OR Name BEGINSWITH 'stacks-'";
const DIAGNOSTIC_NAME = /^(App-|JetsamEvent-|stacks-)/i;

const unhex = (value) =>
  /^[0-9a-f]{32,}$/i.test(value) && value.length % 2 === 0
    ? Buffer.from(value, 'hex').toString('utf8')
    : value;

const keychain = (account) => {
  if (process.platform !== 'darwin') return undefined;
  const result = spawnSync(
    'security',
    ['find-generic-password', '-s', 'vis-ios', '-a', account, '-w'],
    { encoding: 'utf8' },
  );
  return result.status === 0 && result.stdout.trim()
    ? unhex(result.stdout.trim())
    : undefined;
};

const credentials = () => {
  const keyId = process.env.VIS_ASC_KEY_ID?.trim() || keychain('asc_key_id');
  const issuerId = process.env.VIS_ASC_ISSUER_ID?.trim() || keychain('asc_issuer_id');
  const keyPem =
    process.env.VIS_ASC_KEY?.trim() ||
    (process.env.VIS_ASC_KEY_PATH
      ? readFileSync(process.env.VIS_ASC_KEY_PATH, 'utf8')
      : keychain('asc_key'));
  if (!keyId || !issuerId || !keyPem) {
    throw new Error(
      'no App Store Connect API key; run `npm run secrets asc <AuthKey_XXXX.p8> --issuer <uuid> --team <id>`',
    );
  }
  return { keyId, issuerId, keyPem };
};

const safePart = (value) =>
  String(value ?? 'unknown')
    .replace(/[^a-z0-9._-]+/gi, '-')
    .replace(/^-+|-+$/g, '') || 'unknown';

const timestampPart = (date) =>
  new Date(date).toISOString().replace(/\.\d{3}Z$/, 'Z').replace(/:/g, '');

const writeJson = (path, value) =>
  writeFileSync(path, `${JSON.stringify(value, null, 2)}\n`, 'utf8');

/** Normalize ASC JSON:API feedback and attach the included build number. */
export const feedbackEntries = (response, kind, cutoff) => {
  const builds = new Map(
    (response.included ?? [])
      .filter((entry) => entry.type === 'builds')
      .map((entry) => [entry.id, entry.attributes]),
  );

  return (response.data ?? [])
    .filter((entry) => {
      const created = Date.parse(entry.attributes?.createdDate);
      return !Number.isFinite(created) || created >= cutoff;
    })
    .map((entry) => {
      const { screenshots = [], ...attributes } = entry.attributes ?? {};
      const buildId = entry.relationships?.build?.data?.id;
      return {
        kind,
        id: entry.id,
        createdDate: attributes.createdDate,
        build: builds.get(buildId)?.version,
        attributes,
        screenshots,
      };
    })
    .sort((a, b) => Date.parse(b.createdDate) - Date.parse(a.createdDate));
};

export const isMissingCrashLog = (error) => error?.status === 404;

const feedbackStem = (entry) =>
  `${timestampPart(entry.createdDate)}-build-${safePart(entry.build)}-${safePart(entry.id)}`;

const download = async (url, path) => {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`download ${new URL(url).origin} → ${response.status} ${response.statusText}`);
  }
  writeFileSync(path, Buffer.from(await response.arrayBuffer()));
};

const collectAscFeedback = async ({ bundleId, cutoff, limit, outDir }) => {
  mkdirSync(outDir, { recursive: true });
  const mint = () => ascToken(credentials());
  const appId = await appIdFor(mint, bundleId);
  if (!appId) throw new Error(`no App Store Connect app has bundle id ${bundleId}`);

  const query = `limit=${limit}&include=build`;
  const [crashResponse, screenshotResponse] = await Promise.all([
    asc(mint, 'GET', `/v1/apps/${appId}/betaFeedbackCrashSubmissions?${query}`),
    asc(mint, 'GET', `/v1/apps/${appId}/betaFeedbackScreenshotSubmissions?${query}`),
  ]);
  const entries = [
    ...feedbackEntries(crashResponse, 'crash', cutoff),
    ...feedbackEntries(screenshotResponse, 'screenshot', cutoff),
  ].sort((a, b) => Date.parse(b.createdDate) - Date.parse(a.createdDate));

  for (const entry of entries) {
    const stem = feedbackStem(entry);
    entry.files = [];

    if (entry.kind === 'crash') {
      try {
        const response = await asc(
          mint,
          'GET',
          `/v1/betaFeedbackCrashSubmissions/${encodeURIComponent(entry.id)}/crashLog`,
        );
        const logText = response.data?.attributes?.logText;
        if (logText) {
          const name = `${stem}.ips`;
          writeFileSync(join(outDir, name), logText, 'utf8');
          entry.files.push(name);
        } else {
          entry.crashLog = 'empty';
        }
      } catch (error) {
        entry.crashLog = isMissingCrashLog(error)
          ? 'not provided by Apple (hang/OOM feedback)'
          : `download failed: ${error.message}`;
      }
    }

    for (const [index, screenshot] of entry.screenshots.entries()) {
      if (!screenshot.url) continue;
      const extension = extname(new URL(screenshot.url).pathname) || '.jpg';
      const name = `${stem}-screenshot-${index + 1}${extension}`;
      try {
        await download(screenshot.url, join(outDir, name));
        entry.files.push(name);
      } catch (error) {
        entry.downloadErrors = [...(entry.downloadErrors ?? []), `screenshot ${index + 1}: ${error.message}`];
      }
    }
  }

  const persisted = entries.map(({ screenshots: _signedUrls, ...entry }) => entry);
  writeJson(join(outDir, 'feedback.json'), persisted);
  return {
    reports: entries.length,
    crashReports: entries.filter((entry) => entry.kind === 'crash').length,
    screenshotReports: entries.filter((entry) => entry.kind === 'screenshot').length,
    files: persisted.flatMap((entry) => entry.files),
  };
};

export const deviceFileListArgs = (device, subdirectory, jsonOutput) => [
  'devicectl',
  'device',
  'info',
  'files',
  '--device',
  device,
  '--domain-type',
  'systemCrashLogs',
  ...(subdirectory ? ['--subdirectory', subdirectory] : []),
  '--recurse',
  '--filter',
  DIAGNOSTIC_FILTER,
  '--json-output',
  jsonOutput,
];

export const deviceCopyArgs = (device, source, destination, jsonOutput) => [
  'devicectl',
  'device',
  'copy',
  'from',
  '--device',
  device,
  '--domain-type',
  'systemCrashLogs',
  '--source',
  source,
  '--destination',
  destination,
  '--json-output',
  jsonOutput,
];

export const deviceFileCandidates = (groups, cutoff) => {
  const found = new Map();
  for (const { subdirectory, files } of groups) {
    for (const file of files ?? []) {
      const modified = Date.parse(file.metadata?.lastModDate);
      if (
        file.resources?.isDirectory ||
        !DIAGNOSTIC_NAME.test(file.name ?? '') ||
        (Number.isFinite(modified) && modified < cutoff)
      ) {
        continue;
      }
      const relativePath = file.relativePath ?? file.name;
      const source = subdirectory ? `${subdirectory}/${relativePath}` : relativePath;
      if (source.split('/').includes('..')) continue;
      found.set(source, {
        source,
        name: file.name,
        lastModified: file.metadata?.lastModDate,
        size: file.metadata?.size,
      });
    }
  }
  return [...found.values()].sort(
    (a, b) => Date.parse(b.lastModified) - Date.parse(a.lastModified),
  );
};

const physicalIosDevices = (devices) =>
  devices.filter(
    (device) =>
      device.hardwareProperties?.platform === 'iOS' &&
      device.hardwareProperties?.reality === 'physical' &&
      device.connectionProperties?.pairingState === 'paired',
  );

export const selectPhysicalDevice = (devices, requested) => {
  const physical = physicalIosDevices(devices);
  if (requested) {
    const found = physical.find(
      (device) =>
        device.identifier === requested || device.hardwareProperties?.udid === requested,
    );
    if (!found) throw new Error(`paired physical iOS device ${requested} not found`);
    return found;
  }
  if (!physical.length) throw new Error('no paired physical iOS device is connected');
  if (physical.length > 1) {
    const choices = physical
      .map(
        (device) =>
          `${device.deviceProperties?.name ?? 'iPhone'} (${device.identifier ?? device.hardwareProperties?.udid})`,
      )
      .join(', ');
    throw new Error(`more than one physical iOS device is connected; pass --device: ${choices}`);
  }
  return physical[0];
};

export const isVisAppDiagnostic = (contents) =>
  contents.includes(DEFAULT_BUNDLE_ID) || /Vis Companion/i.test(contents);

export const crashFileArgs = (name, contents = '') => {
  if (/^JetsamEvent-/i.test(name)) return { keep: true, reason: 'system-memory' };
  if (/^stacks-/i.test(name)) return { keep: true, reason: 'system-stacks' };
  if (/^App-/i.test(name) && isVisAppDiagnostic(contents)) {
    return { keep: true, reason: 'vis-app-crash' };
  }
  return { keep: false, reason: /^App-/i.test(name) ? 'other-app' : 'not-diagnostic' };
};

const runXcrunJson = (args, jsonOutput) => {
  const result = spawnSync('xcrun', args, { encoding: 'utf8' });
  if (result.status !== 0) {
    const detail = result.stderr.trim() || result.stdout.trim() || `exit ${result.status}`;
    throw new Error(`xcrun ${args.slice(0, 4).join(' ')} failed: ${detail}`);
  }
  return JSON.parse(readFileSync(jsonOutput, 'utf8'));
};

const withTempJson = (prefix, buildArgs) => {
  const temp = mkdtempSync(join(tmpdir(), prefix));
  const jsonOutput = join(temp, 'result.json');
  try {
    return runXcrunJson(buildArgs(jsonOutput), jsonOutput);
  } finally {
    rmSync(temp, { recursive: true, force: true });
  }
};

const listDevices = () =>
  withTempJson('vis-ios-devices-', (jsonOutput) => [
    'devicectl',
    'list',
    'devices',
    '--json-output',
    jsonOutput,
  ]).result?.devices ?? [];

const listDeviceFiles = (device, subdirectory) =>
  withTempJson('vis-ios-files-', (jsonOutput) =>
    deviceFileListArgs(device, subdirectory, jsonOutput),
  ).result?.files ?? [];

const copyDeviceFile = (device, source, destination) => {
  mkdirSync(dirname(destination), { recursive: true });
  rmSync(destination, { force: true });
  return withTempJson('vis-ios-copy-', (jsonOutput) =>
    deviceCopyArgs(device, source, destination, jsonOutput),
  ).result;
};

const collectDeviceDiagnostics = ({ cutoff, requestedDevice, maxFiles, outDir }) => {
  mkdirSync(outDir, { recursive: true });
  const device = selectPhysicalDevice(listDevices(), requestedDevice);
  const id = device.identifier ?? device.hardwareProperties?.udid;
  const groups = [
    { subdirectory: undefined, files: listDeviceFiles(id, undefined) },
    { subdirectory: 'Retired', files: listDeviceFiles(id, 'Retired') },
  ];
  const allCandidates = deviceFileCandidates(groups, cutoff);
  const candidates = allCandidates.slice(0, maxFiles);
  const files = [];

  for (const candidate of candidates) {
    const destination = join(outDir, ...candidate.source.split('/'));
    try {
      copyDeviceFile(id, candidate.source, destination);
      const contents = readFileSync(destination, 'utf8');
      const decision = crashFileArgs(candidate.name, contents);
      const record = { ...candidate, ...decision };
      if (decision.keep) {
        record.file = relative(outDir, destination);
        files.push(record.file);
      } else {
        rmSync(destination, { force: true });
      }
      candidate.result = record;
    } catch (error) {
      candidate.result = { ...candidate, keep: false, reason: 'copy-failed', error: error.message };
    }
  }

  const records = candidates.map((candidate) => candidate.result);
  writeJson(join(outDir, 'index.json'), {
    device: {
      identifier: id,
      udid: device.hardwareProperties?.udid,
      name: device.deviceProperties?.name,
      model: device.hardwareProperties?.marketingName,
      osVersion: device.deviceProperties?.osVersionNumber,
    },
    candidateCount: allCandidates.length,
    truncated: allCandidates.length > candidates.length,
    files: records,
  });
  return {
    device: device.deviceProperties?.name ?? id,
    candidates: allCandidates.length,
    scanned: candidates.length,
    kept: files.length,
    copyFailures: records.filter((record) => record.reason === 'copy-failed').length,
    files,
  };
};

const usage = `
Collect iOS/TestFlight diagnostics into one local directory.

Usage: npm run diagnostics:ios -- [options]

  --days <n>              only reports modified/created in the last n days (default 14)
  --out <dir>             destination (default /tmp/vis-ios-crashes-<timestamp>)
  --limit <n>             maximum feedback records requested per ASC kind (default 50)
  --max-device-files <n>  maximum recent device candidates copied (default 100)
  --device <id-or-udid>   choose a paired physical iPhone when several exist
  --asc-only              skip the physical device
  --device-only           skip App Store Connect
  --bundle <id>           App Store Connect bundle id (default ${DEFAULT_BUNDLE_ID})
  --help                   show this text
`;

const valueFlag = (args, name) => {
  const index = args.indexOf(`--${name}`);
  return index >= 0 ? args[index + 1] : undefined;
};

const positiveInteger = (value, fallback, name) => {
  const parsed = value === undefined ? fallback : Number(value);
  if (!Number.isInteger(parsed) || parsed < 1) throw new Error(`--${name} must be a positive integer`);
  return parsed;
};

export const main = async (argv = process.argv.slice(2)) => {
  if (argv.includes('--help')) {
    console.log(usage);
    return;
  }
  const ascOnly = argv.includes('--asc-only');
  const deviceOnly = argv.includes('--device-only');
  if (ascOnly && deviceOnly) throw new Error('--asc-only and --device-only are mutually exclusive');

  const days = positiveInteger(valueFlag(argv, 'days'), DEFAULT_DAYS, 'days');
  const limit = positiveInteger(valueFlag(argv, 'limit'), DEFAULT_LIMIT, 'limit');
  const maxDeviceFiles = positiveInteger(
    valueFlag(argv, 'max-device-files'),
    DEFAULT_DEVICE_LIMIT,
    'max-device-files',
  );
  const generatedAt = new Date();
  const cutoff = generatedAt.getTime() - days * 24 * 60 * 60 * 1000;
  const outDir = resolve(
    valueFlag(argv, 'out') ?? join(tmpdir(), `vis-ios-crashes-${timestampPart(generatedAt)}`),
  );
  const bundleId = valueFlag(argv, 'bundle') ?? DEFAULT_BUNDLE_ID;
  mkdirSync(outDir, { recursive: true });

  const manifest = {
    generatedAt: generatedAt.toISOString(),
    cutoff: new Date(cutoff).toISOString(),
    bundleId,
    appStoreConnect: null,
    device: null,
    errors: [],
  };

  if (!deviceOnly) {
    try {
      manifest.appStoreConnect = await collectAscFeedback({
        bundleId,
        cutoff,
        limit,
        outDir: join(outDir, 'app-store-connect'),
      });
      console.log(
        `✓ App Store Connect: ${manifest.appStoreConnect.reports} recent feedback report(s)`,
      );
    } catch (error) {
      manifest.errors.push({ source: 'app-store-connect', message: error.message });
      console.error(`! App Store Connect: ${error.message}`);
    }
  }

  if (!ascOnly) {
    try {
      manifest.device = collectDeviceDiagnostics({
        cutoff,
        requestedDevice: valueFlag(argv, 'device'),
        maxFiles: maxDeviceFiles,
        outDir: join(outDir, 'device'),
      });
      console.log(
        `✓ ${manifest.device.device}: kept ${manifest.device.kept} of ${manifest.device.scanned} recent diagnostic candidate(s)`,
      );
    } catch (error) {
      manifest.errors.push({ source: 'device', message: error.message });
      console.error(`! Device: ${error.message}`);
    }
  }

  writeJson(join(outDir, 'manifest.json'), manifest);
  console.log(`\nDiagnostics: ${outDir}\n`);
  const successes = Number(Boolean(manifest.appStoreConnect)) + Number(Boolean(manifest.device));
  if (!successes) process.exitCode = 1;
};

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  main().catch((error) => {
    console.error(`\n✗ ${error.message}\n`);
    process.exitCode = 1;
  });
}

#!/usr/bin/env node
/**
 * Android crash/ANR diagnostics from the Google Play Developer Reporting API — the Android
 * half of what scripts/ios-crashes.mjs collects for TestFlight.
 *
 * Play Console shows Vitals to a human and to nothing else: androidpublisher (scripts/play.mjs)
 * publishes builds but cannot read a single crash. Reporting is the only API that answers
 * "what is failing in the shipped app", so without it Android is a blind spot next to iOS —
 * every reported Android bug would arrive as a sentence from a user.
 *
 * Two grants, in two different consoles, and the preflight names whichever is missing:
 *   1. the Reporting API enabled in the service account's Google Cloud project;
 *   2. the service account added in Play Console with "View app information and download
 *      bulk reports (read-only)" for this app.
 *
 * Usage:
 *   npm run diagnostics:android
 *   npm run diagnostics:android -- --days 28 --out /tmp/vis-android
 *   npm run diagnostics:android -- --check          # preflight only, writes nothing
 */
import { spawnSync } from 'node:child_process';
import { mkdirSync, writeFileSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { playToken } from './play.mjs';

const REPORTING = 'https://playdeveloperreporting.googleapis.com/v1beta1';
export const REPORTING_SCOPE = 'https://www.googleapis.com/auth/playdeveloperreporting';
export const REPORTING_API = 'playdeveloperreporting.googleapis.com';
const DEFAULT_PACKAGE = 'com.blockether.viscompanion';
const DEFAULT_DAYS = 14;
const DEFAULT_LIMIT = 20;
const DEFAULT_REPORTS_PER_ISSUE = 3;
const STACK_LINES = 40;

// `security -w` prints hex whenever the stored secret is not plain printable ASCII.
const unhex = (s) => (/^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s);

const keychain = (service, account) => {
  if (process.platform !== 'darwin') return undefined;
  const res = spawnSync('security', ['find-generic-password', '-s', service, '-a', account, '-w'], { encoding: 'utf8' });
  return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
};

/** The publishing service account, from the environment or the login keychain — never from disk. */
const credentials = () => process.env.VIS_PLAY_SERVICE_ACCOUNT?.trim() || keychain('vis-play', 'service_account');

/**
 * Google answers a project-level "this API is off" with a structured ErrorInfo whose metadata
 * carries the consumer and the service. Read that first; the prose message is the fallback,
 * because it names the API by its display name and only the project number is machine-readable.
 */
export const disabledApi = (error, api = REPORTING_API) => {
  const info = (error?.details ?? []).find((d) => d.reason === 'SERVICE_DISABLED');
  if (info) {
    return { api: info.metadata?.service ?? api, project: String(info.metadata?.consumer ?? '').replace(/^projects\//, '') };
  }
  const project = /has not been used in project (\d+)/.exec(error?.message ?? '')?.[1];
  return project ? { api, project } : undefined;
};

/** The one click that fixes a disabled API, in the project that actually owns the credential. */
export const enableUrl = ({ api, project }) => `https://console.developers.google.com/apis/api/${api}/overview?project=${project}`;

/**
 * A 403 that is NOT "API disabled" is the OTHER grant: the service account exists and holds a
 * Reporting token, but Play Console has not given it bulk-report access to this app.
 */
export const missingPlayGrant = (error) => error?.status === 403 && !disabledApi(error);

/** A google.type.DateTime (or an ISO string) as a plain YYYY-MM-DD day. */
export const dayOf = (value) => {
  if (!value) return undefined;
  if (typeof value === 'string') return value.slice(0, 10);
  const { year, month, day } = value;
  return year && month && day ? `${year}-${String(month).padStart(2, '0')}-${String(day).padStart(2, '0')}` : undefined;
};

/**
 * The search interval is passed as flattened google.type.DateTime query parameters, at whole
 * hours: the Reporting API rejects sub-hour granularity.
 */
export const dateTimeParams = (prefix, date) => ({
  [`${prefix}.year`]: String(date.getUTCFullYear()),
  [`${prefix}.month`]: String(date.getUTCMonth() + 1),
  [`${prefix}.day`]: String(date.getUTCDate()),
  [`${prefix}.hours`]: String(date.getUTCHours()),
  [`${prefix}.minutes`]: '0',
  [`${prefix}.seconds`]: '0',
  [`${prefix}.nanos`]: '0',
  [`${prefix}.timeZone.id`]: 'UTC',
});

/** Query parameters for errorIssues:search / errorReports:search over the last `days`. */
export const searchQuery = ({ days = DEFAULT_DAYS, limit = DEFAULT_LIMIT, now = new Date(), filter, orderBy } = {}) => {
  const end = new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate(), now.getUTCHours()));
  const start = new Date(end.getTime() - days * 24 * 60 * 60 * 1000);
  return {
    ...dateTimeParams('interval.startTime', start),
    ...dateTimeParams('interval.endTime', end),
    pageSize: String(limit),
    ...(filter ? { filter } : {}),
    ...(orderBy ? { orderBy } : {}),
  };
};

/** The metric set's own answer to "how recent can this query be", per aggregation period. */
export const freshnessDay = (metricSet, aggregationPeriod = 'DAILY') =>
  dayOf((metricSet?.freshnessInfo?.freshnesses ?? []).find((f) => f.aggregationPeriod === aggregationPeriod)?.latestEndTime);

/** One crash/ANR cluster per row, heaviest first — the same order the Play Console lists them in. */
export const issueEntries = (response) =>
  (response?.errorIssues ?? [])
    .map((issue) => ({
      id: String(issue.name ?? '').split('/').pop(),
      type: issue.type,
      cause: issue.cause,
      location: issue.location,
      reports: Number(issue.errorReportCount ?? 0),
      users: Number(issue.distinctUsers ?? 0),
      lastReportTime: issue.lastErrorReportTime,
      versionCodes: [issue.firstAppVersion?.versionCode, issue.lastAppVersion?.versionCode]
        .filter((v) => v !== undefined && v !== null)
        .map(String),
      apiLevels: [issue.firstOsVersion?.apiLevel, issue.lastOsVersion?.apiLevel]
        .filter((v) => v !== undefined && v !== null)
        .map(String),
      console: issue.issueUri,
    }))
    .sort((a, b) => b.reports - a.reports || b.users - a.users);

/** One device event per row. The stack trace is truncated: a report is context, not an archive. */
export const reportEntries = (response, stackLines = STACK_LINES) =>
  (response?.errorReports ?? []).map((report) => ({
    id: String(report.name ?? '').split('/').pop(),
    type: report.type,
    issueId: report.issueId,
    eventTime: report.eventTime,
    device: [report.deviceModel?.deviceId?.buildBrand, report.deviceModel?.marketingName].filter(Boolean).join(' ') || undefined,
    apiLevel: report.osVersion?.apiLevel === undefined ? undefined : String(report.osVersion.apiLevel),
    versionCode: report.appVersion?.versionCode === undefined ? undefined : String(report.appVersion.versionCode),
    stack: String(report.reportText ?? '')
      .split('\n')
      .slice(0, stackLines)
      .join('\n'),
  }));

/** A metric-set timeline flattened to one row per period: {day, dimensions, metrics}. */
export const metricRows = (response) =>
  (response?.rows ?? []).map((row) => ({
    day: dayOf(row.startTime),
    dimensions: Object.fromEntries(
      (row.dimensions ?? []).map((d) => [d.dimension, d.stringValue ?? d.int64Value ?? d.valueLabel ?? null]),
    ),
    metrics: Object.fromEntries(
      (row.metrics ?? []).map((m) => [m.metric, m.decimalValue?.value ?? m.int64Value ?? m.doubleValue ?? null]),
    ),
  }));

/** The body of a crashRateMetricSet/anrRateMetricSet :query over a whole-day timeline. */
export const timelineBody = ({ metrics, days = DEFAULT_DAYS, now = new Date(), dimensions = [] }) => {
  const day = (d) => ({ year: d.getUTCFullYear(), month: d.getUTCMonth() + 1, day: d.getUTCDate(), timeZone: { id: 'America/Los_Angeles' } });
  const end = new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate()));
  return {
    // DAILY is fixed to America/Los_Angeles by the API; HOURLY is UTC.
    timelineSpec: { aggregationPeriod: 'DAILY', startTime: day(new Date(end.getTime() - days * 24 * 60 * 60 * 1000)), endTime: day(end) },
    dimensions,
    metrics,
    pageSize: 200,
  };
};

const call = async (token, method, path, { query, body } = {}) => {
  const url = new URL(`${REPORTING}${path}`);
  for (const [k, v] of Object.entries(query ?? {})) url.searchParams.set(k, v);
  const res = await fetch(url, {
    method,
    headers: { Authorization: `Bearer ${token}`, ...(body === undefined ? {} : { 'Content-Type': 'application/json' }) },
    ...(body === undefined ? {} : { body: JSON.stringify(body) }),
  });
  const text = await res.text();
  const json = text ? JSON.parse(text) : {};
  if (!res.ok) {
    const err = new Error(`Reporting ${method} ${path} → ${res.status} ${json.error?.message ?? text}`);
    err.status = res.status;
    err.details = json.error?.details ?? [];
    throw err;
  }
  return json;
};

const flag = (argv, name, fallback) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] && !argv[i + 1].startsWith('--') ? argv[i + 1] : fallback;
};

const writeJson = (path, value) => writeFileSync(path, `${JSON.stringify(value, null, 2)}\n`);

/** Both grants, checked before anything is collected, each with the console that fixes it. */
const preflight = async (token, pkg) => {
  try {
    const set = await call(token, 'GET', `/apps/${pkg}/crashRateMetricSet`);
    return { ok: true, freshness: freshnessDay(set) };
  } catch (error) {
    const disabled = disabledApi(error);
    if (disabled) {
      return {
        ok: false,
        remedy: `enable the Play Developer Reporting API in project ${disabled.project}: ${enableUrl(disabled)}`,
        error,
      };
    }
    if (missingPlayGrant(error)) {
      return {
        ok: false,
        remedy:
          'grant the service account Play Console access: Users and permissions → invite the service-account address → ' +
          `App permissions for ${pkg} → "View app information and download bulk reports (read-only)"`,
        error,
      };
    }
    return { ok: false, remedy: 'unexpected Reporting API failure', error };
  }
};

export const main = async (argv = process.argv.slice(2)) => {
  const pkg = flag(argv, 'package', DEFAULT_PACKAGE);
  const days = Number(flag(argv, 'days', String(DEFAULT_DAYS)));
  const limit = Number(flag(argv, 'limit', String(DEFAULT_LIMIT)));
  const checkOnly = argv.includes('--check');
  const outDir = resolve(flag(argv, 'out', join('/tmp', `vis-android-crashes-${new Date().toISOString().replace(/[:.]/g, '-')}`)));

  const account = credentials();
  if (!account) {
    console.error('no Play service account: set VIS_PLAY_SERVICE_ACCOUNT or run `npm run secrets play <key.json>`');
    return 1;
  }
  const { token, account: email } = await playToken(account, { scope: REPORTING_SCOPE });
  console.log(`service account: ${email}`);

  const check = await preflight(token, pkg);
  if (!check.ok) {
    console.error(`✗ ${check.error.message}`);
    console.error(`→ ${check.remedy}`);
    return 1;
  }
  console.log(`✓ Reporting API reachable for ${pkg} (crash metrics fresh through ${check.freshness ?? 'unknown'})`);
  if (checkOnly) return 0;

  mkdirSync(outDir, { recursive: true });
  const manifest = { generatedAt: new Date().toISOString(), package: pkg, days, freshness: check.freshness, errors: [] };

  const issues = issueEntries(
    await call(token, 'GET', `/apps/${pkg}/errorIssues:search`, {
      query: searchQuery({ days, limit, orderBy: 'errorReportCount desc' }),
    }),
  );
  writeJson(join(outDir, 'issues.json'), issues);
  manifest.issues = issues.length;
  console.log(`✓ ${issues.length} error issue(s) in the last ${days} day(s)`);

  mkdirSync(join(outDir, 'reports'), { recursive: true });
  let reports = 0;
  for (const issue of issues.slice(0, limit)) {
    try {
      const entries = reportEntries(
        await call(token, 'GET', `/apps/${pkg}/errorReports:search`, {
          query: searchQuery({ days, limit: DEFAULT_REPORTS_PER_ISSUE, filter: `errorIssueId = ${issue.id}` }),
        }),
      );
      if (entries.length) writeJson(join(outDir, 'reports', `${issue.id}.json`), entries);
      reports += entries.length;
    } catch (error) {
      manifest.errors.push({ source: `errorReports/${issue.id}`, message: error.message });
    }
  }
  manifest.reports = reports;
  console.log(`✓ ${reports} sample report(s) with stack traces`);

  for (const [name, metrics] of [
    ['crash-rate', ['crashRate', 'userPerceivedCrashRate', 'distinctUsers']],
    ['anr-rate', ['anrRate', 'userPerceivedAnrRate', 'distinctUsers']],
  ]) {
    const set = name === 'crash-rate' ? 'crashRateMetricSet' : 'anrRateMetricSet';
    try {
      const rows = metricRows(await call(token, 'POST', `/apps/${pkg}/${set}:query`, { body: timelineBody({ metrics, days }) }));
      mkdirSync(join(outDir, 'metrics'), { recursive: true });
      writeJson(join(outDir, 'metrics', `${name}.json`), rows);
      console.log(`✓ ${rows.length} day(s) of ${name}`);
    } catch (error) {
      manifest.errors.push({ source: set, message: error.message });
      console.error(`✗ ${set}: ${error.message}`);
    }
  }

  writeJson(join(outDir, 'manifest.json'), manifest);
  console.log(`\n${outDir}`);
  return manifest.errors.length ? 1 : 0;
};

if (process.argv[1] && process.argv[1].endsWith('android-crashes.mjs')) {
  main().then((code) => process.exit(code), (error) => {
    console.error(error.message);
    process.exit(1);
  });
}

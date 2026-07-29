#!/usr/bin/env node
/**
 * Expire TestFlight builds.
 *
 * A build can never be DELETED from App Store Connect — Apple keeps every upload forever.
 * The only lever is `expired: true`, which pulls it from the TestFlight app for every
 * tester and from the build list's "active" section. It is one-way: an expired build
 * cannot be un-expired, only superseded by a newer upload.
 *
 * Why this exists: builds are grouped in the TestFlight app by their VERSION STRING
 * (CFBundleShortVersionString, i.e. the repo-root VERSION). When several builds carry the
 * same version, testers see the newest *installable* one — and a build that was uploaded
 * but never linked to the external group (see testflight.mjs) is not installable, so the
 * app keeps offering the older build. Expiring the stale ones makes the list honest.
 *
 * Usage (dry run by default — nothing changes without --yes):
 *   node scripts/expire.mjs                       # plan: expire everything but the newest
 *   node scripts/expire.mjs --yes
 *   node scripts/expire.mjs --keep 3 --yes        # keep the 3 newest
 *   node scripts/expire.mjs --build 2740 --yes    # exactly these (repeatable, comma ok)
 *   node scripts/expire.mjs --version 0.1.12 --yes
 *   node scripts/expire.mjs --list                # just show what is there
 *
 * Credentials: env first (VIS_ASC_KEY_ID / VIS_ASC_ISSUER_ID / VIS_ASC_KEY_PATH), then the
 * macOS login keychain (`npm run secrets asc …`). Never a file in this repo.
 */
import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { appIdFor, asc, ascToken } from './asc.mjs';

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), '..');

/** Every build of the app, newest upload first, with its version-string group. */
export const listBuilds = async (token, appId, { limit = 200 } = {}) => {
  const res = await asc(
    token,
    'GET',
    `/v1/builds?filter[app]=${appId}&limit=${limit}&sort=-uploadedDate&include=preReleaseVersion`,
  );
  const versions = new Map((res.included ?? []).map((i) => [i.id, i.attributes?.version]));
  return (res.data ?? []).map((b) => ({
    id: b.id,
    build: b.attributes?.version,
    version: versions.get(b.relationships?.preReleaseVersion?.data?.id) ?? '?',
    state: b.attributes?.processingState,
    expired: b.attributes?.expired === true,
    uploaded: b.attributes?.uploadedDate,
  }));
};

export const expireBuild = (token, id) =>
  asc(token, 'PATCH', `/v1/builds/${id}`, { data: { type: 'builds', id, attributes: { expired: true } } });

// ── standalone CLI ────────────────────────────────────────────────────────────────────

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  const args = process.argv.slice(2);
  const flags = (name) =>
    args.flatMap((a, i) => (a === `--${name}` && args[i + 1] ? args[i + 1].split(',') : []));
  const flag = (name) => flags(name)[0];
  const has = (name) => args.includes(`--${name}`);

  const unhex = (s) => (/^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s);
  const keychain = (account) => {
    if (process.platform !== 'darwin') return undefined;
    const res = spawnSync('security', ['find-generic-password', '-s', 'vis-ios', '-a', account, '-w'], { encoding: 'utf8' });
    return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
  };
  const secret = (envName, account) => process.env[envName]?.trim() || keychain(account);

  const keyId = secret('VIS_ASC_KEY_ID', 'asc_key_id');
  const issuerId = secret('VIS_ASC_ISSUER_ID', 'asc_issuer_id');
  const keyPem = process.env.VIS_ASC_KEY_PATH ? readFileSync(process.env.VIS_ASC_KEY_PATH, 'utf8') : keychain('asc_key');
  if (!keyId || !issuerId || !keyPem) {
    console.error('\n✗ no App Store Connect API key (npm run secrets asc <AuthKey_XXXX.p8> --issuer <uuid> --team <id>)\n');
    process.exit(1);
  }

  const bundleId = flag('bundle') ?? JSON.parse(readFileSync(join(appDir, 'capacitor.config.json'), 'utf8')).appId;
  const token = ascToken({ keyId, issuerId, keyPem });
  const appId = await appIdFor(token, bundleId);
  if (!appId) {
    console.error(`\n✗ no app with bundle id ${bundleId} in this API key's team\n`);
    process.exit(1);
  }

  const builds = await listBuilds(token, appId);
  const live = builds.filter((b) => !b.expired);
  const show = (b) => `  ${b.expired ? '·' : '●'} ${b.version} (${b.build})  ${b.state}  ${b.uploaded ?? ''}`;
  console.log(`\n${bundleId} — ${builds.length} builds (● active, · expired):`);
  builds.forEach((b) => console.log(show(b)));

  if (has('list')) process.exit(0);

  const wantBuilds = new Set(flags('build'));
  const wantVersions = new Set(flags('version'));
  const keep = Number(flag('keep') ?? 1);
  const targets =
    wantBuilds.size || wantVersions.size
      ? live.filter((b) => wantBuilds.has(String(b.build)) || wantVersions.has(b.version))
      : live.slice(keep);

  if (!targets.length) {
    console.log('\nNothing to expire.\n');
    process.exit(0);
  }
  console.log(`\nWould expire ${targets.length} build(s):`);
  targets.forEach((b) => console.log(show(b)));

  if (!has('yes')) {
    console.log('\nDry run. Re-run with --yes to expire them (one-way — a build cannot be un-expired).\n');
    process.exit(0);
  }

  let failed = 0;
  for (const b of targets) {
    try {
      await expireBuild(token, b.id);
      console.log(`· expired ${b.version} (${b.build})`);
    } catch (err) {
      failed += 1;
      console.error(`✗ ${b.version} (${b.build}): ${err.message}`);
    }
  }
  console.log(failed ? `\n✗ ${failed} of ${targets.length} failed.\n` : `\n✓ expired ${targets.length} build(s).\n`);
  process.exit(failed ? 1 : 0);
}

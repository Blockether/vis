#!/usr/bin/env node
// Release notes for TestFlight — generated from git, kept in CHANGELOG.md, pushed to
// App Store Connect as the build's "What to Test" text, which is what a tester actually
// reads in the TestFlight app before installing.
//
// Two entry points:
//   • imported by scripts/ios-release.mjs, which calls it right after `altool --upload-app`
//   • standalone: `npm run release:notes -- --build 2705` re-pushes notes for a build that
//     is already in App Store Connect (uploads are async — notes can land minutes later)
//
// The CHANGELOG entry is the source of truth: once written it is never regenerated, so
// hand-edited wording survives a re-run. Deleting the entry makes the next run rebuild it.

import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { appIdFor, asc, ascToken, waitForBuild } from './asc.mjs';
import { syncPackageVersion } from './version.mjs';

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const repoRoot = resolve(appDir, '..', '..');
const changelogPath = join(appDir, 'CHANGELOG.md');

// App Store Connect caps "What to Test" at 4000 characters and silently rejects more.
const WHATS_NEW_LIMIT = 4000;
const MAX_BULLETS = 20;
// No CHANGELOG yet (first ever run): summarise this many commits rather than the whole history.
const FALLBACK_COMMITS = 15;

// Subjects that describe the plumbing, not the product. A tester gains nothing from them.
const NOISE =
  /^(merge\b|revert:?\s*$|wip\b|fixup!|squash!|amend\b|bump\b|chore\(deps\)|format\b|reformat\b|lint\b|typo\b|whitespace\b|release:\s*(update|bump|prepare)|version bump|v?\d+\.\d+\.\d+$|\.{3}$)/i;

const capture = (cmd, args, opts = {}) => {
  const res = spawnSync(cmd, args, { encoding: 'utf8', cwd: repoRoot, ...opts });
  return res.status === 0 ? res.stdout.trim() : '';
};

// ── notes from git ────────────────────────────────────────────────────────────────────

const tidy = (subject) => {
  // Conventional-commit prefixes are for the log, not for a tester.
  const stripped = subject.replace(/^(feat|fix|perf|refactor|docs|build|ci|test|style|chore)(\([^)]*\))?!?:\s*/i, '');
  return stripped.charAt(0).toUpperCase() + stripped.slice(1);
};

/** Commit subjects since `sinceSha` (exclusive), newest first, de-noised and de-duplicated. */
export const collectCommits = (sinceSha, scope = []) => {
  const args = ['log', '--no-merges', '--pretty=format:%H\u001f%s'];
  // A CHANGELOG sha can outlive its commit (rebase, shallow clone) — verify, never assume.
  const known = sinceSha && sinceSha.length >= 7 && spawnSync('git', ['cat-file', '-e', `${sinceSha}^{commit}`], { cwd: repoRoot }).status === 0;
  if (known) {
    args.push(`${sinceSha}..HEAD`);
  } else {
    args.push('-n', String(FALLBACK_COMMITS));
  }
  if (scope.length) args.push('--', ...scope);

  const out = capture('git', args);
  const seen = new Set();
  const bullets = [];
  for (const line of out.split('\n')) {
    if (!line.trim()) continue;
    const subject = line.split('\u001f')[1] ?? '';
    if (!subject || NOISE.test(subject)) continue;
    const text = tidy(subject);
    const key = text.toLowerCase();
    if (seen.has(key)) continue;
    seen.add(key);
    bullets.push(text);
    if (bullets.length >= MAX_BULLETS) break;
  }
  return bullets;
};

// ── CHANGELOG.md ──────────────────────────────────────────────────────────────────────

const ENTRY_RE = /^## (?<version>[\d.]+) \((?<build>\d+)\)/;

/** Every `## <version> (<build>)` section, newest first, with its bullets and release sha. */
export const readChangelog = () => {
  if (!existsSync(changelogPath)) return [];
  const entries = [];
  let current;
  for (const line of readFileSync(changelogPath, 'utf8').split('\n')) {
    const m = ENTRY_RE.exec(line);
    if (m) {
      current = { version: m.groups.version, build: m.groups.build, sha: undefined, bullets: [] };
      entries.push(current);
      continue;
    }
    if (!current) continue;
    const sha = /^<!-- commit: ([0-9a-f]{7,40}) -->$/.exec(line.trim());
    if (sha) current.sha = sha[1];
    else if (line.startsWith('- ')) current.bullets.push(line.slice(2).trim());
  }
  return entries;
};

const renderEntry = ({ version, build, bullets, sha, date }) =>
  `## ${version} (${build}) — ${date}\n<!-- commit: ${sha} -->\n\n${bullets.map((b) => `- ${b}`).join('\n')}\n`;

/**
 * Notes for this build. Reuses an existing CHANGELOG entry verbatim (hand edits win);
 * otherwise generates from the commits since the previous entry and prepends a new one.
 */
export const buildNotes = ({ version, build, scope = [], write = true } = {}) => {
  const entries = readChangelog();
  const existing = entries.find((e) => e.version === version && e.build === build);
  if (existing?.bullets.length) return { bullets: existing.bullets, text: toWhatsNew(existing.bullets), reused: true };

  const bullets = collectCommits(entries[0]?.sha, scope);
  if (!bullets.length) return { bullets: [], text: '', reused: false };

  if (write) {
    const entry = renderEntry({
      version,
      build,
      bullets,
      sha: capture('git', ['rev-parse', 'HEAD']) || 'unknown',
      date: new Date().toISOString().slice(0, 10),
    });
    const head = '# Vis Companion — release notes\n\nWhat each TestFlight build changed. Edit before uploading; the release script never rewrites an existing entry.\n';
    const prev = existsSync(changelogPath) ? readFileSync(changelogPath, 'utf8') : '';
    // Keep only the existing entries: everything from the first `## ` heading on.
    // Slicing at the first blank line would leave the preamble behind and duplicate it.
    const firstEntry = prev.search(/^## /m);
    const body = firstEntry === -1 ? '' : prev.slice(firstEntry);
    writeFileSync(changelogPath, `${head}\n${entry}\n${body.trimStart()}`);
  }
  return { bullets, text: toWhatsNew(bullets), reused: false };
};

const toWhatsNew = (bullets) => {
  let text = bullets.map((b) => `• ${b}`).join('\n');
  if (text.length > WHATS_NEW_LIMIT) text = `${text.slice(0, WHATS_NEW_LIMIT - 1).replace(/\n[^\n]*$/, '')}\n…`;
  return text;
};

// ── App Store Connect ─────────────────────────────────────────────────────────────────
// The signing/HTTP/polling primitives live in ./asc.mjs and are shared with testflight.mjs.

/**
 * Attach `notes` to the TestFlight build as its What to Test text.
 * A freshly uploaded build only appears once Apple has ingested it, so poll rather than
 * fail — `timeoutMs: 0` gives up immediately (useful when re-running for an old build).
 */
export const publishNotes = async ({ keyId, issuerId, keyPem, bundleId, version, build, notes, locale = 'en-US', timeoutMs = 15 * 60 * 1000, log = console.log }) => {
  if (!notes?.trim()) return { ok: false, reason: 'no notes' };
  if (!keyId || !issuerId || !keyPem) return { ok: false, reason: 'no App Store Connect API key' };

  const token = ascToken({ keyId, issuerId, keyPem });

  // Every failure here is reportable, never fatal: the build is already uploaded and the
  // notes are already in CHANGELOG.md, so a bad token must not take the release down.
  try {

    const appId = await appIdFor(token, bundleId);
    if (!appId) return { ok: false, reason: `no app with bundle id ${bundleId}` };

    const found = await waitForBuild(token, { appId, build, timeoutMs, log: (m) => log(`· ${m}`) });
    const buildId = found?.id;
    if (!buildId) return { ok: false, reason: `build ${build} not visible in App Store Connect yet` };

    const existing = await asc(token, 'GET', `/v1/builds/${buildId}/betaBuildLocalizations?limit=50`);
    const mine = existing.data?.find((l) => l.attributes?.locale === locale);
    if (mine) {
      await asc(token, 'PATCH', `/v1/betaBuildLocalizations/${mine.id}`, {
        data: { type: 'betaBuildLocalizations', id: mine.id, attributes: { whatsNew: notes } },
      });
    } else {
      await asc(token, 'POST', '/v1/betaBuildLocalizations', {
        data: {
          type: 'betaBuildLocalizations',
          attributes: { locale, whatsNew: notes },
          relationships: { build: { data: { type: 'builds', id: buildId } } },
        },
      });
    }
    return { ok: true, buildId, version, build };
  } catch (err) {
    return { ok: false, reason: err.message };
  }
};

// ── standalone CLI ────────────────────────────────────────────────────────────────────

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  const args = process.argv.slice(2);
  const flag = (name) => {
    const i = args.indexOf(`--${name}`);
    return i === -1 ? undefined : args[i + 1];
  };
  const has = (name) => args.includes(`--${name}`);

  // Same keychain-first credential rule as scripts/ios-release.mjs: env wins, then the
  // macOS login keychain, never a dotfile in the repo.
  const unhex = (s) => (/^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s);
  const keychain = (account) => {
    if (process.platform !== 'darwin') return undefined;
    const res = spawnSync('security', ['find-generic-password', '-s', 'vis-ios', '-a', account, '-w'], { encoding: 'utf8' });
    return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
  };
  const secret = (envName, account) => process.env[envName]?.trim() || keychain(account);

  // The repo-root VERSION file is the one source of truth; package.json mirrors it.
  const version = flag('version') ?? syncPackageVersion();
  const build = flag('build') ?? capture('git', ['rev-list', '--count', 'HEAD']);
  const scope = flag('scope') ? [flag('scope')] : [];

  const { bullets, text, reused } = buildNotes({ version, build, scope, write: !has('no-changelog') });
  if (!bullets.length) {
    console.error('\n✗ no release-worthy commits found — write CHANGELOG.md by hand or pass --scope\n');
    process.exit(1);
  }
  console.log(`\nRelease notes for ${version} (${build})${reused ? ' — from CHANGELOG.md' : ''}:\n\n${text}\n`);

  if (has('print')) process.exit(0);

  const result = await publishNotes({
    keyId: secret('VIS_ASC_KEY_ID', 'asc_key_id'),
    issuerId: secret('VIS_ASC_ISSUER_ID', 'asc_issuer_id'),
    keyPem: process.env.VIS_ASC_KEY_PATH ? readFileSync(process.env.VIS_ASC_KEY_PATH, 'utf8') : keychain('asc_key'),
    bundleId: flag('bundle-id') ?? 'com.blockether.viscompanion',
    version,
    build,
    notes: text,
    timeoutMs: Number(flag('timeout') ?? 15 * 60 * 1000),
  });
  if (result.ok) console.log(`✓ TestFlight "What to Test" set for build ${build}\n`);
  else {
    console.error(`\n✗ notes not published: ${result.reason}`);
    console.error('  They are in CHANGELOG.md — paste them into App Store Connect ▸ TestFlight ▸ the build ▸ What to Test.\n');
    process.exit(1);
  }
}

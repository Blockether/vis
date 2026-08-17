#!/usr/bin/env node
/**
 * One command: web bundle -> Capacitor sync -> signed .aab -> Google Play track.
 * The Android mirror of scripts/ios-release.mjs, down to the versioning rule:
 *
 *   versionName = repo-root VIS_VERSION              (the release users see)
 *   versionCode = `git rev-list --count HEAD`         (strictly monotonic, shared with iOS)
 *
 * Both are passed to Gradle as project properties AND stamped into build.gradle by
 * scripts/android-prepare.mjs, so `android/` stays a disposable Capacitor output (it is
 * gitignored) and two uploads can never collide on a version code.
 *
 * Tracks — a build testers can install belongs on EVERY tester channel at once, so `--track`
 * takes a list (comma-separated, or repeat the flag) and defaults to `all`, written in ONE
 * transactional Play edit. No track is ever left a version behind:
 *   --track internal    up to 100 named testers, no review wait
 *   --track alpha       closed testing, tester lists/groups
 *   --track beta        OPEN testing — the public one, the TestFlight-public analogue
 *   --track production  the store itself — never implied, always asked for
 *   --track all         every TESTER track this listing has, read from Play — including a
 *                       closed track someone created in the Console, which no list here knows
 *   (default)           all
 *
 * Publishing may be FROZEN: scripts/android-publish-freeze.mjs is the single switch that
 * refuses every Play write while a submitted build is with review. `--no-upload` and
 * `--tracks` keep working; anything that would reach the store refuses before the build.
 *
 * Usage (workflow/store recovery only; normal releases use `npm run release:android`):
 *   npm run release:android:store                          # build + sign + EVERY tester track
 *   npm run release:android:store -- --track internal      # named internal testers only
 *   npm run release:android:store -- --track beta,production  # any subset, one edit
 *   npm run release:android:store -- --no-upload           # stop at the signed .aab
 *   npm run release:android:store -- --draft               # upload, do not release
 *   npm run release:android:store -- --rollout 0.1         # staged 10% (production/beta)
 *   npm run release:android:store -- --skip-web            # reuse dist/ and the last cap sync
 *   npm run release:android:store -- --reuse-existing --build 2861 # promote without re-upload
 *   npm run release:android:store -- --tracks              # just print what each track serves
 *
 * Credentials: env first, then the macOS login keychain (scripts/secrets.mjs) —
 *   VIS_PLAY_SERVICE_ACCOUNT   the service-account JSON itself (CI injects this)
 *   keychain vis-play/service_account
 * Nothing is ever written to a file in this repo.
 */
import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync, readdirSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { androidPublishRefusal } from './android-publish-freeze.mjs';
import { JDK, jdkHelp, pickJdk } from './jdk.mjs';
import { planRelease, promoteBundle, publishBundle, tracks as readTracks } from './play.mjs';
import { buildNotes } from './release-notes.mjs';
import { syncPackageVersion } from './version.mjs';

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const androidDir = join(appDir, 'android');

const args = process.argv.slice(2);
const flag = (name) => {
  const i = args.indexOf(`--${name}`);
  return i === -1 ? undefined : args[i + 1];
};
// Repeatable flags: `--track internal --track beta` reads as both, not as the last one.
const flags = (name) => args.flatMap((a, i) => (a === `--${name}` && args[i + 1] ? [args[i + 1]] : []));
const has = (name) => args.includes(`--${name}`);

const die = (msg) => {
  console.error(`\n✗ ${msg}\n`);
  process.exit(1);
};

// Publishing is frozen while a submitted build is with review — one switch, read by this
// script and by CI alike. A run that never reaches the store stays available, so Android
// keeps being built and signed while it may not be shipped.
if (!has('tracks') && !has('no-upload')) {
  const refusal = androidPublishRefusal('publishing the Vis Companion Android app');
  if (refusal) die(`${refusal}\n  Build without publishing: npm run release:android:store -- --no-upload`);
}
const run = (cmd, cmdArgs, opts = {}) => {
  console.log(`\n$ ${cmd} ${cmdArgs.join(' ')}`);
  const res = spawnSync(cmd, cmdArgs, { stdio: 'inherit', cwd: appDir, ...opts });
  if (res.status !== 0) die(`${cmd} failed (${res.status ?? res.signal})`);
};

const capture = (cmd, cmdArgs, opts = {}) => {
  const res = spawnSync(cmd, cmdArgs, { encoding: 'utf8', cwd: appDir, ...opts });
  return res.status === 0 ? res.stdout.trim() : '';
};

// `security -w` prints hex whenever the stored secret is not plain printable ASCII.
const unhex = (s) => (/^[0-9a-f]{32,}$/i.test(s) && s.length % 2 === 0 ? Buffer.from(s, 'hex').toString('utf8') : s);
const keychain = (service, account) => {
  if (process.platform !== 'darwin') return undefined;
  const res = spawnSync('security', ['find-generic-password', '-s', service, '-a', account, '-w'], { encoding: 'utf8' });
  return res.status === 0 && res.stdout.trim() ? unhex(res.stdout.trim()) : undefined;
};

const serviceAccount = process.env.VIS_PLAY_SERVICE_ACCOUNT?.trim() || keychain('vis-play', 'service_account');
const packageName = JSON.parse(readFileSync(join(appDir, 'capacitor.config.json'), 'utf8')).appId;

// ── read-only probe ───────────────────────────────────────────────────────────────────

if (has('tracks')) {
  if (!serviceAccount) die('no Play service account — `npm run secrets play <service-account.json>`');
  const found = await readTracks({ serviceAccount, packageName });
  console.log(`\n${packageName}`);
  for (const t of found) {
    const rel = t.releases.map((r) => `${r.name} [${r.status}${r.userFraction ? ` ${Math.round(r.userFraction * 100)}%` : ''}] codes ${r.versionCodes?.join(',')}`);
    console.log(`  ${t.track.padEnd(12)} ${rel.length ? rel.join(' · ') : '—'}`);
  }
  console.log();
  process.exit(0);
}

// ── versions ──────────────────────────────────────────────────────────────────────────

// Repo-root VIS_VERSION is the source of truth; npm metadata mirrors it.
const versionName = syncPackageVersion();
const versionCode = flag('build') ?? capture('git', ['rev-list', '--count', 'HEAD']);
if (!/^\d+$/.test(versionCode)) die(`version code must be a positive integer, got "${versionCode}"`);

const reuseExisting = has('reuse-existing');
if (reuseExisting && !flag('build')) die('--reuse-existing requires an explicit --build <versionCode>');

// Ask Play which tracks this listing HAS before planning anything: `all` (the default) then
// means every tester track that exists — a closed track added in the Play Console is served by
// this release without a code change — and a misspelled `--track` is refused against the real
// names. Skipped when there is nothing to publish to, so `--no-upload` still works offline.
let available;
if (serviceAccount && !has('no-upload')) {
  try {
    available = (await readTracks({ serviceAccount, packageName })).map((t) => t.track);
  } catch (err) {
    die(`could not read the Play tracks: ${err.message}`);
  }
}

// Plan the tracks BEFORE the build: an unknown track or a staged rollout aimed at several
// tracks must fail in a second, not after Gradle has spent ten minutes signing an .aab.
let tracks;
try {
  ({ tracks } = planRelease({ tracks: flags('track'), available, userFraction: flag('rollout'), draft: has('draft') }));
} catch (err) {
  die(err.message);
}

console.log(`\nVis Companion ${versionName} (${versionCode}) → Play ${tracks.join(', ')}\n`);

// What the human does next depends on which tracks were written, and the promote path and the
// upload path owe the same lines.
const report = (res) =>
  console.log(
    `\n✓ ${versionName} (${res.versionCode}) is on ${res.tracks.join(', ')} [${res.status}].\n` +
      [
        res.tracks.includes('internal') && '  Internal testing is live within minutes for the testers on that list.',
        res.tracks.includes('beta') &&
          '  Open testing: share the link from Play Console ▸ Testing ▸ Open testing ▸ Testers ▸ Copy link.\n' +
            '  First rollout on a new open track waits for Google review (hours to a day); later ones do not.',
        res.tracks.includes('production') && '  Production is live for everyone once Google finishes reviewing the rollout.',
      ]
        .filter(Boolean)
        .join('\n') +
      '\n',
  );

// Release notes come first so an empty/broken changelog fails before a long build or Play edit.
// Play caps release notes at 500 characters per language (App Store Connect allows 4000).
const notes = has('no-notes') ? { text: '' } : buildNotes({ version: versionName, build: versionCode, write: !has('no-changelog') });
if (notes.text) console.log(`\n· release notes:\n${notes.text}\n`);

if (reuseExisting) {
  if (!serviceAccount) die('no Play service account — `npm run secrets play <service-account.json>`');
  const res = await promoteBundle({
    serviceAccount,
    packageName,
    versionCode,
    tracks,
    releaseName: `${versionName} (${versionCode})`,
    notes: notes.text,
    userFraction: flag('rollout'),
    draft: has('draft'),
    log: (m) => console.log(m),
  });
  report(res);
  process.exit(0);
}

// ── build ─────────────────────────────────────────────────────────────────────────────

if (!has('skip-web')) {
  run('npm', ['run', 'build']);
  if (!existsSync(androidDir)) {
    console.log('· no android/ — scaffolding it with `cap add android`');
    run('npx', ['cap', 'add', 'android']);
  }
  run('npx', ['cap', 'sync', 'android']);
} else if (!existsSync(androidDir)) {
  die('no Android project to reuse with --skip-web; run a normal release once to scaffold it');
} else {
  console.log('· --skip-web: reusing dist/ and the last cap sync');
}

// Materialises google-services.json AND the release keystore from the keychain, and
// stamps the signing config + versions into the generated Gradle project.
run('node', [join(appDir, 'scripts', 'android-prepare.mjs'), '--build', versionCode]);

// The JDK Gradle needs is stock 21 and nothing else — scripts/jdk.mjs owns that rule
// and the search, so the preflight can ask the SAME question the build asks.
const javaHome = pickJdk();
if (!javaHome) die(jdkHelp());
if (javaHome !== process.env.JAVA_HOME) console.log(`\n· JDK ${JDK} for Gradle: ${javaHome}`);

const gradlew = join(androidDir, process.platform === 'win32' ? 'gradlew.bat' : 'gradlew');
run(gradlew, ['--no-daemon', 'bundleRelease'], { cwd: androidDir, env: { ...process.env, JAVA_HOME: javaHome } });

const outDir = join(androidDir, 'app', 'build', 'outputs', 'bundle', 'release');
const aabName = existsSync(outDir) ? readdirSync(outDir).find((f) => f.endsWith('.aab')) : undefined;
if (!aabName) die(`Gradle produced no .aab in ${outDir}`);
const aabPath = join(outDir, aabName);
const aab = readFileSync(aabPath);
console.log(`\n· aab ${aabPath} (${(aab.length / 1e6).toFixed(1)} MB)`);

// An unsigned bundle uploads fine and is then rejected hours later by Play review, so catch
// it here. jarsigner is authoritative (a .aab is JAR-signed) and ships with the same JDK we
// just picked — scanning the bytes for a META-INF/*.RSA block gives false negatives, because
// the signature block sits near the end of a 23 MB zip.
const verify = spawnSync(join(javaHome, 'bin', 'jarsigner'), ['-verify', aabPath], { encoding: 'utf8' });
if (verify.status !== 0 || !/jar verified/i.test(verify.stdout ?? ''))
  die(
    `the .aab is NOT signed — Play would reject it.\n  ${(verify.stdout ?? verify.stderr ?? '').trim().split('\n')[0]}\n` +
      '  Check android/keystore.properties, or re-run `npm run secrets keystore create`.',
  );
console.log('\u00b7 signature verified (jarsigner)');

if (has('no-upload')) {
  console.log(`\n✓ stopped before upload (--no-upload). The signed .aab is ${aabPath}\n`);
  process.exit(0);
}

// ── upload ────────────────────────────────────────────────────────────────────────────

if (!serviceAccount) {
  die(
    'no Play service account.\n' +
      '  Play Console ▸ Users and permissions ▸ invite the service account, then\n' +
      '    npm run secrets play <service-account.json>\n' +
      `  The signed bundle is ready at ${aabPath} — upload it by hand if you prefer.`,
  );
}

const res = await publishBundle({
  serviceAccount,
  packageName,
  aab,
  tracks,
  releaseName: `${versionName} (${versionCode})`,
  notes: notes.text,
  userFraction: flag('rollout'),
  draft: has('draft'),
  log: (m) => console.log(m),
});

report(res);

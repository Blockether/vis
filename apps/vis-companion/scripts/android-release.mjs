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
 * Tracks — "public testing" on Play is the `beta` track (Open testing), the closest thing
 * to a TestFlight public link. Anyone with the link joins, no invite, no per-tester list:
 *   --track internal    up to 100 named testers, no review wait
 *   --track alpha       closed testing, tester lists/groups
 *   --track beta        OPEN testing — the public one                     (default)
 *   --track production  the store itself
 *
 * Usage (workflow/store recovery only; normal releases use `npm run release:android`):
 *   npm run release:android:store                          # build + sign + beta/open testing
 *   npm run release:android:store -- --track internal      # named internal testers only
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
import { promoteBundle, publishBundle, tracks as readTracks } from './play.mjs';
import { buildNotes } from './release-notes.mjs';
import { syncPackageVersion } from './version.mjs';

const appDir = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const androidDir = join(appDir, 'android');

const args = process.argv.slice(2);
const flag = (name) => {
  const i = args.indexOf(`--${name}`);
  return i === -1 ? undefined : args[i + 1];
};
const has = (name) => args.includes(`--${name}`);

const die = (msg) => {
  console.error(`\n✗ ${msg}\n`);
  process.exit(1);
};

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

const track = flag('track') ?? 'beta';
if (!['internal', 'alpha', 'beta', 'production'].includes(track)) die(`unknown track "${track}" (internal | alpha | beta | production)`);

console.log(`\nVis Companion ${versionName} (${versionCode}) → Play ${track}\n`);

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
    track,
    releaseName: `${versionName} (${versionCode})`,
    notes: notes.text,
    userFraction: flag('rollout'),
    draft: has('draft'),
    log: (m) => console.log(m),
  });
  console.log(`\n✓ existing ${versionName} (${res.versionCode}) is on the ${track} track [${res.status}].\n`);
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


// Picking the JDK is not optional bookkeeping — three separate failures live here, and all
// three only bite the RELEASE build, long after a debug run looked fine:
//   • JDK 25 (this machine's SDKMAN default): Gradle 8.14 dies parsing the build script,
//     "Unsupported class file major version 69".
//   • JDK 17: Capacitor 8 compiles with `source 21` — "invalid source release: 21".
//   • GraalVM (any version): AGP's JdkImageTransform shells out to `jlink --disable-plugin
//     system-modules`, which Graal's jlink rejects.
// So: exactly 21, and a stock JDK. CI's temurin 21 satisfies JAVA_HOME and skips the search.
const JDK = 21;

const javaProps = (home) => {
  const bin = join(home, 'bin', 'java');
  if (!existsSync(bin)) return { major: 0, graal: false };
  const res = spawnSync(bin, ['-version'], { encoding: 'utf8' });
  const out = `${res.stderr ?? ''}${res.stdout ?? ''}`;
  return { major: Number(/version "(\d+)/.exec(out)?.[1] ?? 0), graal: /graal/i.test(out) };
};

const gradleJavaHome = () => {
  const usable = (h) => {
    if (!h || !existsSync(h)) return false;
    const { major, graal } = javaProps(h);
    return major === JDK && !graal;
  };
  if (usable(process.env.JAVA_HOME)) return process.env.JAVA_HOME;

  const candidates = [];
  for (const k of Object.keys(process.env)) if (/^JAVA_HOME_21/.test(k)) candidates.push(process.env[k]);
  const mac = spawnSync('/usr/libexec/java_home', ['-v', String(JDK)], { encoding: 'utf8' });
  if (mac.status === 0) candidates.push(mac.stdout.trim());
  for (const root of [join(process.env.HOME ?? '', '.sdkman/candidates/java'), '/Library/Java/JavaVirtualMachines'])
    if (existsSync(root))
      for (const e of readdirSync(root).sort().reverse())
        candidates.push(existsSync(join(root, e, 'Contents/Home')) ? join(root, e, 'Contents/Home') : join(root, e));

  const found = candidates.find(usable);
  if (!found) {
    const here = javaProps(process.env.JAVA_HOME ?? '');
    die(
      `no stock JDK ${JDK} found (JAVA_HOME is ${process.env.JAVA_HOME ?? 'unset'}` +
        `${here.major ? `, java ${here.major}${here.graal ? ' GraalVM' : ''}` : ''}).\n` +
        `  Capacitor 8 needs source ${JDK}, and GraalVM's jlink breaks AGP. Install one:\n` +
        '    sdk install java 21.0.11-tem      # or: brew install --cask temurin@21',
    );
  }
  return found;
};

const javaHome = gradleJavaHome();
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
  track,
  releaseName: `${versionName} (${versionCode})`,
  notes: notes.text,
  userFraction: flag('rollout'),
  draft: has('draft'),
  log: (m) => console.log(m),
});

console.log(
  `\n✓ ${versionName} (${res.versionCode}) is on the ${track} track [${res.status}].\n` +
    (track === 'beta'
      ? '  Open testing: share the link from Play Console ▸ Testing ▸ Open testing ▸ Testers ▸ Copy link.\n' +
        '  First rollout on a new open track waits for Google review (hours to a day); later ones do not.\n'
      : track === 'internal'
        ? '  Internal testing is live within minutes for the testers on that list.\n'
        : ''),
);

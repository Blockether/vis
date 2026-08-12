// Which JDK Gradle gets for an Android release build, and why it is never simply
// the one on PATH.
//
// This lives apart from `android-release.mjs` because that script is a top-level
// side-effecting release runner: importing it starts a release, so the choice that
// decides whether the Android leg can run at all could not be tested in place.
// It is also the PREFLIGHT answer — `node scripts/jdk.mjs` prints the JDK the build
// will really use. Asking a stricter probe instead is how a perfectly capable
// machine gets declared unfit: `/usr/libexec/java_home` only knows about JDKs
// registered under /Library/Java/JavaVirtualMachines and is blind to SDKMAN, which
// is exactly where a developer machine keeps its stock 21.
import { spawnSync } from 'node:child_process';
import { existsSync, readdirSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

// Three separate failures live here, and all three only bite the RELEASE build,
// long after a debug run looked fine:
//   • JDK 25 (the repo-wide GraalVM pin, so usually this machine's default):
//     Gradle 8.14 dies parsing the build script, "Unsupported class file major
//     version 69".
//   • JDK 17: Capacitor 8 compiles with `source 21` — "invalid source release: 21".
//   • GraalVM (any version): AGP's JdkImageTransform shells out to `jlink
//     --disable-plugin system-modules`, which Graal's jlink rejects.
// So: exactly 21, and a stock JDK. CI's temurin 21 satisfies JAVA_HOME and skips
// the search.
export const JDK = 21;

/** Version and vendor of a JDK home, without trusting its directory name. */
export const javaProps = (home) => {
  const bin = join(home, 'bin', 'java');
  if (!existsSync(bin)) return { major: 0, graal: false };
  const res = spawnSync(bin, ['-version'], { encoding: 'utf8' });
  const out = `${res.stderr ?? ''}${res.stdout ?? ''}`;
  return { major: Number(/version "(\d+)/.exec(out)?.[1] ?? 0), graal: /graal/i.test(out) };
};

const listDir = (dir) => (existsSync(dir) ? readdirSync(dir).sort().reverse() : []);

const macJavaHome = () => {
  const res = spawnSync('/usr/libexec/java_home', ['-v', String(JDK)], { encoding: 'utf8' });
  return res.status === 0 ? res.stdout.trim() : '';
};

/** Where whole JDKs live on a machine, most specific first. */
export const jdkRoots = (env) => [
  join(env.HOME ?? '', '.sdkman/candidates/java'),
  '/Library/Java/JavaVirtualMachines',
];

/**
 * The JDK home Gradle should build with, or '' when this machine has none.
 *
 * Every input is injectable so the ordering — which is the whole point — can be
 * tested without the machine that happens to run the test.
 *
 * @param {object} [deps]
 * @param {Record<string, string|undefined>} [deps.env] process environment
 * @param {(path: string) => boolean} [deps.exists]
 * @param {(dir: string) => string[]} [deps.list] entries of a JDK root, newest name first
 * @param {(home: string) => {major: number, graal: boolean}} [deps.props]
 * @param {() => string} [deps.mac] `/usr/libexec/java_home -v 21`, '' when it knows none
 * @returns {string}
 */
export const pickJdk = ({
  env = process.env,
  exists = existsSync,
  list = listDir,
  props = javaProps,
  mac = macJavaHome,
} = {}) => {
  const usable = (home) => {
    if (!home || !exists(home)) return false;
    const { major, graal } = props(home);
    return major === JDK && !graal;
  };
  if (usable(env.JAVA_HOME)) return env.JAVA_HOME;

  const candidates = [];
  for (const key of Object.keys(env)) if (/^JAVA_HOME_21/.test(key)) candidates.push(env[key]);
  const registered = mac();
  if (registered) candidates.push(registered);
  for (const root of jdkRoots(env))
    for (const entry of list(root)) {
      const bundled = join(root, entry, 'Contents/Home');
      candidates.push(exists(bundled) ? bundled : join(root, entry));
    }

  return candidates.find(usable) ?? '';
};

/** What to say when nothing on this machine can build the bundle. */
export const jdkHelp = (env = process.env, props = javaProps) => {
  const here = props(env.JAVA_HOME ?? '');
  return (
    `no stock JDK ${JDK} found (JAVA_HOME is ${env.JAVA_HOME ?? 'unset'}` +
    `${here.major ? `, java ${here.major}${here.graal ? ' GraalVM' : ''}` : ''}).\n` +
    `  Capacitor 8 needs source ${JDK}, and GraalVM's jlink breaks AGP. Install one:\n` +
    '    sdk install java 21.0.11-tem      # or: brew install --cask temurin@21'
  );
};

// `node scripts/jdk.mjs` — the Android-leg preflight. Prints the JDK the release
// build will use, or the install line and a non-zero exit when there is none.
if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const home = pickJdk();
  if (!home) {
    console.error(jdkHelp());
    process.exit(1);
  }
  console.log(home);
}

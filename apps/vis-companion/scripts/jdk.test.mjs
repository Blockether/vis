import { describe, expect, it } from 'vitest';

import { jdkHelp, pickJdk } from './jdk.mjs';

// A fake machine: every JDK it has, keyed by its home, plus what
// `/usr/libexec/java_home -v 21` on macOS admits to knowing.
const machine = ({ homes, registered = '', env = {} }) => ({
  env: { HOME: '/home/dev', ...env },
  exists: (path) => Object.keys(homes).some((home) => home === path),
  list: (root) =>
    Object.keys(homes)
      .filter((home) => home.startsWith(`${root}/`))
      .map((home) => home.slice(root.length + 1).split('/')[0])
      .sort()
      .reverse(),
  props: (home) => homes[home] ?? { major: 0, graal: false },
  mac: () => registered,
});

const stock21 = { major: 21, graal: false };
const graal21 = { major: 21, graal: true };
const sdkman = '/home/dev/.sdkman/candidates/java';

describe('pickJdk', () => {
  it('keeps a stock 21 JAVA_HOME and searches no further', () => {
    const found = pickJdk(
      machine({
        homes: { '/opt/temurin-21': stock21, [`${sdkman}/21.0.11-tem`]: stock21 },
        env: { JAVA_HOME: '/opt/temurin-21' },
      }),
    );
    expect(found).toBe('/opt/temurin-21');
  });

  // The everyday state of this repo: the toolchain pins GraalVM 25 and exports it as
  // JAVA_HOME, while the only JDK that can build the bundle sits in SDKMAN. Inheriting
  // JAVA_HOME would fail inside AGP's JdkImageTransform, minutes into the release.
  it('walks past a GraalVM JAVA_HOME to the stock 21 in SDKMAN', () => {
    const found = pickJdk(
      machine({
        homes: {
          '/opt/graalvm-25': { major: 25, graal: true },
          [`${sdkman}/25.1.3-graalce`]: { major: 25, graal: true },
          [`${sdkman}/21.0.7-graal`]: graal21,
          [`${sdkman}/21.0.11-tem`]: stock21,
          [`${sdkman}/17.0.12-oracle`]: { major: 17, graal: false },
        },
        env: { JAVA_HOME: '/opt/graalvm-25' },
      }),
    );
    expect(found).toBe(`${sdkman}/21.0.11-tem`);
  });

  // The preflight regression: `/usr/libexec/java_home` only reports JDKs registered
  // under /Library/Java/JavaVirtualMachines, so on a SDKMAN-only machine it answers
  // "Unable to locate a Java Runtime" while a usable stock 21 is installed. A search
  // that trusted it would call the Android leg impossible and divert to CI.
  it('finds a JDK that macOS java_home cannot see', () => {
    const found = pickJdk(
      machine({ homes: { [`${sdkman}/21.0.11-tem`]: stock21 }, registered: '' }),
    );
    expect(found).toBe(`${sdkman}/21.0.11-tem`);
  });

  it('resolves the Contents/Home of a /Library install', () => {
    const home = '/Library/Java/JavaVirtualMachines/temurin-21.jdk/Contents/Home';
    expect(pickJdk(machine({ homes: { [home]: stock21 } }))).toBe(home);
  });

  it("prefers CI's JAVA_HOME_21_* over anything installed on the box", () => {
    const found = pickJdk(
      machine({
        homes: { '/runner/jdk-21': stock21, [`${sdkman}/21.0.11-tem`]: stock21 },
        env: { JAVA_HOME_21_X64: '/runner/jdk-21' },
      }),
    );
    expect(found).toBe('/runner/jdk-21');
  });

  it('rejects every near miss rather than starting a build that dies in Gradle', () => {
    const found = pickJdk(
      machine({
        homes: {
          [`${sdkman}/25.1.3-graalce`]: { major: 25, graal: true },
          [`${sdkman}/21.0.7-graal`]: graal21,
          [`${sdkman}/17.0.12-oracle`]: { major: 17, graal: false },
        },
        env: { JAVA_HOME: `${sdkman}/21.0.7-graal` },
      }),
    );
    expect(found).toBe('');
  });

  it('ignores a JAVA_HOME pointing at a directory that is gone', () => {
    const found = pickJdk(
      machine({
        homes: { [`${sdkman}/21.0.11-tem`]: stock21 },
        env: { JAVA_HOME: '/opt/removed-jdk-21' },
      }),
    );
    expect(found).toBe(`${sdkman}/21.0.11-tem`);
  });
});

describe('jdkHelp', () => {
  it('names what JAVA_HOME actually is, so the reader knows why it was refused', () => {
    const help = jdkHelp({ JAVA_HOME: '/opt/graalvm-25' }, () => ({ major: 25, graal: true }));
    expect(help).toContain('/opt/graalvm-25');
    expect(help).toContain('java 25 GraalVM');
    expect(help).toContain('sdk install java 21.0.11-tem');
  });

  it('says so when there is no JAVA_HOME at all', () => {
    expect(jdkHelp({}, () => ({ major: 0, graal: false }))).toContain('JAVA_HOME is unset');
  });
});

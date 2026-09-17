// The suite's rules about the TESTS, not about the app. These scan the test sources
// instead of running them, because a test that cannot fail is worse than no test: it
// reports green forever over code nobody is checking any more.
//
// Every rule here describes a shape that was already wrong in this suite. 486 existence
// checks hid, among other things, two ActivityPanel tests that asserted on a thread the
// panel kept hidden — `toBeVisible` failed the moment the weak matchers were gone.
import { describe, expect, it } from 'vitest';

// Every test source in the app, as text. This file is excluded: it has to spell the
// forbidden matchers out to look for them.
const suites = Object.entries(
  import.meta.glob(['./**/*.test.ts', './**/*.test.tsx', '!./tests.conventions.test.ts'], {
    query: '?raw',
    import: 'default',
    eager: true,
  }) as Record<string, string>,
);

/** Matchers that only prove a value exists, in the two spellings that carry no other meaning. */
const EXISTENCE_ONLY = /\.(?:toBeDefined|toBeTruthy)\(\)|\.not\.(?:toBeNull|toBeUndefined|toBeFalsy)\(\)/g;

/** `it(` / `test(` / `it.each(` at the start of a line, with the body that follows it. */
const TESTS = /^[ \t]*(?:it|test)(?:\.\w+)*\s*\(/gm;

/** `expect(x).toBe(x)` for a literal `x`: a placeholder somebody never finished. */
const TAUTOLOGY = /expect\((true|false|null|undefined|-?\d+|'[^']*')\)\.(?:toBe|toEqual)\(\1\)/g;

describe('the tests stay honest', () => {
  // Around a `getBy*`/`findBy*` query these prove nothing whatsoever: the query already
  // throws when it finds nothing, so the assertion cannot fail on its own. Say what the
  // value should BE instead — `toBeVisible`, `toBeInTheDocument`, `toBe`, `toEqual`,
  // `toMatch`, `toHaveLength`, `toHaveBeenCalledWith`.
  it('asserts what a value is, never merely that it exists', () => {
    for (const [path, source] of suites) {
      expect([...source.matchAll(EXISTENCE_ONLY)].map(([matcher]) => matcher), path).toEqual([]);
    }
  });

  // A test with no expectation passes whatever the app does. Each test is read up to the
  // next one, so reaching the expectations through a helper is fine as long as the call
  // is inside the test.
  it('leaves no test without an expectation', () => {
    for (const [path, source] of suites) {
      const starts = [...source.matchAll(TESTS)].map((start) => start.index ?? 0);
      const silent = starts
        .filter((at, index) => !source.slice(at, starts[index + 1] ?? source.length).includes('expect'))
        .map((at) => source.slice(at, source.indexOf('\n', at)).trim());
      expect(silent, path).toEqual([]);
    }
  });

  it('compares no literal with itself', () => {
    for (const [path, source] of suites) {
      expect([...source.matchAll(TAUTOLOGY)].map(([tautology]) => tautology), path).toEqual([]);
    }
  });
});

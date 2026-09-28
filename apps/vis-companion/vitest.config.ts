import { storybookTest } from '@storybook/addon-vitest/vitest-plugin';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { defineConfig } from 'vitest/config';

import pkg from './package.json' with { type: 'json' };
import { companionBuildInfo } from './scripts/build-info.ts';
import { testWorkers } from './scripts/test-workers.ts';

const dirname = path.dirname(fileURLToPath(import.meta.url));
const buildInfo = companionBuildInfo();

// Deliberately NOT an extension of `vite.config.ts`: the app config exists to
// build a browser bundle (React Compiler, Tailwind, the dev gateway proxy), and
// none of that helps these suites, which run in node and jsdom.
export default defineConfig({
  // `compat.ts` reads the release string the app build injects; the tests need
  // the SAME source of truth, not a hand-written stand-in.
  define: {
    __VIS_APP_VERSION__: JSON.stringify(pkg.version),
    __VIS_APP_BUILD_NUMBER__: JSON.stringify(buildInfo.buildNumber),
    __VIS_APP_BUILD_COMMIT__: JSON.stringify(buildInfo.commit),
  },
  test: {
    // Concurrent runs on one machine share its cores (`scripts/test-workers.ts`).
    maxWorkers: await testWorkers(),
    // A `findBy*` may wait five seconds (`src/test-setup.ts`, `.storybook/preview.tsx`),
    // and a test on a busy machine can need several. A story play that outlives its
    // test keeps typing into the next story's document.
    testTimeout: 15_000,
    projects: [
      {
        extends: true,
        test: {
          name: 'unit',
          // Pure logic modules only. Anything needing a DOM says so per file
          // with a `@vitest-environment` docblock rather than slowing every run.
          environment: 'node',
          include: ['src/**/*.test.ts', 'src/**/*.test.tsx'],
          // Testing Library's matchers and its unmount-between-tests. The setup
          // no-ops under node, so pure logic pays nothing for it.
          setupFiles: ['./src/test-setup.ts'],
          // A VM context per file rather than a worker per file: the same isolation
          // without starting a worker for each one. `vmThreads` would hold every
          // context in one heap.
          pool: 'vmForks',
        },
      },
      {
        extends: true,
        test: {
          name: 'scripts',
          environment: 'node',
          include: ['scripts/**/*.test.mjs'],
          setupFiles: ['./src/test-setup.ts'],
          // Some of these drive the real toolchain, and Rolldown's native binding
          // refuses objects handed to it from a `node:vm` realm: a real process each.
          pool: 'forks',
        },
      },
      {
        extends: true,
        plugins: [storybookTest({ configDir: path.join(dirname, '.storybook') })],
        test: {
          name: 'storybook',
          // Stories play in jsdom, like the unit suite: behaviour and semantics.
          // Layout, colour and scrolling are checked in the browser Storybook.
          environment: 'jsdom',
          setupFiles: ['./src/test-setup.ts'],
          pool: 'vmForks',
        },
      },
    ],
  },
});

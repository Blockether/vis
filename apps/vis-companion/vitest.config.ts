import tailwindcss from '@tailwindcss/vite';
import { storybookTest } from '@storybook/addon-vitest/vitest-plugin';
import { playwright } from '@vitest/browser-playwright';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { defineConfig } from 'vitest/config';

import pkg from './package.json' with { type: 'json' };
import { companionBuildInfo } from './scripts/build-info.ts';

const dirname = path.dirname(fileURLToPath(import.meta.url));
const buildInfo = companionBuildInfo();

// Deliberately NOT an extension of `vite.config.ts`: the app config exists to
// build a browser bundle (React Compiler, Tailwind, the dev gateway proxy), and
// none of that helps the node suite. The browser project compiles the same Tailwind
// CSS as the app so geometry and hit-testing assertions exercise the shipped layout.
export default defineConfig({
  // `compat.ts` reads the release string the app build injects; the tests need
  // the SAME source of truth, not a hand-written stand-in.
  define: {
    __VIS_APP_VERSION__: JSON.stringify(pkg.version),
    __VIS_APP_BUILD_NUMBER__: JSON.stringify(buildInfo.buildNumber),
    __VIS_APP_BUILD_COMMIT__: JSON.stringify(buildInfo.commit),
  },
  test: {
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
          // A VM context per FILE, not a worker per file: the same isolation (fresh
          // module registry, fresh jsdom) without starting a worker and building a
          // DOM for every one of them. On a two-core box — what CI gets — this suite
          // goes 124s -> 46s. `vmThreads` is a second faster on a laptop but keeps
          // every context in ONE heap (9GB against 0.8GB here), which is how a run
          // dies on a runner instead of finishing.
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
          // These drive the real toolchain: a production Vite build, a Playwright
          // browser. Rolldown's native binding refuses objects handed to it from a
          // `node:vm` realm, so the toolchain tests keep a real process.
          pool: 'forks',
        },
      },
      {
        extends: true,
        plugins: [tailwindcss(), storybookTest({ configDir: path.join(dirname, '.storybook') })],
        test: {
          name: 'storybook',
          // CI's two-core runner plays the longest interaction stories at half of Vitest's
          // 15s default. The limit is there to catch a hung story, not a busy runner.
          testTimeout: 30_000,
          // Story files play in iframes of one origin: without their own storage they
          // start from what another story file left (see `src/test-storage.ts`).
          setupFiles: ['./src/test-storage.ts'],
          browser: {
            enabled: true,
            headless: true,
            // Exercise the shipped reduced-motion path; immediate interaction assertions
            // must not race the first transparent frame of real CSS entrance animations.
            provider: playwright({ contextOptions: { reducedMotion: 'reduce' } }),
            instances: [{ browser: 'chromium' }],
          },
        },
      },
    ],
  },
});

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
// none of that helps the node suite. The Storybook project below brings only
// Storybook's own Vite plugin back, then drives every story in real Chromium.
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
          // Pure logic modules only. Anything needing a DOM says so per file
          // with a `@vitest-environment` docblock rather than slowing every run.
          environment: 'node',
          include: ['src/**/*.test.ts', 'src/**/*.test.tsx', 'scripts/**/*.test.mjs'],
          // Testing Library's matchers and its unmount-between-tests. The setup
          // no-ops under node, so pure logic pays nothing for it.
          setupFiles: ['./src/test-setup.ts'],
        },
      },
      {
        extends: true,
        plugins: [storybookTest({ configDir: path.join(dirname, '.storybook') })],
        test: {
          name: 'storybook',
          browser: {
            enabled: true,
            headless: true,
            provider: playwright({}),
            instances: [{ browser: 'chromium' }],
          },
        },
      },
    ],
  },
});
import { defineConfig } from 'vitest/config';
import pkg from './package.json' with { type: 'json' };

// Deliberately NOT an extension of `vite.config.ts`: the app config exists to
// build a browser bundle (React Compiler babel pass, Tailwind, Prism
// pre-bundling, the dev gateway proxy) and none of that helps a unit test — it
// only makes the run slower and couples the suite to bundler plumbing.
export default defineConfig({
  test: {
    // Pure logic modules only. Anything needing a DOM should say so per file
    // with a `@vitest-environment` docblock rather than slowing every run.
    environment: 'node',
    // `scripts/` holds the release plumbing; only its side-effect-free helpers
    // are importable, and those are exactly the ones worth pinning down.
    include: ['src/**/*.test.ts', 'src/**/*.test.tsx', 'scripts/**/*.test.mjs'],
  },
  // `compat.ts` reads the release string the app build injects; the tests need
  // the SAME source of truth, not a hand-written stand-in.
  define: { __VIS_APP_VERSION__: JSON.stringify(pkg.version) },
});

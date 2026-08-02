import { defineConfig } from 'vitest/config';

// Deliberately NOT an extension of `vite.config.ts`: the app config exists to
// build a browser bundle (React Compiler babel pass, Tailwind, Prism
// pre-bundling, the dev gateway proxy) and none of that helps a unit test — it
// only makes the run slower and couples the suite to bundler plumbing.
export default defineConfig({
  test: {
    // Pure logic modules only. Anything needing a DOM should say so per file
    // with a `@vitest-environment` docblock rather than slowing every run.
    environment: 'node',
    include: ['src/**/*.test.ts', 'src/**/*.test.tsx'],
  },
});

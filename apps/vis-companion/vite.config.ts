import tailwindcss from '@tailwindcss/vite';
import babel from '@rolldown/plugin-babel';
import react, { reactCompilerPreset } from '@vitejs/plugin-react';
import { defineConfig } from 'vite';
import pkg from './package.json' with { type: 'json' };
import {
  devConnectionStorageScript,
  discoverDevGatewayConnections,
} from './scripts/dev-gateway.ts';

// https://vite.dev/config/
export default defineConfig(async ({ command }) => {
  const devGateways = command === 'serve' ? await discoverDevGatewayConnections() : [];
  if (command === 'serve') {
    console.info(
      devGateways.length > 0
        ? `[vis] auto-connecting ${devGateways.length} local gateway${devGateways.length === 1 ? '' : 's'}`
        : '[vis] no live local gateway found; starting unpaired',
    );
  }

  return {
    // The app stamps its release version on every gateway request and shows it
    // on the version-mismatch screen. package.json is only a MIRROR of the
    // repo-root VIS_VERSION file (stamped by `scripts/version.mjs`, run from
    // `prebuild`/`predev` and every release script) so app and gateway ship the
    // same number.
    define: { __VIS_APP_VERSION__: JSON.stringify(pkg.version) },
    plugins: [
      {
        name: 'vis-dev-gateway-autoconnect',
        apply: 'serve',
        transformIndexHtml() {
          const children = devConnectionStorageScript(devGateways);
          return children
            ? [{ tag: 'script', children, injectTo: 'head-prepend' as const }]
            : [];
        },
      },
      react(),
      // React Compiler is this app's React checker AND its optimizer: it runs the
      // full Rules-of-React static analysis (purity, immutability, hook rules,
      // preserved manual memoization) on every build. `panicThreshold:
      // 'critical_errors'` makes a real Rules-of-React violation FAIL the build
      // instead of silently bailing out of memoization, while still tolerating
      // syntax the compiler simply cannot lower yet (e.g. try/finally).
      babel({
        presets: [reactCompilerPreset({ target: '19', panicThreshold: 'critical_errors' })],
      }),
      tailwindcss(),
    ],
    optimizeDeps: {
      // Pre-bundle Prism + its language components together at startup so a
      // lazy cold re-optimize can't reload them out of dependency order
      // (prism-tsx extends prism-typescript + prism-jsx and crashes if they
      // haven't executed first).
      include: [
        'prismjs',
        'prismjs/components/prism-bash',
        'prismjs/components/prism-clojure',
        'prismjs/components/prism-css',
        'prismjs/components/prism-diff',
        'prismjs/components/prism-java',
        'prismjs/components/prism-json',
        'prismjs/components/prism-markdown',
        'prismjs/components/prism-python',
        'prismjs/components/prism-typescript',
        'prismjs/components/prism-jsx',
        'prismjs/components/prism-tsx',
        'prismjs/components/prism-yaml',
      ],
    },
    server: {
      // The injected bearer tokens make the dev page privileged. Keep the default
      // on loopback; a deliberate CLI --host may still override it.
      host: '127.0.0.1',
      port: 5273,
    },
  };
});

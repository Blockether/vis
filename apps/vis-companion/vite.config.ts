import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import tailwindcss from '@tailwindcss/vite';
import babel from '@rolldown/plugin-babel';
import react, { reactCompilerPreset } from '@vitejs/plugin-react';
import { defineConfig, type PreviewServer } from 'vite';
import pkg from './package.json' with { type: 'json' };
import {
  devConnectionStorageScript,
  discoverDevGatewayConnections,
  prependHeadScript,
  sameOriginConnectionStorageScript,
} from './scripts/dev-gateway.ts';
import { companionBuildInfo } from './scripts/build-info.ts';

const buildInfo = companionBuildInfo();
// https://vite.dev/config/
export default defineConfig(async ({ command, mode }) => {
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
    define: {
      __VIS_APP_VERSION__: JSON.stringify(pkg.version),
      __VIS_APP_BUILD_NUMBER__: JSON.stringify(buildInfo.buildNumber),
      __VIS_APP_BUILD_COMMIT__: JSON.stringify(buildInfo.commit),
    },
    // `vite build --mode web` is the bundle a gateway serves from its own root
    // (`vis-agent web`, attached to every release as vis-web.tar.gz). It gets its
    // own folder so it never mixes with the Capacitor build in `dist/`.
    // `--mode perf` (`npm run perf`) is the released bundle with the memory overlay on:
    // React's production build, left unminified with source maps so the overlay and
    // heap snapshots name the app's own functions.
    build:
      mode === 'perf'
        ? { outDir: 'dist-perf', minify: false, sourcemap: true }
        : { outDir: mode === 'web' ? 'dist-web' : 'dist' },
    // The preview page carries the same bearer tokens as the dev page: loopback only.
    preview: { host: '127.0.0.1', port: 5274 },
    plugins: [
      {
        name: 'vis-dev-gateway-autoconnect',
        apply: 'serve',
        transformIndexHtml() {
          const children = devConnectionStorageScript(devGateways);
          return children ? [{ tag: 'script', children, injectTo: 'head-prepend' as const }] : [];
        },
      },
      {
        // `vite preview` has no HTML transform, so the preview server seeds the same
        // connections into each page response itself.
        name: 'vis-preview-gateway-autoconnect',
        configurePreviewServer(server: PreviewServer) {
          if (devGateways.length === 0) return;
          const script = devConnectionStorageScript(devGateways);
          const page = resolve(server.config.root, server.config.build.outDir, 'index.html');
          server.middlewares.use((request, response, next) => {
            const path = (request.url ?? '/').split('?')[0];
            if (request.method !== 'GET' || (path !== '/' && path !== '/index.html')) return next();
            response.setHeader('Content-Type', 'text/html; charset=utf-8');
            response.setHeader('Cache-Control', 'no-store');
            response.end(prependHeadScript(readFileSync(page, 'utf8'), script));
          });
        },
      },
      {
        // Only the gateway-served bundle knows its origin is a gateway. Capacitor
        // builds run on `https://localhost` and must never save that as a machine.
        name: 'vis-gateway-same-origin',
        apply: 'build',
        transformIndexHtml() {
          const children = mode === 'web' ? sameOriginConnectionStorageScript() : '';
          return children ? [{ tag: 'script', children, injectTo: 'head-prepend' as const }] : [];
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
        // The diagram renderer is imported lazily, so dev would otherwise
        // discover it mid-session and reload the page under the reader.
        'mermaid',
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

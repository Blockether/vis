import type { CapacitorConfig } from '@capacitor/cli';

// Typed, single source of truth for the Capacitor config.
//
// Why `.mts` + a codegen step instead of the usual `capacitor.config.ts`?
// TypeScript 7 (tsgo) dropped the classic synchronous compiler API
// (`ts.transpileModule` / `ts.ModuleKind`) that the Capacitor CLI uses to load
// a `capacitor.config.ts`. With TS 7 installed, `cap sync` crashes with
// `Cannot read properties of undefined (reading 'CommonJS')`
// (ionic-team/capacitor#8531). The CLI only auto-loads `capacitor.config.ts`,
// then `.js`, then `.json` — it never touches `.mts`. So we keep this typed
// `.mts` as the source, transpile it with jiti (which has its own transpiler and
// never imports the `typescript` package), and emit `capacitor.config.json` that
// the CLI reads. Result: full TS types AND TS 7, with a stable CLI.
//
// Edit THIS file, never `capacitor.config.json`. Run `npm run cap:config`
// (auto-run by `npm run sync` / `ios` / `android`) to regenerate the JSON.
const config: CapacitorConfig = {
  appId: 'com.blockether.viscompanion',
  appName: 'Vis',
  webDir: 'dist',
  server: {
    androidScheme: 'https',
  },
  ios: {
    scheme: 'vis',
  },
};

export default config;

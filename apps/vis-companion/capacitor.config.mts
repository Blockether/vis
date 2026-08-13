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
  // Matches --bg of the default light palette; WKWebView paints this behind
  // unrendered tiles during fast scroll instead of flashing white.
  backgroundColor: '#faf3eb',
  server: {
    androidScheme: 'https',
  },
  android: {
    allowMixedContent: true,
  },
  ios: {
    // Xcode scheme name in ios/App/App.xcodeproj — NOT a URL scheme.
    scheme: 'App',
    // The web layer owns every scroller (body is overflow:hidden). Leaving
    // WKWebView's own UIScrollView live let a sideways drag pan and rubber-band
    // the entire UI, and each bounce re-composited the whole page.
    scrollEnabled: false,
    contentInset: 'never',
  },
  plugins: {
    Keyboard: {
      // Keep WKWebView full-height. The plugin's `keyboardWillShow` event
      // supplies the final keyboard height; the shell applies it immediately
      // instead of adding a second 250 ms web animation after the native delay.
      resize: 'none',
    },
  },
};

export default config;

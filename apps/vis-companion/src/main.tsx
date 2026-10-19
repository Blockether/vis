import './perf-boot';
import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { App } from './App';
import { ErrorBoundary } from './components/ErrorBoundary';
import { captureContextMenu, captureLinkClicks } from './lib/desktop';
import { installDiagnostics } from './lib/diagnostics';
import { loadHost, stampShellHost } from './lib/host';
import { perfActive } from './lib/perf';
import { paintStoredTheme } from './lib/theme';
import { takeWebLaunch } from './lib/web-launch';
import './index.css';

installDiagnostics();
// Pake reads every click for itself once the document is parsed. A module script runs before
// that, so this bundle decides first which links leave the app and which it draws itself.
captureLinkClicks();
// The desktop window is not a browser, so it does not offer a browser's right-click
// menu either. Rows drop their own menu there instead (`SwipeActions`).
captureContextMenu();
// The desktop app and a browser tab share this bundle. Name the host before the first
// paint, so window styling (`desktop:`) never shows in a tab (#381).
stampShellHost();
// The palette is decided before the first render, never after it: `App` used to
// read the preference through the native bridge and repaint from an effect, so
// a dark-theme device painted the light default first — a white sheet under the
// splash on every launch and on every reload iOS performs after recycling the
// backgrounded webview. `index.html` already stamped the raw stored id during
// parse; this settles it against the shipped catalog.
paintStoredTheme();
// One native question, asked before any field can be focused: is this iOS build
// a Mac window? The keyboard driver reads the answer synchronously (`pointer.ts`).
void loadHost();
// A `vis-agent web` launch rides in the fragment. Take it out before the hash router
// reads the address, so the used code never stays in the address bar or the history.
const launch = typeof window === 'undefined' ? null : takeWebLaunch();

const root = createRoot(document.getElementById('root')!);

root.render(
  <StrictMode>
    <ErrorBoundary>
      <App launch={launch} />
    </ErrorBoundary>
  </StrictMode>,
);

// The memory overlay, when Settings, `?perf=1` or the `perf` build turned it on: in its
// own root, so the app's tree and layout stay exactly what a normal launch renders.
if (perfActive()) void import('./components/PerfOverlay').then((overlay) => overlay.mountPerfOverlay());

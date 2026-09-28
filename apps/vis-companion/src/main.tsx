import './perf-boot';
import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { App } from './App';
import { ErrorBoundary } from './components/ErrorBoundary';
import { captureContextMenu, captureLinkClicks } from './lib/desktop';
import { installDiagnostics } from './lib/diagnostics';
import { loadHost } from './lib/host';
import { perfActive } from './lib/perf';
import { paintStoredTheme } from './lib/theme';
import './index.css';

installDiagnostics();
// Pake reads every click for itself once the document is parsed. A module script runs before
// that, so this bundle decides first which links leave the app and which it draws itself.
captureLinkClicks();
// The desktop window is not a browser, so it does not offer a browser's right-click
// menu either. Rows drop their own menu there instead (`SwipeActions`).
captureContextMenu();
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

const root = createRoot(document.getElementById('root')!);

root.render(
  <StrictMode>
    <ErrorBoundary>
      <App />
    </ErrorBoundary>
  </StrictMode>,
);

// The memory overlay, when Settings, `?perf=1` or the `perf` build turned it on: in its
// own root, so the app's tree and layout stay exactly what a normal launch renders.
if (perfActive()) void import('./components/PerfOverlay').then((overlay) => overlay.mountPerfOverlay());

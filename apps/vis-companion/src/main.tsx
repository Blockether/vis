import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { App } from './App';
import { ErrorBoundary } from './components/ErrorBoundary';
import { installDiagnostics } from './lib/diagnostics';
import { paintStoredTheme } from './lib/theme';
import './index.css';

installDiagnostics();
// The palette is decided before the first render, never after it: `App` used to
// read the preference through the native bridge and repaint from an effect, so
// a dark-theme device painted the light default first — a white sheet under the
// splash on every launch and on every reload iOS performs after recycling the
// backgrounded webview. `index.html` already stamped the raw stored id during
// parse; this settles it against the shipped catalog.
paintStoredTheme();

const root = createRoot(document.getElementById('root')!);

root.render(
  <StrictMode>
    <ErrorBoundary>
      <App />
    </ErrorBoundary>
  </StrictMode>,
);

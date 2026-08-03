import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { App } from './App';
import { ErrorBoundary } from './components/ErrorBoundary';
import './index.css';

const root = createRoot(document.getElementById('root')!);

// `#/__design` is the design gallery (`src/dev/DesignGallery.tsx`): proposed
// screens in the real shell, for screenshots. `import.meta.env.DEV` is a build
// constant, so the whole branch — and the dev chunk behind it — is dropped from
// a store build.
if (import.meta.env.DEV && window.location.hash.startsWith('#/__design')) {
  // The gallery reads its variant out of the hash at mount, so moving between
  // proposals is a reload, not a router.
  window.addEventListener('hashchange', () => window.location.reload());
  void import('./dev/DesignGallery').then(({ DesignGallery }) => {
    root.render(
      <StrictMode>
        <DesignGallery />
      </StrictMode>,
    );
  });
} else {
  root.render(
    <StrictMode>
      <ErrorBoundary>
        <App />
      </ErrorBoundary>
    </StrictMode>,
  );
}

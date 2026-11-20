import { expect, it } from 'vitest';

import * as shell from './App';

// User report: each change to the app shell reloaded the whole desktop app and closed
// its open panel. Vite's React refresh takes a module only when each export is a
// component or keeps its value, and `main.tsx` above `App.tsx` reloads the page.
it('exports only components from the app shell, so a change refreshes in place', () => {
  const others = Object.entries(shell)
    .filter(([name, value]) => typeof value !== 'function' || !/^[A-Z]/.test(name))
    .map(([name]) => name);
  expect(others).toEqual([]);
});

import { expect, it } from 'vitest';

import * as shell from './App';
import * as appNodes from './components/AppNodes';
import * as globalApps from './components/GlobalApps';

// User report: each change to the app shell reloaded the whole desktop app and closed
// its open panel. Vite's React refresh takes a module only when each export is a
// component or keeps its value, and `main.tsx` above `App.tsx` reloads the page.
function notComponents(module: Record<string, unknown>): string[] {
  return Object.entries(module)
    .filter(([name, value]) => typeof value !== 'function' || !/^[A-Z]/.test(name))
    .map(([name]) => name);
}

it('exports only components from the app shell, so a change refreshes in place', () => {
  expect(notComponents(shell)).toEqual([]);
});

// User report: a change to the review diff closed the open review panel. The update
// went up through GlobalApps to the shell, which made its split session screen again.
it('exports only components from the global apps, so a change below them stops there', () => {
  expect(notComponents(globalApps)).toEqual([]);
});

it('exports only components from the review diff, so a change to it refreshes only it', () => {
  expect(notComponents(appNodes)).toEqual([]);
});

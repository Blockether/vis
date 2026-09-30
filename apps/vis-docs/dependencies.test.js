import { readFileSync } from 'node:fs';
import { expect, test } from 'vitest';

const manifest = JSON.parse(readFileSync(new URL('./package.json', import.meta.url), 'utf8'));
const lock = JSON.parse(readFileSync(new URL('./package-lock.json', import.meta.url), 'utf8'));

test('every HTTP client copy uses the patched undici override', () => {
  expect(manifest.overrides.undici).toBe('7.29.1');
  const clients = Object.entries(lock.packages)
    .filter(([path]) => path === 'node_modules/undici' || path.endsWith('/node_modules/undici'))
    .map(([, pkg]) => pkg.version);
  expect(clients.length).toBeGreaterThan(0);
  expect(new Set(clients)).toEqual(new Set([manifest.overrides.undici]));
});

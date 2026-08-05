import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import { describe, expect, it } from 'vitest';

const prepare = readFileSync(join(dirname(fileURLToPath(import.meta.url)), 'ios-prepare.mjs'), 'utf8');

describe('iOS background keyboard release', () => {
  // Regression, TestFlight build 3423: backgrounding with an active WebKit editor
  // deadlocked UIKeyboardTaskQueue during the scene update until iOS watchdog-killed Vis.
  it('ends editing synchronously before the application resigns active', () => {
    expect(prepare).toContain('func applicationWillResignActive(_ application: UIApplication)');
    expect(prepare).toContain('window?.endEditing(true)');
  });
});

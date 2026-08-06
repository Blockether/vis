import { describe, expect, it } from 'vitest';
import source from './ConnectScreen.tsx?raw';

// Regression (reported: "width discrepancies between the session list and the machines"): the Machines screen was capped at a narrow desktop column while Sessions used the full app frame.
describe('Machines screen frame', () => {
  it('uses the same full-width desktop frame as the session list', () => {
    const screen = source.slice(source.indexOf('  return ('));
    expect(screen).toContain('max-w-[1400px]');
    expect(screen).not.toContain('max-w-3xl');
  });
});

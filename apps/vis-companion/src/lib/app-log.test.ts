import { describe, expect, it } from 'vitest';

import { logErrorCount, logText, shownLog } from './app-log';
import type { LiveLogEntry } from './live-view';

const entry = (seq: number, level: LiveLogEntry['level'], detail?: string): LiveLogEntry => ({
  seq,
  at: seq,
  level,
  source: seq === 3 ? 'host' : 'app',
  message: `m${seq}`,
  ...(detail ? { detail } : {}),
});
const LOG = [entry(1, 'info'), entry(2, 'warn'), entry(3, 'error', 'at a\nat b'), entry(4, 'error'), entry(5, 'debug')];

describe('the app log', () => {
  it('counts the error entries for the badge', () => {
    expect(logErrorCount(LOG)).toBe(2);
    expect(logErrorCount(undefined)).toBe(0);
  });

  it('shows every entry or the entries of one level, oldest first', () => {
    expect(shownLog(LOG, 'all').map((one) => one.seq)).toEqual([1, 2, 3, 4, 5]);
    expect(shownLog(LOG, 'error').map((one) => one.seq)).toEqual([3, 4]);
  });

  it('copies one line for each entry and indents the lines of a detail', () => {
    expect(logText(LOG.slice(1, 3), (at) => `t${at}`)).toBe('t2 WARN app m2\nt3 ERROR host m3\n  at a\n  at b');
  });
});

/**
 * The debug log of an app (`live_view.log`): what its chrome counts, filters and
 * copies. The host writes the entries; the Companion only reads them.
 */

import { LIVE_LOG_LEVELS, type LiveLogEntry, type LiveLogLevel } from './live-view';

/** What the log panel shows: every entry, or the entries of one level. */
export type AppLogFilter = 'all' | LiveLogLevel;

/** The filters of the log panel, most serious first after `all`. */
export const APP_LOG_FILTERS: readonly AppLogFilter[] = ['all', ...[...LIVE_LOG_LEVELS].reverse()];

/** The number of `error` entries: the badge on the Log control. */
export function logErrorCount(log: readonly LiveLogEntry[] | undefined): number {
  return (log ?? []).filter((entry) => entry.level === 'error').length;
}

/** The entries that `filter` keeps, oldest first. */
export function shownLog(log: readonly LiveLogEntry[], filter: AppLogFilter): LiveLogEntry[] {
  return filter === 'all' ? log.slice() : log.filter((entry) => entry.level === filter);
}

/** The count of each filter, for the label of its chip. */
export function logFilterCounts(log: readonly LiveLogEntry[]): Record<AppLogFilter, number> {
  const counts: Record<AppLogFilter, number> = { all: log.length, debug: 0, info: 0, warn: 0, error: 0 };
  for (const entry of log) counts[entry.level] += 1;
  return counts;
}

/** The local wall clock of an entry, to the millisecond: `14:03:07.042`. */
export function logClock(at: number): string {
  const time = new Date(at);
  const two = (value: number) => String(value).padStart(2, '0');
  return `${two(time.getHours())}:${two(time.getMinutes())}:${two(time.getSeconds())}.${String(
    time.getMilliseconds(),
  ).padStart(3, '0')}`;
}

/**
 * The entries as plain text, one line each, for Copy all. A detail follows its
 * line, each of its lines indented by two spaces.
 */
export function logText(entries: readonly LiveLogEntry[], clock: (at: number) => string = logClock): string {
  return entries
    .map((entry) => {
      const line = `${clock(entry.at)} ${entry.level.toUpperCase()} ${entry.source} ${entry.message}`;
      if (!entry.detail) return line;
      return `${line}\n${entry.detail
        .split('\n')
        .map((part) => `  ${part}`)
        .join('\n')}`;
    })
    .join('\n');
}

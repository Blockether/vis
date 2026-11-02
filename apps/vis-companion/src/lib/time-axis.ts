/**
 * The time of parallel work on one shared scale: a duration as words, and the round
 * marks of a time axis. The TEAM panel and the live `timeline` node both draw with it.
 */

/** `45s`, `3m 04s`, `1h 02m`. */
export function formatDuration(ms: number): string {
  const seconds = Math.floor(ms / 1000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = Math.floor(seconds / 60);
  if (minutes < 60) return `${minutes}m ${String(seconds % 60).padStart(2, '0')}s`;
  return `${Math.floor(minutes / 60)}h ${String(minutes % 60).padStart(2, '0')}m`;
}

// The steps of the time axis, in seconds. The axis takes the least step that gives at most five marks.
const AXIS_STEPS = [10, 15, 30, 60, 120, 300, 600, 900, 1800, 3600, 7200];

/** The marks of a time axis of `ms`, in ms from 0: at most five, on a round step. */
export function axisMarks(ms: number): number[] {
  const seconds = ms / 1000;
  const step = AXIS_STEPS.find((each) => seconds / each < 5) ?? Math.ceil(seconds / 4 / 3600) * 3600;
  return Array.from({ length: Math.floor(seconds / step) + 1 }, (_, index) => index * step * 1000);
}

/** The label of one axis mark: `m:ss` from the start of the axis. */
export const axisLabel = (ms: number) =>
  `${Math.floor(ms / 60_000)}:${String(Math.floor(ms / 1000) % 60).padStart(2, '0')}`;

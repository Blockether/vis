/**
 * IMPORT ONE SESSION FROM A SNAPSHOT FILE.
 *
 * A snapshot is the `.vissnap` file of an export. The gateway makes a new session from it, with
 * its turns and its child sessions. The TUI and `vis-agent sessions import` do the same.
 */

/**
 * The files that the picker offers. A phone does not know the `.vissnap` extension, so the
 * general binary type keeps each file selectable there.
 */
export const SNAPSHOT_ACCEPT = '.vissnap,application/octet-stream';

/** Where an import goes: a project folder, and optionally one session group of it. */
export type ImportTarget = { root: string; groupId?: string };

/** The words of a failed import, for a toast. The gateway says why it refused a file. */
export function importFailure(cause: unknown): string {
  const reason = cause instanceof Error && cause.message ? cause.message : String(cause);
  return `The import failed: ${reason}`;
}

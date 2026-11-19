/**
 * IMPORT ONE SESSION FROM A FILE.
 *
 * The gateway imports a snapshot (`.vissnap`) or an ATIF trajectory (`.json`) as a new
 * session. The TUI and `vis-agent sessions import` do the same.
 */

/**
 * The files that the picker offers. A phone does not know the `.vissnap` extension, so the
 * general binary type keeps each file selectable there.
 */
export const SESSION_IMPORT_ACCEPT = '.vissnap,.json,application/octet-stream,application/json';

/** Where an import goes: a project folder, and optionally one session group of it. */
export type ImportTarget = { root: string; groupId?: string };

/** The words of a failed import, for a toast. The gateway says why it refused a file. */
export function importFailure(cause: unknown): string {
  const reason = cause instanceof Error && cause.message ? cause.message : String(cause);
  return `The import failed: ${reason}`;
}

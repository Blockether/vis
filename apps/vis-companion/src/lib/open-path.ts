import { createContext, useContext } from 'react';

/**
 * WHAT A PRESSED PATH DOES.
 *
 * A path in a transcript names a file on the machine that RAN the step, not on the
 * one reading it: the phone holding this session has no such tree. So the press
 * shows the file HERE first — the gateway reads the lines and the app stands them
 * at the line the press named — and the editor on the machine that holds the file
 * is one press further, in that same sheet.
 *
 * `line` is the place in the file worth standing at: a patch header points at its
 * first hunk. Without one the window starts at the top.
 *
 * The screen publishes the opener the way it publishes the workspace roots, because
 * a file row is drawn deep inside a memoized transcript while the session — and the
 * client that can reach its gateway — is known only at the top.
 *
 * Nothing published means nothing to press: a screen that was never given a session —
 * a story, a preview — leaves its paths plain words, and a row that presses for some
 * other reason keeps that press.
 */
export const OpenPathContext = createContext<((path: string, line?: number) => void) | null>(
  null,
);

export function useOpenPath(): ((path: string, line?: number) => void) | null {
  return useContext(OpenPathContext);
}

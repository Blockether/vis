import { createContext, useContext } from 'react';

/**
 * WHERE A PRESSED PATH OPENS.
 *
 * A path in a transcript names a file on the machine that RAN the step, not on the
 * one reading it: the phone holding this session has no such tree, and the desktop
 * beside it has the editor the file belongs in. So a press travels back to the
 * gateway and the file opens THERE, which is the same thing the TUI does for a path
 * in its own chronology.
 *
 * The screen publishes the opener the way it publishes the workspace roots, because
 * a file row is drawn deep inside a memoized transcript while the session — and the
 * client that can reach its gateway — is known only at the top.
 *
 * Nothing published means nothing to open: a screen that was never given a session —
 * a story, a preview — leaves its paths plain words, and a row that presses for some
 * other reason keeps that press.
 */
export const OpenPathContext = createContext<((path: string) => void) | null>(null);

export function useOpenPath(): ((path: string) => void) | null {
  return useContext(OpenPathContext);
}

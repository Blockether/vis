import { createContext, useContext } from 'react';

/**
 * THE ROOTS A PATH IS SHOWN AGAINST.
 *
 * A file row is drawn deep inside a memoized transcript — turn → block → activity
 * axis → path — while the only thing that knows which workspace the session
 * belongs to is the screen at the top. Threading the roots down as a prop would
 * put every historical row on the session object and re-render the lot whenever it
 * moved, so the screen PUBLISHES them, exactly as it publishes the image intake,
 * and whatever draws a path reads them.
 *
 * Empty means "nothing to shorten against": the path falls back to its
 * home-relative form, which is what a machine with no open workspace shows anyway.
 */
export const WorkspaceRootsContext = createContext<
  readonly (string | null | undefined)[]
>([]);

export function useWorkspaceRoots(): readonly (string | null | undefined)[] {
  return useContext(WorkspaceRootsContext);
}

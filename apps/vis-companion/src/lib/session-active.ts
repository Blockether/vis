import { createContext, useContext } from 'react';

/**
 * False for a session screen that stands in the BACKGROUND: on a phone, the parent of an open
 * subagent session waits under it, so the back swipe uncovers it as it is. Such a screen shows
 * what it holds and takes nothing: no keys, no back layer and no read marks.
 */
export const SessionActive = createContext(true);

/** Whether the calling session screen is the one in front. */
export function useSessionActive(): boolean {
  return useContext(SessionActive);
}

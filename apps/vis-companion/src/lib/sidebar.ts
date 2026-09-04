/**
 * Whether the desk's sidebar is up. A preference, not a screen state: hiding the list
 * to read one transcript wide is a decision that should hold across sessions and
 * reloads, the way ChatGPT's does. `localStorage` because a React state initializer
 * cannot await (same reason as `project-fold.ts`); a browser without it simply starts
 * shown every time.
 */
import { useCallback, useState } from 'react';

const STORE_KEY = 'vis.sidebar';

/** What the reader last decided; shown when they never said. */
export function readSidebarShown(): boolean {
  try {
    return globalThis.localStorage?.getItem(STORE_KEY) !== 'hidden';
  } catch {
    return true;
  }
}

export function writeSidebarShown(isShown: boolean): void {
  try {
    globalThis.localStorage?.setItem(STORE_KEY, isShown ? 'shown' : 'hidden');
  } catch {
    // Private mode / quota: the choice then lasts as long as the page does.
  }
}

/** The sidebar's state and the one verb that flips it. */
export function useSidebar(): [boolean, () => void] {
  const [isShown, setShown] = useState(readSidebarShown);
  const toggle = useCallback(() => {
    setShown((was) => {
      writeSidebarShown(!was);
      return !was;
    });
  }, []);
  return [isShown, toggle];
}

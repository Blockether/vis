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

/**
 * THE DESK'S RAIL, UP OR PUT AWAY.
 *
 * Putting the list away slides it under the pane that takes its place: the box KEEPS
 * its width — the rows never rewrap, and the reader's scroll, scope and folds survive
 * the round trip — and only the margin pulls it off the seam. That margin mirrors
 * the one fixed width exactly, so the rail lands flush off the edge and the pane
 * behind it takes the whole shell. Letting the width go instead sized the rail by its
 * own rows: an invisible column stayed standing in the row, and the pane beside it was
 * stretched past the shell's edge (user report: the list goes out wrong and then is
 * not there at all). `visibility` rides the same transition, so the list leaves the tab
 * order only once the ride is over, and the seam's rule fades with it — the rule is
 * carried in BOTH states, because a border that stops existing cannot fade. A
 * motion-reduce reader gets the instant move instead of the ride.
 *
 * The ride is 200ms on `ease-in-out`, the pace the rest of the app moves a panel at.
 * The 300ms ease-out it used to carry put half its distance into the first fifth of
 * that time and then crawled through the rest, which reads as a rail that sticks
 * rather than one that travels. Every frame of the ride also moves the seam, so the
 * transcript beside it is laid out again on each one: `ChatContent`'s paint skip and
 * `JustifiedProse` both refuse to learn from a width that is still moving, and wait
 * for the rail to land before they measure anything.
 *
 * The rail has ONE width, 26rem, on every desk window: it does not grow with the window
 * and the person cannot drag it narrower or wider (user request).
 */
export function sidebarRailClass(isUp: boolean): string {
  const box =
    'relative h-full w-[26rem] min-w-[26rem] max-w-[26rem] shrink-0 border-r transition-[margin-left,visibility,border-color] duration-200 ease-in-out motion-reduce:transition-none';
  return isUp
    ? `${box} border-dialog-edge`
    : `${box} -ml-[26rem] invisible border-transparent`;
}

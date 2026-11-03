/**
 * The open state of the app log in one app surface, and the Escape key that
 * closes it before it closes the app.
 */

import { useEffect, useState, type RefObject } from 'react';

import type { LiveLogEntry } from '../lib/live-view';

/**
 * The log of `view` is open when the reader asked for it and it has entries. The
 * capture phase sees Escape before the dialog and the frame do; a key outside
 * this app is not ours. A key from inside the log gives the focus back to the Log
 * control, because the focused control goes away with the log.
 */
export function useAppLog(
  log: readonly LiveLogEntry[] | undefined,
  surface: RefObject<HTMLElement | null>,
  isFramed: boolean,
): { isOpen: boolean; toggle: () => void } {
  const [isWanted, setWanted] = useState(false);
  const isOpen = isWanted && (log?.length ?? 0) > 0;
  useEffect(() => {
    if (!isOpen) return;
    function onKey(event: KeyboardEvent) {
      if (event.key !== 'Escape' || event.defaultPrevented) return;
      const frame = surface.current?.closest(isFramed ? '[data-app-frame]' : '[role="dialog"]') ?? null;
      const target = event.target;
      if (target !== document.body && !(target instanceof Node && frame?.contains(target))) return;
      event.preventDefault();
      event.stopPropagation();
      if (target !== document.body) {
        (frame ?? surface.current)?.querySelector<HTMLElement>('[data-app-log-button]')?.focus();
      }
      setWanted(false);
    }
    window.addEventListener('keydown', onKey, true);
    return () => window.removeEventListener('keydown', onKey, true);
  }, [isOpen, isFramed, surface]);
  return { isOpen, toggle: () => setWanted(!isOpen) };
}

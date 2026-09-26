/** Device-local transcript presentation, never an engine execution setting. */
import { useSyncExternalStore } from 'react';

const CHANGE = 'vis:transcript-display';
let storageWritable = true;

function subscribe(changed: () => void): () => void {
  globalThis.addEventListener?.(CHANGE, changed);
  globalThis.addEventListener?.('storage', changed);
  return () => {
    globalThis.removeEventListener?.(CHANGE, changed);
    globalThis.removeEventListener?.('storage', changed);
  };
}

/**
 * One switch, kept on this device. Only the word that differs from `initial` is read back,
 * so a missing or unknown stored value keeps the switch where it starts.
 */
function displaySwitch(key: string, words: { on: string; off: string }, initial: boolean) {
  let fallback = initial;
  const read = (): boolean => {
    try {
      if (!storageWritable || !globalThis.localStorage) return fallback;
      const stored = localStorage.getItem(key);
      return initial ? stored !== words.off : stored === words.on;
    } catch {
      return fallback;
    }
  };
  const write = (on: boolean): void => {
    fallback = on;
    try {
      globalThis.localStorage?.setItem(key, on ? words.on : words.off);
    } catch {
      // Restricted storage: keep the choice for this page's lifetime.
      storageWritable = false;
    }
    globalThis.dispatchEvent?.(new Event(CHANGE));
  };
  const useSwitch = (): boolean => useSyncExternalStore(subscribe, read, () => initial);
  return { read, write, use: useSwitch };
}

const pythonCode = displaySwitch('vis.show_python_code', { on: 'shown', off: 'hidden' }, true);

export const readPythonCodeShown = pythonCode.read;
export const setPythonCodeShown = pythonCode.write;
export const usePythonCodeShown = pythonCode.use;

/** A finished turn folds to its one-line digest unless the reader chose every step. */
const finishedTurns = displaySwitch(
  'vis.expand_finished_turns',
  { on: 'expanded', off: 'folded' },
  false,
);

export const readFinishedTurnsExpanded = finishedTurns.read;
export const setFinishedTurnsExpanded = finishedTurns.write;
export const useFinishedTurnsExpanded = finishedTurns.use;

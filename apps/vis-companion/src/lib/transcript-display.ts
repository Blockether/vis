/** Device-local transcript presentation, never an engine execution setting. */
import { useSyncExternalStore } from "react";

const KEY = "vis.show_python_code";
const CHANGE = "vis:transcript-display";
let fallback = true;
let storageWritable = true;

export function readPythonCodeShown(): boolean {
  try {
    return storageWritable && globalThis.localStorage
      ? localStorage.getItem(KEY) !== "hidden"
      : fallback;
  } catch {
    return fallback;
  }
}

export function setPythonCodeShown(shown: boolean): void {
  fallback = shown;
  try {
    globalThis.localStorage?.setItem(KEY, shown ? "shown" : "hidden");
  } catch {
    // Restricted storage: keep the choice for this page's lifetime.
    storageWritable = false;
  }
  globalThis.dispatchEvent?.(new Event(CHANGE));
}

function subscribe(changed: () => void): () => void {
  globalThis.addEventListener?.(CHANGE, changed);
  globalThis.addEventListener?.("storage", changed);
  return () => {
    globalThis.removeEventListener?.(CHANGE, changed);
    globalThis.removeEventListener?.("storage", changed);
  };
}

export function usePythonCodeShown(): boolean {
  return useSyncExternalStore(subscribe, readPythonCodeShown, () => true);
}

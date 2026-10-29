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

/** Steps between two progress notes share one Activity unless the reader chose every step. */
const stepsSummarized = displaySwitch(
  'vis.summarize_steps',
  { on: 'summarized', off: 'separate' },
  true,
);

export const readStepsSummarized = stepsSummarized.read;
export const setStepsSummarized = stepsSummarized.write;
export const useStepsSummarized = stepsSummarized.use;

/**
 * Zen mode shows only the transcript and the composer, in a centred golden ratio column.
 * The TUI keeps the same switch in its own settings (`zen_mode`).
 */
const zenMode = displaySwitch('vis.zen_mode', { on: 'zen', off: 'normal' }, false);

export const readZenMode = zenMode.read;
export const setZenMode = zenMode.write;
export const useZenMode = zenMode.use;

export type ZenSide = 'left' | 'right';

const ZEN_PICTURE_KEY: Record<ZenSide, string> = {
  left: 'vis.zen_left_image',
  right: 'vis.zen_right_image',
};

/** Pictures kept for this page only, after storage refused them. */
const zenPictureFallback: Record<ZenSide, string | null> = { left: null, right: null };

function readZenPicture(side: ZenSide): string | null {
  try {
    if (!storageWritable || !globalThis.localStorage) return zenPictureFallback[side];
    return localStorage.getItem(ZEN_PICTURE_KEY[side]) ?? zenPictureFallback[side];
  } catch {
    return zenPictureFallback[side];
  }
}

/**
 * Keep `picture` (a data URL) as the Zen picture of `side`, or remove it with null.
 * Returns false when this device could not keep it after a reload.
 */
export function setZenPicture(side: ZenSide, picture: string | null): boolean {
  zenPictureFallback[side] = picture;
  let kept = true;
  try {
    if (picture) globalThis.localStorage?.setItem(ZEN_PICTURE_KEY[side], picture);
    else globalThis.localStorage?.removeItem(ZEN_PICTURE_KEY[side]);
  } catch {
    // A full or restricted storage keeps the picture for this page only.
    kept = false;
  }
  globalThis.dispatchEvent?.(new Event(CHANGE));
  return kept;
}

export function useZenPicture(side: ZenSide): string | null {
  return useSyncExternalStore(
    subscribe,
    () => readZenPicture(side),
    () => null,
  );
}

/** The longest edge of a kept Zen picture, in pixels: sharp in a margin on a 2x screen. */
const ZEN_PICTURE_EDGE = 1600;

/** `file` as a data URL that fits `ZEN_PICTURE_EDGE`, so device storage can keep it. */
export async function zenPictureFromFile(file: Blob): Promise<string> {
  const bitmap = await createImageBitmap(file);
  try {
    const scale = Math.min(1, ZEN_PICTURE_EDGE / Math.max(bitmap.width, bitmap.height));
    const canvas = document.createElement('canvas');
    canvas.width = Math.max(1, Math.round(bitmap.width * scale));
    canvas.height = Math.max(1, Math.round(bitmap.height * scale));
    const context = canvas.getContext('2d');
    if (!context) throw new Error('This browser cannot draw the picture.');
    context.drawImage(bitmap, 0, 0, canvas.width, canvas.height);
    // WebP keeps transparency at a fraction of PNG's size; a browser without it answers PNG.
    return canvas.toDataURL('image/webp', 0.88);
  } finally {
    bitmap.close();
  }
}

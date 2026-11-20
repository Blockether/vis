/** The width that the person chose for a panel that the person can resize. */
import { useState } from 'react';

/** Width bounds in CSS pixels, and the step of one arrow key. */
export type WidthBounds = { min: number; max: number; step: number };

export function clampWidth(width: number, bounds: WidthBounds): number {
  return Math.round(Math.min(bounds.max, Math.max(bounds.min, width)));
}

function readChosenWidth(key: string, bounds: WidthBounds): number | null {
  try {
    const stored = Number(window.localStorage.getItem(key));
    return Number.isFinite(stored) && stored > 0 ? clampWidth(stored, bounds) : null;
  } catch {
    return null;
  }
}

/**
 * The width that the person chose, kept under `key`, or null before the first choice.
 * Only a chosen width is kept, so a new default still applies to a panel never resized.
 */
export function useChosenWidth(
  key: string,
  bounds: WidthBounds,
): [number | null, (width: number) => void] {
  const [width, setWidth] = useState(() => readChosenWidth(key, bounds));
  const choose = (next: number) => {
    const clamped = clampWidth(next, bounds);
    setWidth(clamped);
    try {
      window.localStorage.setItem(key, String(clamped));
    } catch {
      // Private browsing can refuse storage. The width then lasts for this page only.
    }
  };
  return [width, choose];
}

/**
 * The draggable edge of a panel that the person can resize. The live views panel and
 * the desk's session list share it.
 */

import { type KeyboardEvent as ReactKeyboardEvent, type PointerEvent as ReactPointerEvent } from 'react';
import type { WidthBounds } from '../lib/chosen-width';

export interface ResizeEdgeProps {
  /** The accessible name of the edge. */
  label: string;
  /** Where the edge stands on its panel: `start` for a panel on the right, `end` for one on the left. */
  side: 'start' | 'end';
  bounds: WidthBounds;
  /** The width that the panel has, or null when the screen decides it. */
  width: number | null;
  onWidth: (width: number) => void;
}

/**
 * The edge of a panel. `side` is where the edge stands on its panel: `start` for a
 * panel on the right of the window, `end` for one on the left. Drag it, or use the
 * arrow keys on it, to resize the panel. Without a `width`, a drag starts from the
 * width that the panel has on the screen.
 */
export function ResizeEdge({ label, side, bounds, width, onWidth }: ResizeEdgeProps) {
  const sign = side === 'end' ? 1 : -1;
  const heldWidth = (edge: HTMLElement) =>
    width ?? edge.parentElement?.getBoundingClientRect().width ?? bounds.min;
  const drag = (event: ReactPointerEvent<HTMLDivElement>) => {
    event.preventDefault();
    const startX = event.clientX;
    const startWidth = heldWidth(event.currentTarget);
    const move = (moved: PointerEvent) => onWidth(startWidth + sign * (moved.clientX - startX));
    const stop = () => {
      window.removeEventListener('pointermove', move);
      window.removeEventListener('pointerup', stop);
      window.removeEventListener('pointercancel', stop);
    };
    window.addEventListener('pointermove', move);
    window.addEventListener('pointerup', stop);
    window.addEventListener('pointercancel', stop);
  };
  const resizeByKey = (event: ReactKeyboardEvent<HTMLDivElement>) => {
    const step = event.key === 'ArrowRight' ? sign : event.key === 'ArrowLeft' ? -sign : 0;
    if (step === 0) return;
    event.preventDefault();
    onWidth(heldWidth(event.currentTarget) + step * bounds.step);
  };
  return (
    <div
      role="separator"
      aria-orientation="vertical"
      aria-label={label}
      aria-valuemin={bounds.min}
      aria-valuemax={bounds.max}
      aria-valuenow={width ?? undefined}
      tabIndex={0}
      className={`absolute inset-y-0 ${side === 'end' ? '-right-1.5' : '-left-1.5'} z-10 w-3 cursor-col-resize touch-none focus-visible:bg-accent/40`}
      onPointerDown={drag}
      onKeyDown={resizeByKey}
    />
  );
}

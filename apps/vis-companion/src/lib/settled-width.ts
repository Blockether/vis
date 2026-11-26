/**
 * Hold the reading column at one width while its scroller is resized step by step.
 *
 * Each width step lays out all prose on screen again: hyphenation and justification cost
 * about 4 ms for each step with the CPU 4x slower. A window drag or a panel drag makes a
 * step on each frame. So the column keeps its width while the steps come, and lays out
 * one time when they stop. A single change, such as a panel that opens, lays out at once.
 */

/** Two width changes closer than this are one continuous resize. */
const STEP_GAP_MS = 100;
/** The column takes the new width after the scroller width has held still this long. */
export const SETTLE_MS = 150;

export function holdWidthWhileResizing(column: HTMLElement, scroller: HTMLElement): () => void {
  if (typeof ResizeObserver === 'undefined') return () => {};
  let width: number | null = null;
  let changedAt = Number.NEGATIVE_INFINITY;
  let release: ReturnType<typeof setTimeout> | null = null;

  const settle = () => {
    release = null;
    column.style.width = '';
  };

  const observer = new ResizeObserver(() => {
    const next = scroller.clientWidth;
    const previous = width;
    width = next;
    if (previous === null || Math.abs(next - previous) < 0.5) return;
    const now = performance.now();
    const isStep = now - changedAt < STEP_GAP_MS;
    changedAt = now;
    if (!isStep && release === null) return;
    // The column was laid out at the width of the step before: it keeps that width.
    if (!column.style.width) column.style.width = `${column.getBoundingClientRect().width}px`;
    if (release !== null) clearTimeout(release);
    release = setTimeout(settle, SETTLE_MS);
  });
  observer.observe(scroller);

  return () => {
    observer.disconnect();
    if (release !== null) clearTimeout(release);
    column.style.width = '';
  };
}

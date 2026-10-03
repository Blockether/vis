import { fireEvent, render, within, type RenderOptions } from '@testing-library/react';
import type { ReactElement } from 'react';

/**
 * Opens each closed step digest under `root`, as a reader does before reading the
 * thinking, code and Activity of the steps under a note.
 */
export function openStepDigests(root: HTMLElement = document.body): void {
  for (const digest of within(root).queryAllByRole('button', { name: /^Expand steps/ }))
    fireEvent.click(digest);
}

/**
 * Renders a trace and opens its step digests. A rerender also opens the digests of new
 * steps. Use `render` to test a closed digest.
 */
export function renderOpenSteps(ui: ReactElement, options?: Omit<RenderOptions, 'queries'>) {
  const view = render(ui, options);
  openStepDigests(view.container);
  return {
    ...view,
    rerender: (next: ReactElement) => {
      view.rerender(next);
      openStepDigests(view.container);
    },
  };
}

/** The HTML of a trace with its step digests open, for a test that reads markup. */
export function openStepsMarkup(ui: ReactElement): string {
  return renderOpenSteps(ui).container.innerHTML;
}

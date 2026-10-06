import { render, type RenderOptions } from '@testing-library/react';
import type { ReactElement } from 'react';
import { OpenBands } from './dev/OpenBands';

/** `render` with every settings band open, for a test about band content, not the fold. */
export function renderOpenBands(ui: ReactElement, options?: Omit<RenderOptions, 'wrapper'>) {
  return render(ui, { ...options, wrapper: OpenBands });
}

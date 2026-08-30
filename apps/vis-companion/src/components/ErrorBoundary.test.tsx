// @vitest-environment jsdom
import { cleanup, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { ErrorFallback } from './ErrorBoundary';

afterEach(cleanup);

describe('the render failure surface', () => {
  it('keeps the caught message and the recovery action available', async () => {
    const reload = vi.fn();
    render(<ErrorFallback message="timeline failed" onReload={reload} />);

    expect(screen.getByText('timeline failed')).toBeTruthy();
    await userEvent.click(screen.getByRole('button', { name: 'Reload Vis' }));
    expect(reload).toHaveBeenCalledOnce();
  });
});

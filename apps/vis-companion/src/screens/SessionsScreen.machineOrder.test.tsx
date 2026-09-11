// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from '@testing-library/react';
import { expect, it } from 'vitest';
import { renderSessionsScreen } from './sessions-screen-harness';

// Regression: connectivity changes must not move machine tabs.
it('keeps an unavailable machine in its assigned position', async () => {
  const view = renderSessionsScreen({
    machines: [{ label: 'alpha', down: true, heals: true }, { label: 'beta' }, { label: 'gamma' }],
  });
  try {
    const strip = within(await screen.findByLabelText('Machines'));
    await waitFor(() =>
      expect(strip.getByRole('button', { name: /alpha/ }).getAttribute('title')).toContain(
        'network',
      ),
    );
    expect(
      strip
        .getAllByRole('button')
        .map((button) => button.getAttribute('aria-label') ?? button.textContent),
    ).toEqual(['Reconnect to alpha', 'beta', 'gamma']);
    fireEvent.click(strip.getByRole('button', { name: 'Reconnect to alpha' }));
    await waitFor(() =>
      expect(
        strip
          .getAllByRole('button')
          .map((button) => button.getAttribute('aria-label') ?? button.textContent),
      ).toEqual(['alpha', 'beta', 'gamma']),
    );
  } finally {
    view.restore();
  }
});

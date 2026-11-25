// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';

import { renderSessionScreen, sessionFixture } from './session-screen-harness';

describe('branch picker', () => {
  it('switches the workspace to the picked branch and shows it in the chip', async () => {
    const switchBranch = vi.fn(async (_sid: string, branch: string) => ({
      branch,
      branches: ['main', 'feature'],
    }));
    renderSessionScreen({
      session: sessionFixture({ workspace: { root: '/work/vis' } }),
      client: {
        workspaceBranches: async () => ({ branch: 'main', branches: ['main', 'feature'] }),
        switchBranch,
      },
    });

    fireEvent.click(await screen.findByRole('button', { name: 'Branch — main, choose a branch' }));
    fireEvent.click(within(screen.getByRole('dialog', { name: 'Branch' })).getByText('feature'));

    await waitFor(() => expect(switchBranch).toHaveBeenCalledWith('s1', 'feature'));
    expect(await screen.findByRole('button', { name: 'Branch — feature, choose a branch' })).toBeVisible();
  });

  it('shows the gateway refusal in the composer', async () => {
    renderSessionScreen({
      session: sessionFixture({ workspace: { root: '/work/vis' } }),
      client: {
        workspaceBranches: async () => ({ branch: 'main', branches: ['main', 'feature'] }),
        switchBranch: async () => {
          throw new Error('Your local changes would be overwritten by checkout');
        },
      },
    });

    fireEvent.click(await screen.findByRole('button', { name: 'Branch — main, choose a branch' }));
    fireEvent.click(within(screen.getByRole('dialog', { name: 'Branch' })).getByText('feature'));

    expect(await screen.findByText(/local changes would be overwritten/)).toBeVisible();
  });
});

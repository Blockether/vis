/** @vitest-environment jsdom */
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { AutomationsPanel } from './AutomationsPanel';
import { storyAutomationsClient } from '../../dev/story-data';

describe('Automations band in machine settings', () => {
  it('opens closed and shows the automations of the machine', async () => {
    render(<AutomationsPanel client={storyAutomationsClient()} />);
    fireEvent.click(await screen.findByRole('button', { name: 'Show automations' }));
    expect(await screen.findByText('Morning summary')).toBeVisible();
    expect(screen.getByRole('button', { name: 'New automation' })).toBeEnabled();
  });

  it('shows the band for a machine with no automations', async () => {
    render(<AutomationsPanel client={storyAutomationsClient({ empty: true })} />);
    expect(await screen.findByRole('button', { name: 'Show automations' })).toBeVisible();
  });

  it('does not invent the band for an unavailable or older machine', async () => {
    const client = storyAutomationsClient();
    const read = vi.spyOn(client, 'automations').mockRejectedValue(new Error('Not found'));
    render(<AutomationsPanel client={client} />);
    await waitFor(() => expect(read).toHaveBeenCalledOnce());
    expect(screen.queryByRole('button', { name: 'Show automations' })).toBeNull();
  });
});

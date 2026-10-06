/** @vitest-environment jsdom */
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { AutomationsPanel } from './AutomationsPanel';
import { storyAutomationsClient } from '../../dev/story-data';

describe('Automations band in machine settings', () => {
  it('opens closed and shows the automations of the machine', async () => {
    render(<AutomationsPanel client={storyAutomationsClient()} />);
    // The closed band hides its list; its chevron used to turn while the list stayed.
    await screen.findByRole('button', { name: 'Show automations' });
    expect(screen.queryByText('Morning summary')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Show automations' }));
    expect(await screen.findByText('Morning summary')).toBeVisible();
    expect(screen.getByRole('button', { name: 'New automation' })).toBeEnabled();
    fireEvent.click(screen.getByRole('button', { name: 'Hide automations' }));
    expect(screen.queryByText('Morning summary')).toBeNull();
  });

  it('shows the band for a machine with no automations', async () => {
    render(<AutomationsPanel client={storyAutomationsClient({ empty: true })} />);
    expect(await screen.findByRole('button', { name: 'Show automations' })).toBeVisible();
  });

  // User report: the band repeated its name in a count line with two text buttons.
  // It now adds an automation with the same header + as the other bands.
  it('starts a new automation from the + of the closed band', async () => {
    render(<AutomationsPanel client={storyAutomationsClient({ empty: true })} />);
    fireEvent.click(await screen.findByRole('button', { name: 'New automation' }));
    expect(screen.getByRole('heading', { name: 'New automation' })).toBeVisible();
    expect(screen.queryByText('0 automations')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Refresh' })).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Close the new automation' }));
    expect(screen.queryByRole('heading', { name: 'New automation' })).toBeNull();
    expect(await screen.findByText('No automations on this machine.')).toBeVisible();
  });

  it('does not invent the band for an unavailable or older machine', async () => {
    const client = storyAutomationsClient();
    const read = vi.spyOn(client, 'automations').mockRejectedValue(new Error('Not found'));
    render(<AutomationsPanel client={client} />);
    await waitFor(() => expect(read).toHaveBeenCalledOnce());
    expect(screen.queryByRole('button', { name: 'Show automations' })).toBeNull();
  });
});

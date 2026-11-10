/** @vitest-environment jsdom */
import { render, screen, waitFor } from '@testing-library/react';
import { describe, expect, it, vi } from 'vitest';
import { AutomationsPanel } from './AutomationsPanel';
import { storyAutomationsClient } from '../../dev/story-data';

describe('Automations band in machine settings', () => {
  it('does not invent the band for an unavailable or older machine', async () => {
    const client = storyAutomationsClient();
    const read = vi.spyOn(client, 'automations').mockRejectedValue(new Error('Not found'));
    render(<AutomationsPanel client={client} />);
    await waitFor(() => expect(read).toHaveBeenCalledOnce());
    expect(screen.queryByRole('button', { name: 'Show automations' })).toBeNull();
  });

  // Regression, user report: on each Settings open the band appeared a round trip
  // after the bands around it and moved them.
  it('paints the band in the first frame when the machine answered before', () => {
    const client = storyAutomationsClient();
    vi.spyOn(client, 'isAutomationsAvailable').mockReturnValue(true);
    vi.spyOn(client, 'automations').mockReturnValue(new Promise(() => {}));
    render(<AutomationsPanel client={client} />);
    expect(screen.getByRole('button', { name: 'Show automations' })).toBeTruthy();
  });
});

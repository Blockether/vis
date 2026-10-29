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
});

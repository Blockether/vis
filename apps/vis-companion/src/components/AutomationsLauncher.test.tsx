/** @vitest-environment jsdom */
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { AutomationsLauncher } from './AutomationsLauncher';
import { GatewayClient } from '../lib/gateway';
import type { GatewayConn } from '../lib/types';
import { STORY_AUTOMATIONS } from '../dev/story-data';

vi.mock('../screens/AutomationsScreen', () => ({
  AutomationsDialog: ({ gateways }: { gateways: GatewayConn[] }) => (
    <div role="dialog">{gateways.map((gateway) => gateway.label).join(', ')}</div>
  ),
}));

const gateways = [{ url: 'http://gateway.example.com', label: 'Workstation' }];
const kept = { automations: STORY_AUTOMATIONS, is_enabled: false };

afterEach(() => vi.restoreAllMocks());

describe('global Automations entry', () => {
  it('appears for a machine that allows automations and opens its list', async () => {
    vi.spyOn(GatewayClient.prototype, 'automations').mockResolvedValue({
      automations: [],
      is_enabled: true,
    });
    render(<AutomationsLauncher gateways={gateways} />);
    fireEvent.click(await screen.findByRole('button', { name: 'Open automations' }));
    expect(await screen.findByRole('dialog')).toHaveTextContent('Workstation');
  });

  it('stays while a machine keeps automations, and goes when the last one is gone', async () => {
    const read = vi.spyOn(GatewayClient.prototype, 'automations').mockResolvedValue(kept);
    const view = render(<AutomationsLauncher gateways={gateways} refreshKey={false} />);
    expect(await screen.findByRole('button', { name: 'Open automations' })).toBeVisible();
    read.mockResolvedValue({ automations: [], is_enabled: false });
    view.rerender(<AutomationsLauncher gateways={gateways} refreshKey />);
    await waitFor(() =>
      expect(screen.queryByRole('button', { name: 'Open automations' })).toBeNull(),
    );
  });

  it('does not invent the entry for an unavailable or older machine', async () => {
    const read = vi
      .spyOn(GatewayClient.prototype, 'automations')
      .mockRejectedValue(new Error('Not found'));
    render(<AutomationsLauncher gateways={gateways} />);
    await waitFor(() => expect(read).toHaveBeenCalledOnce());
    expect(screen.queryByRole('button', { name: 'Open automations' })).toBeNull();
  });

  it('opens the list only for machines that show the entry', async () => {
    vi.spyOn(GatewayClient.prototype, 'automations')
      .mockResolvedValueOnce(kept)
      .mockResolvedValueOnce({ automations: [], is_enabled: false });
    render(
      <AutomationsLauncher
        gateways={[...gateways, { url: 'http://127.0.0.1:9999', label: 'Disabled machine' }]}
      />,
    );
    fireEvent.click(await screen.findByRole('button', { name: 'Open automations' }));
    expect(await screen.findByRole('dialog')).toHaveTextContent('Workstation');
    expect(screen.getByRole('dialog')).not.toHaveTextContent('Disabled machine');
  });
});

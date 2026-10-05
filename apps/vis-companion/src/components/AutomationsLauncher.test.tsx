/** @vitest-environment jsdom */
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { AutomationsLauncher } from './AutomationsLauncher';
import { GatewayClient } from '../lib/gateway';
import type { GatewayConn } from '../lib/types';

vi.mock('../screens/AutomationsScreen', () => ({
  AutomationsDialog: ({ gateways }: { gateways: GatewayConn[] }) => (
    <div role="dialog">{gateways.map((gateway) => gateway.label).join(', ')}</div>
  ),
}));

const gateways = [{ url: 'http://gateway.example.com', label: 'Workstation' }];

afterEach(() => vi.restoreAllMocks());

describe('global Automations entry', () => {
  it('appears for a machine with no automations and opens its list', async () => {
    vi.spyOn(GatewayClient.prototype, 'automations').mockResolvedValue({
      automations: [],
    });
    render(<AutomationsLauncher gateways={gateways} />);
    fireEvent.click(await screen.findByRole('button', { name: 'Open automations' }));
    expect(await screen.findByRole('dialog')).toHaveTextContent('Workstation');
  });

  it('keeps the entry after refreshing an empty list', async () => {
    const read = vi.spyOn(GatewayClient.prototype, 'automations').mockResolvedValue({
      automations: [],
    });
    const view = render(<AutomationsLauncher gateways={gateways} refreshKey={false} />);
    expect(await screen.findByRole('button', { name: 'Open automations' })).toBeVisible();
    view.rerender(<AutomationsLauncher gateways={gateways} refreshKey />);
    await waitFor(() => expect(read).toHaveBeenCalledTimes(2));
    expect(screen.getByRole('button', { name: 'Open automations' })).toBeVisible();
  });

  it('does not invent the entry for an unavailable or older machine', async () => {
    const read = vi
      .spyOn(GatewayClient.prototype, 'automations')
      .mockRejectedValue(new Error('Not found'));
    render(<AutomationsLauncher gateways={gateways} />);
    await waitFor(() => expect(read).toHaveBeenCalledOnce());
    expect(screen.queryByRole('button', { name: 'Open automations' })).toBeNull();
  });

  it('opens the list for every available machine, including an empty one', async () => {
    vi.spyOn(GatewayClient.prototype, 'automations').mockResolvedValue({ automations: [] });
    render(
      <AutomationsLauncher
        gateways={[...gateways, { url: 'http://127.0.0.1:9999', label: 'Empty machine' }]}
      />,
    );
    fireEvent.click(await screen.findByRole('button', { name: 'Open automations' }));
    expect(await screen.findByRole('dialog')).toHaveTextContent('Workstation, Empty machine');
  });
});

/** @vitest-environment jsdom */
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { ImproveLauncher } from './ImproveLauncher';
import { GatewayClient } from '../lib/gateway';
import type { GatewayConn } from '../lib/types';

vi.mock('../screens/ImproveScreen', () => ({
  ImproveDialog: ({ gateways }: { gateways: GatewayConn[] }) => (
    <div role="dialog">{gateways.map((gateway) => gateway.label).join(', ')}</div>
  ),
}));

const gateways = [{ url: 'http://gateway.example.com', label: 'Workstation' }];
const settings = { mode: 'human' as const, provider: null, model: null, interval_minutes: 60 };

afterEach(() => vi.restoreAllMocks());

describe('global Improve entry', () => {
  it('appears only after enabled settings are confirmed and disappears after disabling', async () => {
    const read = vi.spyOn(GatewayClient.prototype, 'improveSettings').mockResolvedValue(settings);
    const view = render(<ImproveLauncher gateways={gateways} refreshKey={false} />);
    fireEvent.click(await screen.findByRole('button', { name: 'Open Improve' }));
    await screen.findByRole('dialog');
    read.mockResolvedValue({ ...settings, mode: 'off' });
    view.rerender(<ImproveLauncher gateways={gateways} refreshKey />);
    await waitFor(() => expect(screen.queryByRole('button', { name: 'Open Improve' })).toBeNull());
    expect(screen.queryByRole('dialog')).toBeNull();
  });

  it('does not invent a feature for off or unavailable machines', async () => {
    const read = vi
      .spyOn(GatewayClient.prototype, 'improveSettings')
      .mockResolvedValue({ ...settings, mode: 'off' });
    const view = render(<ImproveLauncher gateways={gateways} />);
    await waitFor(() => expect(read).toHaveBeenCalledOnce());
    expect(screen.queryByRole('button', { name: 'Open Improve' })).toBeNull();
    read.mockRejectedValue(new Error('offline'));
    view.rerender(<ImproveLauncher gateways={gateways} refreshKey />);
    await waitFor(() => expect(read).toHaveBeenCalledTimes(2));
    expect(screen.queryByRole('button', { name: 'Open Improve' })).toBeNull();
  });

  it('opens the register only for machines with Improve enabled', async () => {
    vi.spyOn(GatewayClient.prototype, 'improveSettings')
      .mockResolvedValueOnce(settings)
      .mockResolvedValueOnce({ ...settings, mode: 'off' });
    render(<ImproveLauncher gateways={[
      ...gateways, { url: 'http://127.0.0.1:9999', label: 'Disabled machine' },
    ]} />);
    fireEvent.click(await screen.findByRole('button', { name: 'Open Improve' }));
    expect(await screen.findByRole('dialog')).toHaveTextContent('Workstation');
    expect(screen.getByRole('dialog')).not.toHaveTextContent('Disabled machine');
  });
});

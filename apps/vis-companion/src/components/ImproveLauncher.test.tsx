/** @vitest-environment jsdom */
import { render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { ImproveLauncher } from './ImproveLauncher';
import { GatewayClient } from '../lib/gateway';

const gateways = [{ url: 'http://gateway.example.com', label: 'Workstation' }];
const settings = { mode: 'human' as const, provider: null, model: null, interval_minutes: 60 };

afterEach(() => vi.restoreAllMocks());

describe('global Improve entry', () => {
  it('appears only after enabled settings are confirmed and disappears after disabling', async () => {
    const read = vi.spyOn(GatewayClient.prototype, 'improveSettings').mockResolvedValue(settings);
    const view = render(<ImproveLauncher gateways={gateways} refreshKey={false} />);
    await screen.findByRole('button', { name: 'Open Improve' });
    read.mockResolvedValue({ ...settings, mode: 'off' });
    view.rerender(<ImproveLauncher gateways={gateways} refreshKey />);
    await waitFor(() => expect(screen.queryByRole('button', { name: 'Open Improve' })).toBeNull());
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
});

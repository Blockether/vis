// @vitest-environment jsdom
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { SettingsDialog } from './SettingsScreen';
import { GatewayClient, GatewayError, INCOMPATIBLE_STATUS } from '../lib/gateway';
import type { GatewayConn } from '../lib/types';

const URL_A = 'http://127.0.0.1:7890';
const URL_B = 'http://127.0.0.1:7891';
beforeEach(() => {
  vi.spyOn(GatewayClient.prototype, 'cachedSettings').mockReturnValue(null);
  vi.spyOn(GatewayClient.prototype, 'settings').mockResolvedValue({
    revision: 'machine-1',
    groups: [
      {
        id: 'agent',
        title: 'Agent',
        toggles: [{ id: 'plans', label: 'Plans', type: 'boolean', enabled: false }],
      },
    ],
  });
  vi.stubGlobal(
    'fetch',
    vi.fn(async () => Response.json({})),
  );
});
afterEach(() => {
  cleanup();
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
  globalThis.localStorage?.clear();
});
const open = (gateways: GatewayConn[], providerMachineUrl?: string) =>
  render(
    <SettingsDialog
      gateways={gateways}
      providerMachineUrl={providerMachineUrl}
      onAddMachine={async () => {}}
      onClose={() => {}}
    />,
  );

describe('one selected machine editor', () => {
  it('opens a sole machine directly in Basics without loading every resource panel', async () => {
    open([{ url: URL_A, label: 'tower' }]);
    await screen.findByRole('switch', { name: 'Plans: off' });
    expect(screen.getByRole('combobox', { name: 'Settings machine' })).toHaveTextContent('tower');
    expect(screen.queryByRole('heading', { name: 'Providers' })).toBeNull();
    expect(screen.queryByRole('heading', { name: 'MCP servers' })).toBeNull();
  });
  it('switches machines only after the draft is discarded', async () => {
    open([
      { url: URL_A, label: 'tower' },
      { url: URL_B, label: 'laptop' },
    ]);
    fireEvent.click(await screen.findByRole('switch', { name: 'Plans: off' }));
    await userEvent.click(screen.getByRole('combobox', { name: 'Settings machine' }));
    await userEvent.click(screen.getByRole('option', { name: 'laptop' }));
    expect(screen.getByRole('combobox', { name: 'Settings machine' })).toHaveTextContent('tower');
    fireEvent.click(screen.getByRole('button', { name: 'Keep editing' }));
    expect(screen.getByRole('switch', { name: 'Plans: on' })).toBeInTheDocument();
    await userEvent.click(screen.getByRole('combobox', { name: 'Settings machine' }));
    await userEvent.click(screen.getByRole('option', { name: 'laptop' }));
    fireEvent.click(screen.getByRole('button', { name: 'Discard and leave' }));
    await waitFor(() =>
      expect(screen.getByRole('combobox', { name: 'Settings machine' })).toHaveTextContent(
        'laptop',
      ),
    );
    await screen.findByRole('switch', { name: 'Plans: off' });
  });
  it('opens a requested machine directly at Providers', async () => {
    open(
      [
        { url: URL_A, label: 'tower' },
        { url: URL_B, label: 'laptop' },
      ],
      URL_B,
    );
    await screen.findByRole('heading', { name: 'Providers' });
    expect(screen.getByRole('combobox', { name: 'Settings machine' })).toHaveTextContent('laptop');
    expect(screen.getByRole('button', { name: 'Models and responses' })).toHaveAttribute(
      'aria-pressed',
      'true',
    );
  });
  it('reports protocol incompatibility without calling the machine unreachable', async () => {
    vi.mocked(GatewayClient.prototype.settings).mockRejectedValue(
      new GatewayError(INCOMPATIBLE_STATUS, 'Update this client'),
    );
    open([{ url: URL_A, label: 'tower' }]);
    await screen.findByText('Update this client');
    expect(screen.queryByText('Machine unreachable')).toBeNull();
    expect(screen.queryByRole('heading', { name: 'MCP servers' })).toBeNull();
  });
  it('allows device settings without fetching resource panels from an offline machine', async () => {
    vi.mocked(GatewayClient.prototype.settings).mockRejectedValue(
      new Error('Connection unavailable'),
    );
    open([{ url: URL_A, label: 'tower' }]);
    await screen.findByText('Connection unavailable');
    await userEvent.click(screen.getByRole('button', { name: 'This device' }));
    expect(screen.getByRole('heading', { name: 'Theme' })).toBeInTheDocument();
    expect(screen.queryByRole('heading', { name: 'Providers' })).toBeNull();
  });
});

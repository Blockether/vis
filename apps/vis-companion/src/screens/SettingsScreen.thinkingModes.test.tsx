// @vitest-environment jsdom
// Simplified thinking modes decide how the reasoning chip works, so the switch sits
// in the Application column with the other ways that this app shows a session.
import { cleanup, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { GatewayClient } from '../lib/gateway';
import type { GatewayConn, Toggle } from '../lib/types';
import { SettingsDialog } from './SettingsScreen';

const MACHINE: GatewayConn = {
  url: 'http://10.0.0.5:7890',
  token: 't',
  label: 'tower',
};

const thinkingModes = (enabled: boolean): Toggle => ({
  id: 'simplified_thinking_modes',
  label: 'Simplified thinking modes',
  type: 'boolean',
  description: 'Step through quick, balanced and deep.',
  enabled,
});

let previousFetch: typeof fetch;

beforeEach(() => {
  previousFetch = globalThis.fetch;
  globalThis.fetch = vi.fn(() =>
    Promise.resolve(
      new Response('{}', {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      }),
    ),
  ) as unknown as typeof fetch;
});

afterEach(() => {
  cleanup();
  globalThis.fetch = previousFetch;
  globalThis.localStorage?.clear();
  vi.restoreAllMocks();
});

async function openApplication() {
  render(
    <SettingsDialog
      gateways={[MACHINE]}
      primaryUrl={MACHINE.url}
      onAddMachine={async () => {}}
      onClose={() => {}}
    />,
  );
  await userEvent.click(screen.getByRole('button', { name: 'Show application settings' }));
}

describe('simplified thinking modes in the Application column', () => {
  it("shows the primary machine's value and flips it with one tap", async () => {
    const read = vi
      .spyOn(GatewayClient.prototype, 'setting')
      .mockResolvedValue(thinkingModes(true));
    const write = vi
      .spyOn(GatewayClient.prototype, 'setSetting')
      .mockResolvedValue(thinkingModes(false));

    await openApplication();

    const on = await screen.findByRole('switch', {
      name: 'Simplified thinking modes: on',
    });
    expect(read).toHaveBeenCalledWith('simplified_thinking_modes', expect.any(AbortSignal));
    expect(screen.getByText('Step through quick, balanced and deep.')).toBeVisible();

    await userEvent.click(on);

    expect(write).toHaveBeenCalledWith('simplified_thinking_modes', 'toggle');
    expect(
      await screen.findByRole('switch', {
        name: 'Simplified thinking modes: off',
      }),
    ).toBeVisible();
  });

  it('hides the switch when the machine cannot answer', async () => {
    const read = vi
      .spyOn(GatewayClient.prototype, 'setting')
      .mockRejectedValue(new Error('offline'));

    await openApplication();

    await waitFor(() => expect(read).toHaveBeenCalled());
    expect(screen.getByRole('switch', { name: 'Compact mode: on' })).toBeVisible();
    expect(screen.queryByRole('switch', { name: /Simplified thinking modes/ })).toBeNull();
  });
});

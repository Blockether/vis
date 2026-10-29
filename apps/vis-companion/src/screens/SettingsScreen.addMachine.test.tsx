// @vitest-environment jsdom
// Regression, user report (a screenshot over the settings dialog): the machines
// band's ＋ opened `Add a machine` as its own modal ON TOP of Settings — a dialog
// inside a dialog, with two close marks, two Escape targets and the fleet greyed
// out behind the form that joins it. Pairing is a band in the machines column.
import { cleanup, screen } from '@testing-library/react';
import { renderOpenBands } from '../test-settings';
import userEvent from '@testing-library/user-event';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { SettingsDialog } from './SettingsScreen';
import type { GatewayConn } from '../lib/types';

const MACHINE: GatewayConn = { url: 'http://10.0.0.5:7890', token: 't', label: 'tower' };

/** A machine that answers every read with an empty body, so the bands can paint. */
const quiet = () =>
  Promise.resolve(
    new Response(JSON.stringify({}), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    }),
  );

let previousFetch: typeof fetch;

beforeEach(() => {
  previousFetch = globalThis.fetch;
  globalThis.fetch = vi.fn(quiet) as unknown as typeof fetch;
});

afterEach(() => {
  cleanup();
  globalThis.fetch = previousFetch;
  globalThis.localStorage?.clear();
  vi.restoreAllMocks();
});

const open = (onClose: () => void = () => {}) =>
  renderOpenBands(<SettingsDialog gateways={[MACHINE]} onAddMachine={async () => {}} onClose={onClose} />);

/** The one field every way of pairing ends in. */
const field = () => screen.queryByPlaceholderText(/vis:\/\/gateway/);

describe('adding a machine from the settings dialog', () => {
  it('gives Escape to the form first and to Settings after it', async () => {
    const onClose = vi.fn();
    open(onClose);
    await userEvent.click(screen.getByRole('button', { name: 'Add a machine' }));

    await userEvent.keyboard('{Escape}');
    expect(field()).toBeNull();
    expect(onClose).not.toHaveBeenCalled();

    await userEvent.keyboard('{Escape}');
    expect(onClose).toHaveBeenCalledTimes(1);
  });
});

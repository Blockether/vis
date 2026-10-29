// @vitest-environment jsdom
import { cleanup, render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { GatewayConn } from '../lib/types';
import { MachineRows } from './Machines';

// Regression, user report (the settings design, "including that whole CURRENT"):
// every machine's settings were ONE column body standing under the WHOLE list and
// painted for whichever row had been pressed last. Pressing a machine therefore
// opened nothing — it swapped the settings already on screen for another machine's
// — so the reader pressed a row here and read the consequence far below it, told
// which machine they were changing by a `CURRENT` mark they had never chosen. A
// machine's settings are hidden under that machine's own row now, behind the same
// chevron the rest of this app discloses with.

const tower: GatewayConn = { url: 'http://192.168.0.241:7890', label: 'tower' };
const laptop: GatewayConn = { url: 'http://100.64.0.10:7890', label: 'laptop' };

afterEach(cleanup);

/** One machine's row, as the control it is. */
function rowOf(machine: string): HTMLElement {
  const row = screen.getByText(machine).closest('button');
  if (!row) throw new Error(`no row for ${machine}`);
  return row;
}

const settingsOf = (machine: GatewayConn) => <p>settings of {machine.label}</p>;

describe('a machine hides its own settings under its own row', () => {
  it('always shows a sole machine without a disclosure or a clickable header', async () => {
    const onPick = vi.fn();
    const view = render(
      <MachineRows
        conns={[tower]}
        health={{ [tower.url]: { state: 'online', at: Date.now() } }}
        onPick={onPick}
        renderPanel={settingsOf}
      />,
    );

    expect(screen.getByText('settings of tower')).toBeVisible();
    const name = screen.getByText('tower');
    expect(name.closest('button')).toBeNull();
    expect(view.container.querySelector('[aria-expanded]')).toBeNull();
    expect(view.container.querySelector('.lucide-chevron-right')).toBeNull();
    await userEvent.click(name);
    expect(onPick).not.toHaveBeenCalled();
    expect(screen.getByText('settings of tower')).toBeVisible();
  });

  it('uses disclosures only while there is more than one machine', () => {
    const props = {
      health: {
        [tower.url]: { state: 'online' as const, at: Date.now() },
        [laptop.url]: { state: 'online' as const, at: Date.now() },
      },
      openUrls: new Set<string>(),
      onPick: vi.fn(),
      renderPanel: settingsOf,
    };
    const view = render(<MachineRows {...props} conns={[tower, laptop]} />);
    expect(screen.queryByText(/^settings of/)).toBeNull();

    view.rerender(<MachineRows {...props} conns={[tower]} />);
    expect(screen.getByText('settings of tower')).toBeVisible();
    expect(screen.getByText('tower').closest('button')).toBeNull();

    view.rerender(<MachineRows {...props} conns={[tower, laptop]} />);
    expect(rowOf('tower')).toHaveAttribute('aria-expanded', 'false');
    expect(rowOf('laptop')).toHaveAttribute('aria-expanded', 'false');
    expect(screen.queryByText(/^settings of/)).toBeNull();

    view.rerender(<MachineRows {...props} conns={[laptop]} />);
    expect(screen.getByText('settings of laptop')).toBeVisible();
    expect(screen.getByText('laptop').closest('button')).toBeNull();
  });

  it.each(['offline', 'auth', 'checking'] as const)(
    'does not disclose settings while %s, even when already open',
    async (state) => {
      const onPick = vi.fn();
      const panel = vi.fn(settingsOf);
      const onRetry = vi.fn();
      render(
        <MachineRows
          conns={[tower]}
          openUrls={new Set([tower.url])}
          health={{ [tower.url]: { state, at: Date.now(), why: 'network error' } }}
          onPick={onPick}
          renderPanel={panel}
          onRetry={onRetry}
        />,
      );

      expect(panel).not.toHaveBeenCalled();
      expect(rowOf('tower')).not.toHaveAttribute('aria-expanded');
      await userEvent.click(rowOf('tower'));
      expect(onPick).not.toHaveBeenCalled();
      expect(onRetry).toHaveBeenCalledTimes(state === 'checking' ? 0 : 1);
      expect(rowOf('tower').querySelector('.lucide-refresh-cw')).toBeInTheDocument();
      expect(rowOf('tower').querySelector('.lucide-chevron-right')).toBeNull();
    },
  );

  it('hides an open panel when its machine stops answering and restores it after recovery', () => {
    const props = {
      conns: [tower],
      openUrls: new Set([tower.url]),
      onPick: vi.fn(),
      renderPanel: settingsOf,
    };
    const view = render(
      <MachineRows {...props} health={{ [tower.url]: { state: 'online', at: Date.now() } }} />,
    );
    expect(screen.getByText('settings of tower')).toBeVisible();
    for (const state of ['offline', 'checking'] as const) {
      view.rerender(<MachineRows {...props} health={{ [tower.url]: { state, at: Date.now() } }} />);
      expect(screen.queryByText('settings of tower')).toBeNull();
      expect(rowOf('tower')).not.toHaveAttribute('aria-controls');
    }
    view.rerender(
      <MachineRows {...props} health={{ [tower.url]: { state: 'online', at: Date.now() } }} />,
    );
    expect(screen.getByText('settings of tower')).toBeVisible();
    expect(screen.getByText('tower').closest('button')).toBeNull();
  });
});

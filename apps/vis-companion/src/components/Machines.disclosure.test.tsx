// @vitest-environment jsdom
import { useState, type ReactNode } from 'react';
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

/** The settings column: every row discloses that machine's own settings, in place. */
function Column({ panel }: { panel: (conn: GatewayConn) => ReactNode }) {
  const [open, setOpen] = useState<ReadonlySet<string>>(new Set());
  return (
    <MachineRows
      conns={[tower, laptop]}
      openUrls={open}
      health={{
        [tower.url]: { state: 'online', at: Date.now() },
        [laptop.url]: { state: 'online', at: Date.now() },
      }}
      onPick={(conn) =>
        setOpen((current) => {
          const next = new Set(current);
          if (!next.delete(conn.url)) next.add(conn.url);
          return next;
        })
      }
      renderPanel={panel}
    />
  );
}

/** One machine's row, as the control it is. */
function rowOf(machine: string): HTMLElement {
  const row = screen.getByText(machine).closest('button');
  if (!row) throw new Error(`no row for ${machine}`);
  return row;
}

/** What that row DISCLOSES, reached the way a screen reader reaches it. */
function panelOf(machine: string): HTMLElement | null {
  const id = rowOf(machine).getAttribute('aria-controls');
  return id ? document.getElementById(id) : null;
}

const settingsOf = (machine: GatewayConn) => <p>settings of {machine.label}</p>;

describe('a machine hides its own settings under its own row', () => {
  it('opens nothing until the row is pressed, and says as much on the row', () => {
    const panel = vi.fn(settingsOf);
    render(<Column panel={panel} />);

    expect(screen.queryByText(/^settings of/)).toBeNull();
    expect(rowOf('tower').getAttribute('aria-expanded')).toBe('false');
    // A machine's panels never mount — and never poll its gateway — unopened.
    expect(panel).not.toHaveBeenCalled();
  });

  it('stands the settings between that machine and the next one', async () => {
    const user = userEvent.setup();
    render(<Column panel={settingsOf} />);

    await user.click(rowOf('tower'));

    expect(rowOf('tower').getAttribute('aria-expanded')).toBe('true');
    const panel = panelOf('tower');
    expect(panel?.textContent).toContain('settings of tower');
    // Under the machine it belongs to, and above the machine it does not.
    const wherever = panel?.compareDocumentPosition(rowOf('laptop')) ?? 0;
    expect(wherever & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(panelOf('laptop')).toBeNull();
  });

  it('never closes one machine to open another', async () => {
    const user = userEvent.setup();
    render(<Column panel={settingsOf} />);

    await user.click(rowOf('tower'));
    await user.click(rowOf('laptop'));

    // The whole report: pressing a machine used to REPLACE the settings on screen
    // with that machine's. Both stand now, each under the row that owns it.
    expect(panelOf('tower')?.textContent).toContain('settings of tower');
    expect(panelOf('laptop')?.textContent).toContain('settings of laptop');
  });

  it('closes the machine it opened, on the same press', async () => {
    const user = userEvent.setup();
    render(<Column panel={settingsOf} />);

    await user.click(rowOf('tower'));
    await user.click(rowOf('tower'));

    expect(rowOf('tower').getAttribute('aria-expanded')).toBe('false');
    expect(screen.queryByText(/^settings of/)).toBeNull();
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
      expect(rowOf('tower').querySelector('.lucide-refresh-cw')).not.toBeNull();
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
    expect(screen.getByText('settings of tower')).toBeTruthy();
    for (const state of ['offline', 'checking'] as const) {
      view.rerender(<MachineRows {...props} health={{ [tower.url]: { state, at: Date.now() } }} />);
      expect(screen.queryByText('settings of tower')).toBeNull();
      expect(rowOf('tower')).not.toHaveAttribute('aria-controls');
    }
    view.rerender(
      <MachineRows {...props} health={{ [tower.url]: { state: 'online', at: Date.now() } }} />,
    );
    expect(screen.getByText('settings of tower')).toBeTruthy();
    expect(rowOf('tower')).toHaveAttribute('aria-expanded', 'true');
  });
});

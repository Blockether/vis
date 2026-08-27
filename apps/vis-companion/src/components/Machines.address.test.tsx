// @vitest-environment jsdom
import { cleanup, render, screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it, vi } from 'vitest';

// Regression, user report ("why are the addresses not in the slide of the machine
// line — it should be a simple dropdown after I click bind a different address, I
// don't need a separate place for it"): the addresses of a machine were a SECOND
// LIST. An `Address` panel stood under the machine rows, it existed only for the
// machine the settings column happened to be READING, it vanished entirely when a
// machine had fewer than two addresses, and each address was a sliding row of its
// own carrying `Use` / `Pin` / `Auto`. So "which machine" and "which route to it"
// were two lists asking one question.

// Nothing here is about the network — except the one address that answers nothing,
// which is what the menu has to be able to SAY.
vi.mock('../lib/gateway', async (importOriginal) => ({
  ...(await importOriginal<typeof import('../lib/gateway')>()),
  GatewayClient: class {
    url: string;
    constructor(conn: { url: string }) {
      this.url = conn.url;
    }
    ping() {
      return Promise.resolve(
        !this.url.includes('127.0.0.1') && !this.url.includes('100.64.0.11'),
      );
    }
  },
}));

import settingsSource from '../screens/SettingsScreen.tsx?raw';
import { MachineRows } from './Machines';
import type { GatewayConn } from '../lib/types';

const TAILSCALE = 'http://100.64.0.10:7890';
const LAN = 'http://192.168.0.5:7890';
const LOOPBACK = 'http://127.0.0.1:7890';
const DEAD_TAILSCALE = 'http://100.64.0.11:7890';

/** A machine this device can reach three ways, currently bound to the LAN one. */
const tower: GatewayConn = {
  url: LAN,
  token: 't',
  label: 'tower',
  alts: [TAILSCALE, LOOPBACK],
};

/** A machine with one address and no pin: nothing to choose between. */
const nas: GatewayConn = { url: 'http://10.0.0.9:7890', token: 't', label: 'nas' };

// An open menu is a PORTAL on `document.body`: unmount it through React, never by
// wiping the body out from under it.
afterEach(cleanup);

/** The settings column's list, with the binding verb wired. */
function fleet(conns: GatewayConn[] = [tower, nas], primaryUrl?: string) {
  const bound: Array<[string, string, boolean]> = [];
  render(
    <MachineRows
      conns={conns}
      selectedUrl={conns[0]?.url}
      primaryUrl={primaryUrl}
      health={{}}
      onPick={() => {}}
      onSelectAddress={(conn, url, pinned) => {
        bound.push([conn.url, url, pinned]);
      }}
    />,
  );
  return bound;
}

/** Open one machine's addresses from its own row, and read the menu back. */
async function openAddresses(machine: string) {
  const user = userEvent.setup();
  await user.click(
    screen.getByRole('button', { name: `Bind ${machine} to a different address` }),
  );
  return { menu: within(await screen.findByRole('menu')), user };
}

/** What one menu row offers: its title, its consequence, and any badge. */
const rowsOf = (menu: ReturnType<typeof within>) =>
  menu.getAllByRole('menuitem').map((item: HTMLElement) => (item.textContent ?? '').trim());

describe('binding a machine to one of its addresses', () => {
  it('offers the address as the row\'s own verb, and only where there is a choice', () => {
    fleet();

    // Regression, user report ("what for I need this bottom row? for changing address
    // we should have the address icon after swipe"): every machine row carried a
    // SECOND line under it repeating the address the row's own name already said,
    // with a chevron on it. The route is a verb in the row's slide now.
    const verb = screen.getByRole('button', { name: 'Bind tower to a different address' });
    expect(verb.textContent).toContain('Address');
    expect(screen.queryByText(LAN)).toBeNull();

    // One address and no pin is simply "the address": no verb at all.
    expect(screen.queryByRole('button', { name: /^Bind nas/ })).toBeNull();
    expect(screen.queryByText(nas.url)).toBeNull();
  });

  it('offers every address this device knows, most durable first', async () => {
    fleet();
    const { menu } = await openAddresses('tower');

    expect(screen.getByRole('menu').getAttribute('aria-label')).toBe('Addresses on tower');
    expect(menu.getByText('Bind tower to\u2026')).toBeTruthy();
    // Each row NAMES the address and says what makes it durable; the one this
    // device is talking to wears the only mark on the list, and every one of them
    // is probed from HERE while the menu is open, so an address that answers
    // nothing says so before it is chosen.
    await waitFor(() =>
      expect(rowsOf(menu)).toEqual([
        '100.64.0.10:7890Works from anywhere your tailnet reaches',
        '192.168.0.5:7890Only while on the same Wi-Fiin use',
        '127.0.0.1:7890Only on the machine running visno answer',
      ]),
    );
  });

  it('binds the machine the line belongs to, and pins what was asked for by name', async () => {
    const bound = fleet();
    const { menu, user } = await openAddresses('tower');

    await user.click(menu.getByRole('menuitem', { name: /100\.64\.0\.10/ }));
    // The row's own machine, the address picked, and the rank that freezes it:
    // asking for a route by name outranks the durability order.
    expect(bound).toEqual([[LAN, TAILSCALE, true]]);
    expect(screen.queryByRole('menu')).toBeNull();
  });

  it('offers Automatic only to a machine that is pinned, and hands the rank back', async () => {
    const unpinned = fleet();
    const { menu, user } = await openAddresses('tower');
    expect(rowsOf(menu).some((row: string) => row.startsWith('Automatic'))).toBe(false);
    await user.keyboard('{Escape}');
    expect(unpinned).toEqual([]);
    cleanup();

    const bound = fleet([{ ...tower, pinned: true }]);
    // A pinned machine says so on its own row, beside `Primary` and `Current`, and
    // the last row of its menu is the way back to letting this device follow the
    // durability order.
    expect(screen.getByText('Pinned')).toBeTruthy();
    const pinned = await openAddresses('tower');
    expect(rowsOf(pinned.menu).at(-1)).toBe(
      'AutomaticFollow the most durable address that answers',
    );

    await pinned.user.click(pinned.menu.getByRole('menuitem', { name: /^Automatic/ }));
    expect(bound).toEqual([[LAN, TAILSCALE, false]]);
  });

  // Regression, user report (paraphrased: an address could not be chosen for a
  // non-primary machine, and Automatic did not take on the first attempt after
  // making it primary): Automatic ranked an address that its own open menu had
  // already proved did not answer, so the saved machine moved onto a dead route.
  it('makes Automatic follow the best address that answers', async () => {
    const pinned: GatewayConn = {
      ...tower,
      pinned: true,
      alts: [DEAD_TAILSCALE],
    };
    const bound = fleet([pinned, nas], nas.url);
    const { menu, user } = await openAddresses('tower');

    await waitFor(() =>
      expect(rowsOf(menu)).toContain(
        '100.64.0.11:7890Works from anywhere your tailnet reachesno answer',
      ),
    );
    await user.click(menu.getByRole('menuitem', { name: /^Automatic/ }));

    expect(bound).toEqual([[LAN, LAN, false]]);
  });

  it('leaves no address panel behind in settings', () => {
    // The panel, its probe and its three swipe verbs are gone — not kept beside the
    // dropdown, which would be the two lists again.
    expect(settingsSource).not.toContain('AddressPanel');
    expect(settingsSource).not.toContain('SwipeActions');
  });
});

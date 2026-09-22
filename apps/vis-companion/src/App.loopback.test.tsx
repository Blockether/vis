// @vitest-environment jsdom
// Regression, Blockether/vis#277: the desktop app running on the SAME Mac as the
// gateway settled on that Mac's LAN address, where the macOS firewall refused
// the native binary — while http://127.0.0.1:7890 answered the whole time. The
// gateway now advertises loopback as well, and the desktop window takes it, but
// only once the gateway answering there proves it is the one already connected.
import { waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import { renderApp } from './app-harness';
import type { GatewayConn } from './lib/types';

const LAN = 'http://192.168.0.150:7890';
const LOOPBACK = 'http://127.0.0.1:7890';

type DesktopWindow = Window & { __TAURI__?: { core: { invoke: ReturnType<typeof vi.fn> } } };

/** The Pake desktop window: its command channel is how the bundle knows it runs on the host. */
function stubDesktopHost() {
  (window as DesktopWindow).__TAURI__ = { core: { invoke: vi.fn(async () => null) } };
}

/** The pairing as this device has it saved, after the app has had its way. */
const saved = (): GatewayConn[] => JSON.parse(localStorage.getItem('vis.connections') ?? '[]');

const asked = (requests: string[], address: string) =>
  requests.filter((href) => href.startsWith(`${address}/`));

afterEach(() => {
  delete (window as DesktopWindow).__TAURI__;
});

describe('the desktop app on the gateway machine', () => {
  it('moves itself onto loopback instead of the host LAN interface', async () => {
    stubDesktopHost();
    const view = renderApp({ machines: [{ label: 'mac', url: LAN, alts: [LOOPBACK] }] });
    try {
      await waitFor(() => {
        // The machine travels whole — same name, same token, both addresses
        // still known — and lands on the address no firewall stands in front of.
        expect(saved()).toEqual([
          expect.objectContaining({
            url: LOOPBACK,
            label: 'mac',
            token: 't',
            alts: [LAN, LOOPBACK],
          }),
        ]);
      });
      expect(localStorage.getItem('vis.activeConnection')).toBe(LOOPBACK);
    } finally {
      view.unmount();
      view.restore();
    }
  });

  it('stays put when 127.0.0.1 is a different gateway on this device', async () => {
    stubDesktopHost();
    // A second vis on this same device answers `/healthz` — which needs no
    // token — so reachability alone would hand the app to the wrong gateway.
    const view = renderApp({
      machines: [{ label: 'mac', url: LAN, alts: [LOOPBACK] }, { label: 'other', url: LOOPBACK }],
    });
    try {
      await waitFor(() => expect(asked(view.requests, LOOPBACK)).not.toEqual([]));
      expect(saved()[0]).toEqual(expect.objectContaining({ url: LAN, label: 'mac' }));
      expect(localStorage.getItem('vis.activeConnection')).toBe(LAN);
    } finally {
      view.unmount();
      view.restore();
    }
  });
});

describe('a phone', () => {
  it('never dials the loopback address a gateway advertises', async () => {
    const view = renderApp({ machines: [{ label: 'mac', url: LAN, alts: [LOOPBACK] }] });
    try {
      // It learns the address — the list is the gateway's — and leaves it alone:
      // on a phone 127.0.0.1 is the phone.
      await waitFor(() => expect(saved()[0]?.alts).toEqual([LAN, LOOPBACK]));
      expect(asked(view.requests, LOOPBACK)).toEqual([]);
      expect(localStorage.getItem('vis.activeConnection')).toBe(LAN);
    } finally {
      view.unmount();
      view.restore();
    }
  });
});

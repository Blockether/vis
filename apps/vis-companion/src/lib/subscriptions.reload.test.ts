// @vitest-environment jsdom
import { describe, expect, it, vi } from 'vitest';

import { SessionSubscriptionHub } from './subscriptions';
import type { GatewayClient } from './gateway';
import type { SseEvent } from './types';

/** A client with the two streams the hub can open and the reload notice. */
function fakeClient() {
  const deliver: { fleet?: (event: SseEvent) => void; session?: (event: SseEvent) => void } = {};
  const extensionsReloaded = vi.fn();
  const client = {
    streamFleetStatus(onEvent: (event: SseEvent) => void) {
      deliver.fleet = onEvent;
      return () => {};
    },
    streamSessionEvents(_cursors: unknown, onEvent: (event: SseEvent) => void) {
      deliver.session = onEvent;
      return () => {};
    },
    extensionsReloaded,
  };
  return { deliver, extensionsReloaded, client: client as unknown as GatewayClient };
}

// #393: a reload in one client (the TUI, Settings, `/reload`) changes the providers for
// every client. The gateway sends `extensions.reloaded` to all of them.
describe('an extensions.reloaded event', () => {
  it('reads the providers again when it arrives on the fleet feed', () => {
    const { deliver, extensionsReloaded, client } = fakeClient();
    const hub = new SessionSubscriptionHub(client);
    const seen: SseEvent[] = [];
    hub.subscribeFleet((event) => seen.push(event));

    deliver.fleet!({ type: 'extensions.reloaded', scope: 'fleet', reload_id: 'r1', seq: 3 });
    deliver.fleet!({ type: 'session.status', scope: 'fleet', session_id: 's1', seq: 4 });

    expect(extensionsReloaded.mock.calls).toEqual([['r1']]);
    expect(seen.map((event) => event.type)).toEqual(['extensions.reloaded', 'session.status']);
    hub.dispose();
  });

  it('reads the providers again when it arrives on a session stream', () => {
    const { deliver, extensionsReloaded, client } = fakeClient();
    const hub = new SessionSubscriptionHub(client);
    hub.watchSessions(['s1']);

    deliver.session!({ type: 'extensions.reloaded', session_id: 's1', reload_id: 'r2', seq: 7 });

    expect(extensionsReloaded.mock.calls).toEqual([['r2']]);
    hub.dispose();
  });
});

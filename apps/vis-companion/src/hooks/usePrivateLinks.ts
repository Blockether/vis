import { useEffect } from 'react';
import { desktopInvoke } from '../lib/desktop';
import type { GatewayClient } from '../lib/gateway';
import { setPrivateLinkRoute } from '../lib/private-links';

/**
 * Keeps the private links of the active gateway current. The app reads the proxy when it
 * connects and each time it shows again, because the configuration of the gateway can change.
 * A failed read keeps the current route. An older gateway has no proxy, so its links open
 * as before.
 */
export function usePrivateLinks(client: GatewayClient | null): void {
  useEffect(() => {
    void setPrivateLinkRoute(null);
    if (!client) return;
    let ctrl = new AbortController();
    const read = () => {
      ctrl.abort();
      const current = (ctrl = new AbortController());
      client.privateLinkRoute(current.signal).then(
        (route) => {
          if (!current.signal.aborted) void setPrivateLinkRoute(route, desktopInvoke());
        },
        () => {},
      );
    };
    const onVisible = () => {
      if (document.visibilityState === 'visible') read();
    };
    read();
    document.addEventListener('visibilitychange', onVisible);
    return () => {
      ctrl.abort();
      document.removeEventListener('visibilitychange', onVisible);
    };
  }, [client]);
}

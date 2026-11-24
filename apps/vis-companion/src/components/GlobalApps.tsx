/**
 * The global apps of the gateway: apps that belong to no session. The shell
 * header shows one icon for each, and a tap opens that app over the whole shell.
 */

import type { GatewayClient } from '../lib/gateway';
import type { LiveView } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import { AppSurface } from './AppAttachment';

interface GlobalAppsProps {
  client: GatewayClient;
  hub: SessionSubscriptionHub | null;
  apps: readonly LiveView[];
  /** The id of the open app, or null. */
  openId: string | null;
  onClose: () => void;
}

/**
 * The one global app picked from the header. The app opens over the
 * whole shell: a dialog on a desk, the whole glass on a phone. Its actions go
 * to the global action route.
 */
export function GlobalApps({ client, hub, apps, openId, onClose }: GlobalAppsProps) {
  // An app that left the list, for example after global apps were turned off, closes.
  const picked = openId === null ? undefined : apps.find((one) => one.id === openId);
  return (
    <SessionEventsContext.Provider value={hub}>
      {picked && (
        <AppSurface key={picked.id} client={client} sid={null} viewId={picked.id} fallbackTitle={picked.title} onClose={onClose} />
      )}
    </SessionEventsContext.Provider>
  );
}

/**
 * The global apps of the gateway: apps that belong to no session. The shell
 * header shows one icon for each; C-x b O opens their list. A pick opens one
 * app over the whole shell.
 */

import type { GatewayClient } from '../lib/gateway';
import { listedGlobalApps } from '../lib/global-apps';
import type { LiveView } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import { AppSurface, AppsListDialog } from './AppAttachment';

interface GlobalAppsProps {
  client: GatewayClient;
  hub: SessionSubscriptionHub | null;
  apps: readonly LiveView[];
  isListOpen: boolean;
  onListClose: () => void;
  /** The id of the open app, or null. */
  openId: string | null;
  onOpen: (view: LiveView) => void;
  onClose: () => void;
}

/**
 * The global apps list and the one app picked from it. The app opens over the
 * whole shell: a dialog on a desk, the whole glass on a phone. Its actions go
 * to the global action route.
 */
export function GlobalApps({ client, hub, apps, isListOpen, onListClose, openId, onOpen, onClose }: GlobalAppsProps) {
  // An app that left the list, for example after global apps were turned off, closes.
  const picked = openId === null ? undefined : apps.find((one) => one.id === openId);
  return (
    <SessionEventsContext.Provider value={hub}>
      {isListOpen && (
        <AppsListDialog
          title="Global apps"
          empty="No program shows a global app."
          within="app"
          apps={listedGlobalApps(apps)}
          onPick={onOpen}
          onClose={onListClose}
        />
      )}
      {picked && (
        <AppSurface key={picked.id} client={client} sid={null} viewId={picked.id} fallbackTitle={picked.title} onClose={onClose} />
      )}
    </SessionEventsContext.Provider>
  );
}

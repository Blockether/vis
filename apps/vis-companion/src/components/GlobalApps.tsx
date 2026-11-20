/**
 * The global apps of the gateway: apps that belong to no session. The shell
 * header opens their list, and a pick opens one app over the whole shell.
 */

import { useCallback, useState } from 'react';

import { canOpenApp } from '../lib/app-view';
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
}

/**
 * The global apps list and the one app picked from it. The app opens over the
 * whole shell: a dialog on a desk, the whole glass on a phone. Its actions go
 * to the global action route.
 */
export function GlobalApps({ client, hub, apps, isListOpen, onListClose }: GlobalAppsProps) {
  const [pickedId, setPickedId] = useState<string | null>(null);
  // An app that left the list, for example after global apps were turned off, closes.
  const picked = pickedId === null ? undefined : apps.find((one) => one.id === pickedId);
  const pick = useCallback(
    (view: LiveView) => {
      if (!canOpenApp(view)) return;
      onListClose();
      setPickedId(view.id);
    },
    [onListClose],
  );
  const close = useCallback(() => setPickedId(null), []);
  return (
    <SessionEventsContext.Provider value={hub}>
      {isListOpen && (
        <AppsListDialog
          title="Global apps"
          empty="No program shows a global app."
          within="app"
          apps={listedGlobalApps(apps)}
          onPick={pick}
          onClose={onListClose}
        />
      )}
      {picked && (
        <AppSurface key={picked.id} client={client} sid={null} viewId={picked.id} fallbackTitle={picked.title} onClose={close} />
      )}
    </SessionEventsContext.Provider>
  );
}

/**
 * The global apps of the gateway: apps that belong to no session. The shell
 * header opens their list, and a pick opens one app over the whole shell.
 */

import { useCallback, useEffect, useRef, useState } from 'react';

import { isAppSurfaceSetting } from '../lib/app-surfaces';
import { newerAppView } from '../lib/app-view';
import { onSettingChange, type GatewayClient } from '../lib/gateway';
import { foldGlobalApps, listedGlobalApps } from '../lib/global-apps';
import type { LiveView } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import { AppSurface, AppsListDialog } from './AppAttachment';

/**
 * The global apps of the gateway, oldest first. Empty until the gateway answers,
 * and empty while the person turns global apps off.
 *
 * The list follows the fleet stream. A lost frame, a new stream and a saved app
 * setting read the list again, because the fleet stream has no replay.
 */
export function useGlobalApps(client: GatewayClient | null, hub: SessionSubscriptionHub | null): LiveView[] {
  const [apps, setAppsState] = useState<LiveView[]>([]);
  const held = useRef<LiveView[]>([]);
  const [generation, setGeneration] = useState(0);
  const setApps = useCallback((next: LiveView[]) => {
    held.current = next;
    setAppsState(next);
  }, []);
  const reread = useCallback(() => setGeneration((one) => one + 1), []);

  useEffect(() => {
    if (!client) return;
    const controller = new AbortController();
    client
      .globalApps(controller.signal)
      .then((views) => {
        if (controller.signal.aborted) return;
        // A slow list never undoes a patch that arrived while it was read.
        const now = held.current;
        setApps(views.map((read) => newerAppView(now.find((one) => one.id === read.id) ?? null, read)));
      })
      .catch((error: unknown) => {
        if (!controller.signal.aborted) console.warn('global apps: list failed', error);
      });
    return () => controller.abort();
  }, [client, generation, setApps]);

  useEffect(() => {
    if (!client) return;
    return onSettingChange(client.base, (updated) => {
      if (isAppSurfaceSetting(updated.id)) reread();
    });
  }, [client, reread]);

  useEffect(() => {
    if (!hub) return;
    const offEvents = hub.subscribeFleet((event) => {
      const folded = foldGlobalApps(held.current, event);
      if (folded.apps !== held.current) setApps(folded.apps);
      if (folded.isReread) reread();
    });
    let wasStreaming: boolean | null = null;
    const offState = hub.subscribeFleetState((streaming) => {
      if (streaming && wasStreaming === false) reread();
      wasStreaming = streaming;
    });
    return () => {
      offEvents();
      offState();
    };
  }, [hub, setApps, reread]);

  return client ? apps : [];
}

/**
 * The global apps list and the one app picked from it. The app opens over the
 * whole shell: a dialog on a desk, the whole glass on a phone. Its actions go
 * to the global action route.
 */
export function GlobalApps({
  client,
  hub,
  apps,
  isListOpen,
  onListClose,
}: {
  client: GatewayClient;
  hub: SessionSubscriptionHub | null;
  apps: readonly LiveView[];
  isListOpen: boolean;
  onListClose: () => void;
}) {
  const [pickedId, setPickedId] = useState<string | null>(null);
  // An app that left the list, for example after global apps were turned off, closes.
  const picked = pickedId === null ? undefined : apps.find((one) => one.id === pickedId);
  const pick = useCallback(
    (view: LiveView) => {
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

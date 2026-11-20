/**
 * The global apps of a gateway: apps that belong to no session. Only an SDK
 * program opens one. Their frames come on the fleet stream, which has no replay,
 * so a new or restored stream reads the list again.
 */

import { useCallback, useEffect, useRef, useState } from 'react';

import { isAppSurfaceSetting } from './app-surfaces';
import { appStateWord, foldAppEvent, newerAppView, type AppFold } from './app-view';
import { onSettingChange, type GatewayClient } from './gateway';
import { appSummary, liveViewFromWire, type LiveView } from './live-view';
import type { SessionSubscriptionHub } from './subscriptions';
import type { SseEvent } from './types';
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT, VIEW_PATCH_EVENT } from './view';

const VIEW_EVENTS: ReadonlySet<string> = new Set([VIEW_OPEN_EVENT, VIEW_PATCH_EVENT, VIEW_CLOSE_EVENT]);

/**
 * A view frame of a global app. Session frames also reach a fleet listener
 * through the shared stream, and they always name their session.
 */
export function isGlobalAppFrame(event: SseEvent): boolean {
  return VIEW_EVENTS.has(event.type) && !event.session_id && !event.sid;
}

/** The fleet stream is ready again. It carries no replay, so the apps must be read again. */
function isFleetReady(event: SseEvent): boolean {
  return event.type === 'subscription.ready' && event.scope === 'fleet';
}

/** What one fleet frame does to the global apps list. */
export interface GlobalAppsFold {
  apps: LiveView[];
  /** A frame was lost or the stream started again: read the list from the gateway. */
  isReread: boolean;
}

/** Fold one fleet frame into the global apps list, oldest first. */
export function foldGlobalApps(apps: LiveView[], event: SseEvent): GlobalAppsFold {
  if (isFleetReady(event)) return { apps, isReread: true };
  if (!isGlobalAppFrame(event)) return { apps, isReread: false };
  if (event.type === VIEW_OPEN_EVENT) {
    const read = liveViewFromWire(event.view);
    if (read?.scope !== 'global') return { apps, isReread: false };
    // The list keeps only the entry of an app. The opened app reads its own body.
    const opened = appSummary(read);
    const at = apps.findIndex((one) => one.id === opened.id);
    if (at < 0) return { apps: [...apps, opened], isReread: false };
    const next = apps.slice();
    next[at] = newerAppView(apps[at], opened);
    return { apps: next, isReread: false };
  }
  let isReread = false;
  let changed = false;
  const next = apps.map((app) => {
    const folded = foldAppEvent(app, event);
    if (folded.isReread) isReread = true;
    if (folded.view !== app) changed = true;
    return folded.view;
  });
  return { apps: changed ? next : apps, isReread };
}

/** Fold one fleet frame into one opened global app. */
export function foldGlobalAppView(view: LiveView, event: SseEvent): AppFold {
  if (isFleetReady(event)) return { view, isReread: true };
  if (!isGlobalAppFrame(event)) return { view, isReread: false };
  return foldAppEvent(view, event);
}

/**
 * The global apps that the list and its header button show: each live app whose
 * program is connected. A closed, archived or disconnected app stays held, so an
 * opened app keeps its picture, but the list leaves it out.
 */
export function listedGlobalApps(apps: readonly LiveView[]): LiveView[] {
  return apps.filter((app) => appStateWord(app) === '');
}

/** The state word of a global app. A stale picture waits for its program. */
export function globalAppStateWord(view: Pick<LiveView, 'is_closed' | 'is_archived' | 'is_disconnected'>): string {
  const word = appStateWord(view);
  return word === 'Disconnected' ? 'Program not connected' : word;
}

/**
 * The global apps of the gateway, oldest first. Empty until the gateway answers,
 * and empty while the person turns global apps off.
 *
 * The list follows the fleet stream. A lost frame, a new stream and a saved app
 * setting read the list again, because the fleet stream has no replay.
 *
 * It is not in `GlobalApps.tsx`: a module that the shell imports keeps only
 * components, so a change below it refreshes in place.
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

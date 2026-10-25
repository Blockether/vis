/**
 * The header views of one machine. They belong to no session: the gateway lists them
 * on `GET /v1/views/live`, and the fleet stream carries their changes.
 */

import { useEffect, useState } from 'react';
import type { GatewayClient } from './gateway';
import type { SessionSubscriptionHub } from './subscriptions';
import {
  applyLiveViewEvent,
  isLiveViewEvent,
  livePlacement,
  type LiveView as LiveViewModel,
} from './live-view';
import { VIEW_OPEN_EVENT } from './view';

const NO_VIEWS: LiveViewModel[] = [];

/** The header views of the machine: read once, then kept current by the fleet stream. */
export function useHeaderViews(
  client: GatewayClient | null,
  subscriptions: SessionSubscriptionHub | null,
): LiveViewModel[] {
  const [views, setViews] = useState<LiveViewModel[]>([]);
  useEffect(() => {
    if (!client || !subscriptions) return;
    let cancelled = false;
    const controller = new AbortController();
    const reload = () => {
      client
        .headerViews(controller.signal)
        .then((open) => {
          if (!cancelled) setViews(open.filter((view) => livePlacement(view) === 'header'));
        })
        .catch(() => undefined);
    };
    reload();
    let wasStreaming: boolean | null = null;
    const stopState = subscriptions.subscribeFleetState((streaming) => {
      // A stream that comes back may have missed frames: read the views again.
      if (streaming && wasStreaming === false) reload();
      wasStreaming = streaming;
    });
    const stopEvents = subscriptions.subscribeFleet((event) => {
      if (!isLiveViewEvent(event)) return;
      if (event.type === VIEW_OPEN_EVENT) {
        const opened = event.view as { placement?: unknown } | null | undefined;
        if (opened?.placement !== 'header') return;
      }
      setViews((current) => applyLiveViewEvent(current, event));
    });
    return () => {
      cancelled = true;
      controller.abort();
      stopState();
      stopEvents();
    };
  }, [client, subscriptions]);
  const isStale = views.some((view) => view.is_stale === true);
  useEffect(() => {
    if (!isStale || !client) return;
    const controller = new AbortController();
    client
      .headerViews(controller.signal)
      .then((open) => setViews(open.filter((view) => livePlacement(view) === 'header')))
      .catch(() => undefined);
    return () => controller.abort();
  }, [client, isStale]);
  return client && subscriptions ? views : NO_VIEWS;
}

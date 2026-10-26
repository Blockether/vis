/**
 * An app attachment: a card in the transcript and the full app surface that it
 * opens. The surface reads the app from the gateway, folds the session events of
 * its view and saves through `draft`, `change` and `submit` actions.
 */

import { memo, useCallback, useContext, useEffect, useMemo, useRef, useState } from 'react';

import { runAppAction, SendKeys, type AppActionResult, type AppActionStatus } from '../lib/app-actions';
import {
  appFallbackTitle,
  appStateWord,
  draftAction,
  foldAppEvent,
  newerAppView,
  sendAction,
  withSubmission,
} from '../lib/app-view';
import type { GatewayClient } from '../lib/gateway';
import type { HumanInputValue } from '../lib/human-input';
import type { LiveView as LiveViewModel } from '../lib/live-view';
import { SessionEventsContext } from '../lib/session-events';
import { useStickyOverlay } from '../lib/sticky-overlay';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { IterationAttachment } from '../lib/types';
import type { ViewAction } from '../lib/view';
import { ActionStatus, type AppNodeActions } from './AppNodes';
import { LiveViewPanel, RunDialog } from './LiveView';
import { BandLabel, ExecutionAction, ListRow } from './ui';

/** Read the app again this long after an acknowledged action that no patch followed. */
export const APP_ECHO_WAIT_MS = 1500;

export interface AppViewState {
  view: LiveViewModel | null;
  /** The first read failed and no picture is held. */
  isMissing: boolean;
  /** Read the app again from the gateway. */
  reread: () => void;
  /** Send one action and fold its answer in. */
  act: (action: ViewAction) => Promise<AppActionResult>;
}

/**
 * The live state of one app. It reads the app once, folds the events of its
 * view, reads it again after a missed patch, a close or a reconnect, and
 * reads it again when an acknowledged action brings no patch.
 */
export function useAppView(
  client: GatewayClient,
  sid: string,
  viewId: string,
  hub: SessionSubscriptionHub | null,
): AppViewState {
  const [view, setView] = useState<LiveViewModel | null>(null);
  const [isMissing, setMissing] = useState(false);
  const viewRef = useRef<LiveViewModel | null>(null);
  const reading = useRef<AbortController | null>(null);
  const echo = useRef<ReturnType<typeof setTimeout> | null>(null);

  const commit = useCallback((next: LiveViewModel | null) => {
    viewRef.current = next;
    setView(next);
  }, []);

  const reread = useCallback(() => {
    reading.current?.abort();
    const controller = new AbortController();
    reading.current = controller;
    client
      .liveView(sid, viewId, controller.signal)
      .then((read) => {
        if (controller.signal.aborted) return;
        if (!read) {
          if (!viewRef.current) setMissing(true);
          return;
        }
        setMissing(false);
        commit(newerAppView(viewRef.current, read));
      })
      .catch((error: unknown) => {
        if (controller.signal.aborted) return;
        console.warn('app attachment: read failed', viewId, error);
        if (!viewRef.current) setMissing(true);
      });
  }, [client, sid, viewId, commit]);

  useEffect(() => {
    reread();
    return () => {
      reading.current?.abort();
      if (echo.current) clearTimeout(echo.current);
    };
  }, [reread]);

  useEffect(() => {
    if (!hub) return;
    const offEvents = hub.subscribeSession(
      sid,
      (event) => {
        const held = viewRef.current;
        if (!held) return;
        const folded = foldAppEvent(held, event);
        if (folded.view !== held) commit(folded.view);
        if (folded.isReread) reread();
      },
      { replay: false },
    );
    let wasConnected: boolean | null = null;
    const offConnection = hub.subscribeConnection((connected) => {
      if (connected && wasConnected === false) reread();
      wasConnected = connected;
    });
    return () => {
      offEvents();
      offConnection();
    };
  }, [hub, sid, commit, reread]);

  const act = useCallback(
    async (action: ViewAction) => {
      const result = await runAppAction(() => client.viewAction(sid, viewId, action));
      const held = viewRef.current;
      if (result.submission && held) commit(withSubmission(held, result.submission));
      if (result.status === 'conflict' || result.status === 'stale' || result.status === 'closed') {
        reread();
      } else if (result.status === 'saved' && result.seq !== undefined) {
        const seq = result.seq;
        if (echo.current) clearTimeout(echo.current);
        if (!hub) reread();
        else
          echo.current = setTimeout(() => {
            if ((viewRef.current?.seq ?? 0) < seq) reread();
          }, APP_ECHO_WAIT_MS);
      }
      return result;
    },
    [client, sid, viewId, hub, commit, reread],
  );

  return { view, isMissing, reread, act };
}

/** The opened app: its title in the dialog band and its nodes in the shared renderer. */
export function AppSurface({
  client,
  sid,
  viewId,
  fallbackTitle,
  onClose,
}: {
  client: GatewayClient;
  sid: string;
  viewId: string;
  fallbackTitle: string;
  onClose: () => void;
}) {
  const hub = useContext(SessionEventsContext);
  const { view, isMissing, act } = useAppView(client, sid, viewId, hub);
  const buttonKeys = useRef(new Map<string, SendKeys>());
  const [formStatus, setFormStatus] = useState<AppActionStatus>('idle');
  const isReadOnly = !view || view.is_closed === true || view.is_disconnected === true;

  const actions = useMemo<AppNodeActions | undefined>(() => {
    if (!view) return undefined;
    return {
      isReadOnly,
      submissions: view.submissions ?? [],
      draft: (nodeId, target) => act(draftAction(view, nodeId, target)),
      send: (nodeId, key) => act(sendAction(view, nodeId, key)),
    };
  }, [view, isReadOnly, act]);

  const onChange = useCallback(
    async (nodeId: string, value: HumanInputValue) => {
      if (!view) return;
      setFormStatus('saving');
      const result = await act({ action: 'change', node_id: nodeId, value, base_seq: view.seq });
      setFormStatus(result.status);
    },
    [view, act],
  );

  const onSubmit = useCallback(
    async (nodeId: string, values: Record<string, HumanInputValue>) => {
      if (!view) return;
      let keys = buttonKeys.current.get(nodeId);
      if (!keys) {
        keys = new SendKeys();
        buttonKeys.current.set(nodeId, keys);
      }
      setFormStatus('saving');
      const result = await act(sendAction(view, nodeId, keys.next(), values));
      keys.settle(result);
      setFormStatus(result.status);
    },
    [view, act],
  );

  const title = view?.title || fallbackTitle;
  const state = view ? appStateWord(view) : '';
  return (
    <RunDialog
      title={title}
      subtitle={
        view ? (
          <span className="flex items-center gap-2">
            <span>{state}</span>
            <ActionStatus status={formStatus} />
          </span>
        ) : undefined
      }
      onClose={onClose}
    >
      <div className="min-h-0 min-w-0 flex-1 overflow-y-auto" data-app-surface={viewId}>
        {view ? (
          <LiveViewPanel
            view={view}
            inDialog
            isSettled={view.is_closed === true}
            app={actions}
            onChange={isReadOnly ? undefined : onChange}
            onSubmit={isReadOnly ? undefined : onSubmit}
            load={(nodeId, from, limit, query) =>
              client.liveViewLog(sid, viewId, nodeId, from, limit, query)
            }
          />
        ) : (
          <p role="status" className="p-4 font-mono text-meta text-dialog-hint">
            {isMissing ? 'This app could not be read.' : 'Loading…'}
          </p>
        )}
      </div>
    </RunDialog>
  );
}

/**
 * An app in the transcript: one row that names it and opens it. The row reads
 * nothing until it opens; the title comes from the app once it is read.
 */
export const AppAttachmentRow = memo(function AppAttachmentRow({
  client,
  sid,
  attachment,
  embedded = false,
}: {
  client: GatewayClient;
  sid: string;
  attachment: IterationAttachment;
  embedded?: boolean;
}) {
  const viewId = attachment.view_id ?? '';
  const name = appFallbackTitle(attachment.filename);
  const [opened, setOpened] = useStickyOverlay(`app:${viewId}`);
  const close = useCallback(() => setOpened(false), [setOpened]);
  const open = useCallback(() => setOpened(true), [setOpened]);
  if (!viewId) return null;
  return (
    <>
      {embedded ? (
        <ExecutionAction onClick={open} aria-label={`Open app ${name}`}>
          <BandLabel>APP</BandLabel>
          <span className="min-w-0 flex-1 truncate">{name}</span>
        </ExecutionAction>
      ) : (
        <ListRow onClick={open} title={name} aria-label={`Open app ${name}`}>
          <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-accent-ink">APP</span>
          <span className="min-w-0 flex-1 truncate font-mono text-chip text-muted">{name}</span>
        </ListRow>
      )}
      {opened && (
        <AppSurface client={client} sid={sid} viewId={viewId} fallbackTitle={name} onClose={close} />
      )}
    </>
  );
});

/** One app of the session list, with its title and state. */
export function AppListRow({ view, onOpen }: { view: LiveViewModel; onOpen: (view: LiveViewModel) => void }) {
  return (
    <ListRow onClick={() => onOpen(view)} title={view.title} aria-label={`Open app ${view.title}`}>
      <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-accent-ink">APP</span>
      <span className="min-w-0 flex-1 truncate font-mono text-chip text-white">{view.title}</span>
      <span className="shrink-0 font-mono text-chip text-footer-muted">{appStateWord(view)}</span>
    </ListRow>
  );
}

/** The apps of a session, newest last. Empty until the gateway answers. */
export function useSessionApps(client: GatewayClient, sid: string): LiveViewModel[] {
  const [apps, setApps] = useState<LiveViewModel[]>([]);
  useEffect(() => {
    const controller = new AbortController();
    client
      .appViews(sid, false, controller.signal)
      .then((views) => {
        if (!controller.signal.aborted) setApps(views);
      })
      .catch((error: unknown) => {
        if (!controller.signal.aborted) console.warn('app attachments: list failed', sid, error);
      });
    return () => controller.abort();
  }, [client, sid]);
  return apps;
}

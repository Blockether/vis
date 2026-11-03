/**
 * An app attachment: a card in the transcript and the full app surface that it
 * opens in its place. The surface reads the app from the gateway, folds the session events of
 * its view and saves through `draft`, `change` and `submit` actions.
 */

import { createContext, memo, useCallback, useContext, useEffect, useMemo, useRef, useState } from 'react';

import { useAppLog } from '../hooks/useAppLog';
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
import { onSettingChange, type GatewayClient } from '../lib/gateway';
import { appFrameStep, appSurfaces, isAppSurfaceSetting, shownAppFrame } from '../lib/app-surfaces';
import { foldGlobalAppView, globalAppStateWord } from '../lib/global-apps';
import type { HumanInputValue } from '../lib/human-input';
import { liveViewFromWire, type LiveShortcut, type LiveView as LiveViewModel, type LiveNode } from '../lib/live-view';
import { ariaShortcut, isAppKeyTarget, matchShortcut, shortcutLabel, shortcutName } from '../lib/app-shortcuts';
import { SessionEventsContext } from '../lib/session-events';
import { useStickyOverlay } from '../lib/sticky-overlay';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { IterationAttachment, SseEvent } from '../lib/types';
import { VIEW_CLOSE_EVENT, VIEW_OPEN_EVENT, VIEW_PATCH_EVENT, type ViewAction } from '../lib/view';
import { ActionStatus, type AppNodeActions } from './AppNodes';
import { LiveViewPanel, RunDialog } from './LiveView';
import { AppLogButton, AppLogPanel } from './AppLog';
import { ChevronIcon, FullScreenIcon } from './icons';
import { BandButton, BandLabel, DialogFrame, DialogHeader, ExecutionAction, KeyCap, ListRow, Modal, closeWith } from './ui';

/** The open apps of the session, with their surfaces, for the rows of the transcript. */
export const SessionAppViewsContext = createContext<readonly LiveViewModel[]>([]);

/**
 * The focus mark of an app row: a 2px bar in the row's own ink at its start edge.
 * The hover paper alone is too faint on the dialog paper to show the focus.
 */
const APP_ROW_FOCUS = 'focus-visible:shadow-[inset_2px_0_0_currentColor]';

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
  /** The session of the app, or `null` for a global app. */
  sid: string | null,
  viewId: string,
  hub: SessionSubscriptionHub | null,
): AppViewState {
  const [view, setView] = useState<LiveViewModel | null>(null);
  const [isMissing, setMissing] = useState(false);
  const viewRef = useRef<LiveViewModel | null>(null);
  const reading = useRef<AbortController | null>(null);
  // Actions go to the owner one at a time, in the order the human made them: the
  // `change` of a field that loses focus reaches the owner before the click that
  // took the focus away.
  const sending = useRef<Promise<unknown>>(Promise.resolve());
  const echo = useRef<ReturnType<typeof setTimeout> | null>(null);

  const commit = useCallback((next: LiveViewModel | null) => {
    viewRef.current = next;
    setView(next);
  }, []);

  const reread = useCallback(() => {
    reading.current?.abort();
    const controller = new AbortController();
    reading.current = controller;
    (sid === null ? client.globalApp(viewId, controller.signal) : client.liveView(sid, viewId, controller.signal))
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

  // A saved app setting changes where the app shows. The engine answers it on a read.
  useEffect(
    () => onSettingChange(client.base, (updated) => isAppSurfaceSetting(updated.id) && reread()),
    [client, reread],
  );

  useEffect(() => {
    if (!hub) return;
    const fold = (event: SseEvent) => {
      const held = viewRef.current;
      if (!held) return;
      const folded = sid === null ? foldGlobalAppView(held, event) : foldAppEvent(held, event);
      if (folded.view !== held) commit(folded.view);
      if (folded.isReread) reread();
    };
    // A global app lives on the fleet stream, which has no replay.
    const offEvents = sid === null ? hub.subscribeFleet(fold) : hub.subscribeSession(sid, fold, { replay: false });
    let wasConnected: boolean | null = null;
    const onConnection = (connected: boolean) => {
      if (connected && wasConnected === false) reread();
      wasConnected = connected;
    };
    const offConnection = sid === null ? hub.subscribeFleetState(onConnection) : hub.subscribeConnection(onConnection);
    return () => {
      offEvents();
      offConnection();
    };
  }, [hub, sid, commit, reread]);

  const act = useCallback(
    async (action: ViewAction) => {
      const run = sending.current.then(() =>
        runAppAction(() =>
          sid === null ? client.globalAppAction(viewId, action) : client.viewAction(sid, viewId, action),
        ),
      );
      sending.current = run;
      const result = await run;
      const held = viewRef.current;
      if (result.submission && held) commit(withSubmission(held, result.submission));
      if (result.status === 'disconnected' && held && held.is_disconnected !== true) {
        // The owner went away before its stale picture arrived: stop at once, then read it.
        commit({ ...held, is_disconnected: true });
        reread();
      } else if (result.status === 'conflict' || result.status === 'stale' || result.status === 'closed') {
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

/** The label of node `id` in `nodes`, for the key legend. */
function nodeLabel(nodes: readonly LiveNode[], id: string): string | undefined {
  for (const node of nodes) {
    if (node.id === id) {
      if (node.type === 'button') return node.label;
      if (node.type === 'card') return node.title;
      return node.label;
    }
    if (node.type === 'group') {
      const found = nodeLabel(node.children, id);
      if (found !== undefined) return found;
    }
  }
  return undefined;
}

/** The field errors after one `change` answer: a refusal sets the error of its input, an acceptance clears it. */
function withFieldError(
  held: Readonly<Record<string, string>>,
  nodeId: string,
  result: AppActionResult,
): Readonly<Record<string, string>> {
  if (result.status === 'invalid') {
    const message = result.errors?.[nodeId] ?? 'Not valid';
    return held[nodeId] === message ? held : { ...held, [nodeId]: message };
  }
  if (result.status !== 'saved' || !(nodeId in held)) return held;
  const { [nodeId]: _cleared, ...rest } = held;
  void _cleared;
  return rest;
}

/** The most key actions that wait for the owner at one time. More presses are dropped. */
export const APP_KEY_QUEUE = 8;

/** How long a pressed key stays lit in the legend. */
const KEY_FLASH_MS = 180;

/**
 * The key legend of an app. Each entry is also a button, so a touch screen without
 * keys can press every shortcut.
 */
function KeyLegend({
  shortcuts,
  nodes,
  lit,
  onPress,
}: {
  shortcuts: readonly LiveShortcut[];
  nodes: readonly LiveNode[];
  lit: string | null;
  onPress: (shortcut: LiveShortcut) => void;
}) {
  return (
    // A phone scrolls the row sideways. Each entry is a whole 44px box there,
    // because a scroller clips an invisible reach that stands outside it.
    <div
      role="toolbar"
      aria-label="App keys"
      className="-mx-4 -my-2 flex flex-nowrap gap-x-1 overflow-x-auto px-3 [scrollbar-width:none] sm:flex-wrap sm:overflow-x-visible mouse:gap-x-3 mouse:gap-y-2 mouse:px-4 mouse:py-2"
    >
      {shortcuts.map((shortcut) => {
        const label = shortcutLabel(shortcut, shortcut.node_id ? nodeLabel(nodes, shortcut.node_id) : undefined);
        return (
          <button
            key={shortcut.key}
            type="button"
            // The cap is decorative, so the name says the key: `Next, key n`.
            aria-label={shortcutName(shortcut, label)}
            aria-keyshortcuts={ariaShortcut(shortcut.key)}
            data-shortcut-key={shortcut.key}
            // A finger gets a whole 44px box; a mouse gets a 20px face in a wrapped row.
            className={`inline-flex h-11 min-h-11 min-w-11 shrink-0 items-center justify-center gap-1.5 whitespace-nowrap px-1.5 font-mono text-meta font-bold normal-case tracking-normal transition-colors duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60 motion-reduce:transition-none mouse:-mx-1 mouse:h-5 mouse:min-h-5 mouse:min-w-0 mouse:px-1 ${
              lit === shortcut.key ? 'text-accent-ink' : 'text-dialog-hint hover:text-white'
            }`}
            onClick={() => onPress(shortcut)}
          >
            <KeyCap keyName={shortcut.key} isLit={lit === shortcut.key} />
            <span>{label}</span>
          </button>
        );
      })}
    </div>
  );
}

/**
 * The opened app: its title in a band and its nodes in the shared renderer.
 * In the transcript it opens in place of its row (`isInline`); the Artifacts
 * sheet opens it in the session dialog.
 */
export function AppSurface({
  client,
  sid,
  viewId,
  fallbackTitle,
  isInline = false,
  isPane = false,
  closeLabel,
  backLabel,
  onFullScreen,
  onView,
  onClose,
}: {
  client: GatewayClient;
  /** The session of the app, or `null` for a global app, which opens over the whole shell. */
  sid: string | null;
  viewId: string;
  fallbackTitle: string;
  /** Open in the transcript, where the row was, not in a dialog. */
  isInline?: boolean;
  /** Fill the box that holds it: the side panel or the chat area. */
  isPane?: boolean;
  /** The name of the way out, when it is not "Close app <title>". */
  closeLabel?: string;
  /** Lead the band with a Back control of this name instead of a close at its end. */
  backLabel?: string;
  /** Offer a Full screen control that opens the same app in the session dialog. */
  onFullScreen?: () => void;
  /** Hear each new picture of the app, for example to learn its surfaces. */
  onView?: (view: LiveViewModel) => void;
  onClose: () => void;
}) {
  const hub = useContext(SessionEventsContext);
  const { view, isMissing, act } = useAppView(client, sid, viewId, hub);
  useEffect(() => {
    if (view) onView?.(view);
  }, [view, onView]);
  const isFramed = isInline || isPane;
  const buttonKeys = useRef(new Map<string, SendKeys>());
  const [formStatus, setFormStatus] = useState<AppActionStatus>('idle');
  const [fieldErrors, setFieldErrors] = useState<Readonly<Record<string, string>>>({});
  const [lit, setLit] = useState<string | null>(null);
  const surface = useRef<HTMLDivElement | null>(null);
  const keyQueue = useRef<{ tail: Promise<unknown>; waiting: number }>({ tail: Promise.resolve(), waiting: 0 });
  const isReadOnly = !view || view.is_closed === true || view.is_disconnected === true;
  const shortcuts = useMemo(() => (isReadOnly ? [] : (view?.shortcuts ?? [])), [isReadOnly, view?.shortcuts]);

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
      setFieldErrors((held) => withFieldError(held, nodeId, result));
      setFormStatus(result.status);
      return result.status === 'saved' ? result.seq : undefined;
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
      if (result.status === 'invalid') setFieldErrors(result.errors ?? {});
      else if (result.status === 'saved') setFieldErrors({});
      setFormStatus(result.status);
    },
    [view, act],
  );

  // A button or a selectable card reports to the owner. Only a refusal shows a
  // word: a press that the owner took changes the app, and that is its answer.
  const onActivate = useCallback(
    async (nodeId: string) => {
      const result = await act({ action: 'activate', node_id: nodeId });
      // An owner can refuse a press for the values typed so far, as Add refuses an empty task.
      if (result.status === 'invalid') setFieldErrors(result.errors ?? {});
      else if (result.status === 'saved') setFieldErrors((held) => (Object.keys(held).length === 0 ? held : {}));
      setFormStatus(result.status === 'saved' ? 'idle' : result.status);
    },
    [act],
  );

  const onSelect = useCallback(
    (nodeId: string, itemIds: string[]) => {
      void act({ action: 'select', node_id: nodeId, item_ids: itemIds }).then((result) =>
        setFormStatus(result.status === 'saved' ? 'idle' : result.status),
      );
    },
    [act],
  );

  // One shortcut, pressed by its key or by its legend entry. A node shortcut
  // clicks the node itself, so a disabled or pending node ignores the key exactly
  // as it ignores a click. Owner actions wait in order: a game reads its moves in
  // the order they were pressed, and parallel requests can arrive out of order.
  const press = useCallback(
    (shortcut: LiveShortcut) => {
      setLit(shortcut.key);
      if (shortcut.node_id) {
        const target = surface.current?.querySelector<HTMLButtonElement>(
          `[data-node-id="${CSS.escape(shortcut.node_id)}"]`,
        );
        if (target && !target.disabled) target.click();
        return;
      }
      const queue = keyQueue.current;
      if (queue.waiting >= APP_KEY_QUEUE) return;
      queue.waiting += 1;
      queue.tail = queue.tail
        .then(() => act({ action: 'shortcut', key: shortcut.key }))
        .then((result) => setFormStatus(result.status === 'saved' ? 'idle' : result.status))
        .catch((error: unknown) => console.warn('app attachment: shortcut failed', viewId, shortcut.key, error))
        .finally(() => {
          queue.waiting -= 1;
        });
    },
    [act, viewId],
  );

  useEffect(() => {
    if (lit === null) return;
    const timer = setTimeout(() => setLit(null), KEY_FLASH_MS);
    return () => clearTimeout(timer);
  }, [lit]);

  // The opened app takes the focus, so its keys work at once, also when a row
  // behind it opened it. Closing gives the focus back to that row.
  // The dialog can mount its body again when its layer moves, so each new body
  // takes the focus that nothing else holds.
  const opener = useRef<Element | null>(null);
  const attachSurface = useCallback((node: HTMLDivElement | null) => {
    surface.current = node;
    if (!node) return;
    opener.current ??= document.activeElement ?? document.body;
    const held = document.activeElement;
    if (!held || held === document.body || held === opener.current || !held.isConnected)
      node.focus({ preventScroll: true });
  }, []);
  useEffect(() => {
    return () => {
      const back = opener.current;
      if (back instanceof HTMLElement && back.isConnected) back.focus({ preventScroll: true });
    };
  }, []);

  useEffect(() => {
    if (shortcuts.length === 0) return;
    function onKey(event: KeyboardEvent) {
      if (event.defaultPrevented) return;
      // In a dialog, the key belongs to this app when nothing else holds the focus:
      // the page itself, the row that opened the app, or any control inside this
      // app's own dialog. A dialog opened over the app keeps its keys. In the
      // transcript, only a focus inside the app gives it the key, so the page keeps
      // its scroll keys and two open apps never share one press.
      const target = event.target;
      const frame = surface.current?.closest(isFramed ? '[data-app-frame]' : '[role="dialog"]') ?? null;
      if (isFramed ? !(target instanceof Node && frame?.contains(target)) : !isAppKeyTarget(target, frame, opener.current))
        return;
      // Enter and Space on a focused control press that control, as they always do.
      if (
        (event.key === 'Enter' || event.key === ' ') &&
        target instanceof Element &&
        target.closest('button, a[href], summary, [role="button"], [role="option"], [role="tab"]')
      )
        return;
      const shortcut = matchShortcut(shortcuts, event);
      if (!shortcut) return;
      event.preventDefault();
      press(shortcut);
    }
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [shortcuts, press, isFramed]);

  const log = view?.log;
  const appLog = useAppLog(log, surface, isFramed);

  const title = view?.title || fallbackTitle;
  const state = view ? (sid === null ? globalAppStateWord(view) : appStateWord(view)) : '';
  const subtitle = view ? (
    <span className="flex items-center gap-2">
      <span>{state}</span>
      {/* A read-only app says why in its state word; an old refusal adds nothing. */}
      <ActionStatus status={isReadOnly ? 'idle' : formStatus} isOnBand />
    </span>
  ) : undefined;
  const footer =
    view && shortcuts.length > 0 ? (
      <KeyLegend shortcuts={shortcuts} nodes={view.nodes} lit={lit} onPress={press} />
    ) : undefined;
  const body = (
    <div
      ref={attachSurface}
      tabIndex={-1}
      className={`min-w-0 focus:outline-none ${isInline ? '' : 'min-h-0 flex-1 overflow-y-auto overscroll-contain'}`}
      data-app-surface={viewId}
    >
      {view ? (
        <LiveViewPanel
          view={view}
          inDialog
          isSettled={view.is_closed === true}
          app={actions}
          errors={fieldErrors}
          onActivate={isReadOnly ? undefined : onActivate}
          onSelect={isReadOnly ? undefined : onSelect}
          onChange={isReadOnly ? undefined : onChange}
          onSubmit={isReadOnly ? undefined : onSubmit}
          load={
            sid === null
              ? undefined
              : (nodeId, from, limit, query) => client.liveViewLog(sid, viewId, nodeId, from, limit, query)
          }
        />
      ) : (
        <p role="status" className="p-4 font-mono text-meta text-dialog-hint">
          {isMissing ? 'This app could not be read.' : 'Loading…'}
        </p>
      )}
    </div>
  );
  const leave = closeLabel ?? `Close app ${title}`;
  const bandActions = (
    <>
      <AppLogButton log={log} title={title} isOpen={appLog.isOpen} onToggle={appLog.toggle} />
      {onFullScreen && (
        <BandButton label={`Full screen ${title}`} onClick={onFullScreen}>
          <FullScreenIcon className="size-3.5" />
        </BandButton>
      )}
    </>
  );
  const logPanel = appLog.isOpen && log ? <AppLogPanel log={log} title={title} isPane={!isFramed} /> : null;
  if (isFramed) {
    return (
      // Escape inside the app closes it, as it closes the dialog. An armed control
      // inside stops the key first.
      <section
        aria-label={title}
        data-app-frame={viewId}
        className={`flex min-w-0 flex-col bg-panel ${isPane ? 'min-h-0 flex-1' : 'border border-dialog-edge'}`}
        onKeyDown={(event) => {
          if (event.key === 'Escape' && !event.defaultPrevented) {
            event.preventDefault();
            onClose();
          }
        }}
      >
        <DialogHeader
          title={title}
          isTitleTruncated
          subtitle={subtitle}
          leading={
            backLabel ? (
              <BandButton label={backLabel} onClick={onClose}>
                <ChevronIcon back className="size-4" />
              </BandButton>
            ) : undefined
          }
          actions={bandActions}
          {...closeWith(backLabel ? undefined : onClose, leave)}
        />
        {/* In a pane the log takes at most half of the frame and scrolls; the app keeps the rest. */}
        {logPanel && (
          <div className={`border-t border-dialog-edge ${isPane ? 'flex max-h-[50%] min-h-0 flex-col' : ''}`}>{logPanel}</div>
        )}
        <div className={`border-t border-dialog-edge ${isPane ? 'flex min-h-0 flex-1 flex-col' : ''}`}>{body}</div>
        {footer && (
          <footer className="border-t border-dialog-edge bg-panel-2 px-4 py-2 font-mono text-meta text-dialog-hint">
            {footer}
          </footer>
        )}
      </section>
    );
  }
  return (
    <RunDialog
      title={title}
      subtitle={subtitle}
      footer={footer}
      closeLabel={closeLabel}
      within={sid === null ? 'app' : 'session'}
      actions={bandActions}
      onClose={onClose}
    >
      {logPanel}
      {body}
    </RunDialog>
  );
}

/**
 * An app in the transcript: one row that names it and opens it in its place.
 * The row reads nothing until it opens; the title comes from the app once it is read.
 * Full screen opens the same app in the session dialog. An app without the inline
 * surface never shows its frame in the turn: its row opens it full screen.
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
  const [isFull, setFull] = useState(false);
  // The session list knows where the app shows. A row outside it learns it from its read.
  const known = useContext(SessionAppViewsContext).find((one) => one.id === viewId);
  const [readInline, setReadInline] = useState<boolean | null>(null);
  const inline = readInline ?? appSurfaces(known).inline;
  const place = useRef<HTMLDivElement | null>(null);
  const frame = shownAppFrame(opened ? (isFull ? 'full' : 'inline') : 'row', inline);
  const go = useCallback(
    (step: 'open' | 'full' | 'escape' | 'close') => {
      const next = appFrameStep(frame, step, inline);
      setOpened(next !== 'row');
      setFull(next === 'full');
      // The app replaces its row, so a close gives the focus to the row that comes back.
      if (next === 'row')
        requestAnimationFrame(() => place.current?.querySelector<HTMLElement>('button')?.focus({ preventScroll: true }));
    },
    [frame, inline, setOpened],
  );
  const open = useCallback(() => go('open'), [go]);
  const onView = useCallback((view: LiveViewModel) => setReadInline(appSurfaces(view).inline), [setReadInline]);
  if (!viewId) return null;
  const row = embedded ? (
    <ExecutionAction onClick={open} aria-label={`Open app ${name}`}>
      <BandLabel>APP</BandLabel>
      <span className="min-w-0 flex-1 truncate">{name}</span>
    </ExecutionAction>
  ) : (
    <ListRow onClick={open} title={name} aria-label={`Open app ${name}`} className={APP_ROW_FOCUS}>
      <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-accent-ink">APP</span>
      <span className="min-w-0 flex-1 truncate font-mono text-ui text-muted">{name}</span>
    </ListRow>
  );
  return (
    <div ref={place} className="min-w-0">
      {frame === 'inline' ? (
        <AppSurface
          client={client}
          sid={sid}
          viewId={viewId}
          fallbackTitle={name}
          isInline
          onView={onView}
          onFullScreen={() => go('full')}
          onClose={() => go('close')}
        />
      ) : (
        row
      )}
      {frame === 'full' && (
        <AppSurface
          client={client}
          sid={sid}
          viewId={viewId}
          fallbackTitle={name}
          onView={onView}
          // Escape leaves full screen first: back to the turn, or to the row.
          closeLabel={inline ? `Leave full screen ${name}` : undefined}
          onClose={() => go('escape')}
        />
      )}
    </div>
  );
});

/** One app of the session list, with its title and state. */
export function AppListRow({
  view,
  stateWord = appStateWord,
  onOpen,
}: {
  view: LiveViewModel;
  stateWord?: (view: LiveViewModel) => string;
  onOpen: (view: LiveViewModel) => void;
}) {
  return (
    <ListRow onClick={() => onOpen(view)} title={view.title} aria-label={`Open app ${view.title}`} className={APP_ROW_FOCUS}>
      <span className="shrink-0 border border-edge-strong px-1.5 text-chip text-accent-ink">APP</span>
      <span className="min-w-0 flex-1 truncate font-mono text-ui text-white">{view.title}</span>
      <span className="shrink-0 font-mono text-meta text-footer-muted">{stateWord(view)}</span>
    </ListRow>
  );
}

/**
 * The session apps list of the header button. It lists each app whose session
 * surface is not `off`; a pick opens the app where its surface says.
 */
export function SessionAppsDialog({
  apps,
  onPick,
  onClose,
}: {
  apps: readonly LiveViewModel[];
  onPick: (view: LiveViewModel) => void;
  onClose: () => void;
}) {
  return (
    <AppsListDialog
      title="Session apps"
      empty="No app of this session shows here."
      within="session"
      apps={apps}
      onPick={onPick}
      onClose={onClose}
    />
  );
}

/** A list of apps in a dialog. A pick opens one app; Escape closes the list. */
export function AppsListDialog({
  title,
  empty,
  within,
  apps,
  stateWord,
  onPick,
  onClose,
}: {
  title: string;
  /** The note of an empty list. */
  empty: string;
  within: 'app' | 'session';
  apps: readonly LiveViewModel[];
  stateWord?: (view: LiveViewModel) => string;
  onPick: (view: LiveViewModel) => void;
  onClose: () => void;
}) {
  useEffect(() => {
    function onKey(event: KeyboardEvent) {
      if (event.key === 'Escape' && !event.defaultPrevented) onClose();
    }
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);
  return (
    <Modal within={within} size="fit" onDismiss={onClose}>
      <DialogFrame title={title} closeLabel={`Close ${title.toLowerCase()}`} onClose={onClose}>
        {apps.length === 0 ? (
          <p role="status" className="p-4 font-mono text-meta text-dialog-hint">
            {empty}
          </p>
        ) : (
          <ul aria-label={title} className="min-w-0">
            {apps.map((view) => (
              <li key={view.id}>
                <AppListRow view={view} stateWord={stateWord} onOpen={onPick} />
              </li>
            ))}
          </ul>
        )}
      </DialogFrame>
    </Modal>
  );
}

/**
 * The apps of a session, newest last. Empty until the gateway answers.
 *
 * The list follows the session stream: a new app joins it, a patch or a close
 * changes its row, and a missed patch or a reconnect reads the list again.
 */
export function useSessionApps(client: GatewayClient, sid: string): LiveViewModel[] {
  const hub = useContext(SessionEventsContext);
  const [apps, setAppsState] = useState<LiveViewModel[]>([]);
  const held = useRef<LiveViewModel[]>([]);
  const [generation, setGeneration] = useState(0);
  const setApps = useCallback((next: LiveViewModel[]) => {
    held.current = next;
    setAppsState(next);
  }, []);
  useEffect(() => {
    const controller = new AbortController();
    client
      .appViews(sid, false, controller.signal)
      .then((views) => {
        if (controller.signal.aborted) return;
        // A slow list never undoes a patch that arrived while it was read.
        const now = held.current;
        setApps(views.map((read) => newerAppView(now.find((one) => one.id === read.id) ?? null, read)));
      })
      .catch((error: unknown) => {
        if (!controller.signal.aborted) console.warn('app attachments: list failed', sid, error);
      });
    return () => controller.abort();
  }, [client, sid, generation, setApps]);

  useEffect(
    () =>
      onSettingChange(client.base, (updated) => {
        if (isAppSurfaceSetting(updated.id)) setGeneration((one) => one + 1);
      }),
    [client],
  );

  useEffect(() => {
    if (!hub) return;
    const reread = () => setGeneration((one) => one + 1);
    const offEvents = hub.subscribeSession(
      sid,
      (event) => {
        const now = held.current;
        if (event.type === VIEW_OPEN_EVENT) {
          const opened = liveViewFromWire(event.view);
          if (!opened || opened.placement !== 'attachment') return;
          const at = now.findIndex((one) => one.id === opened.id);
          if (at < 0) {
            setApps([...now, opened]);
            return;
          }
          const next = now.slice();
          next[at] = newerAppView(now[at], opened);
          setApps(next);
          return;
        }
        if (event.type !== VIEW_PATCH_EVENT && event.type !== VIEW_CLOSE_EVENT) return;
        let isReread = false;
        let changed = false;
        const next = now.map((app) => {
          const folded = foldAppEvent(app, event);
          if (folded.isReread) isReread = true;
          if (folded.view !== app) changed = true;
          return folded.view;
        });
        if (changed) setApps(next);
        if (isReread) reread();
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
  }, [hub, sid, setApps]);
  return apps;
}

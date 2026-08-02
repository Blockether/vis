import { memo, useCallback, useDeferredValue, useEffect, useLayoutEffect, useMemo, useRef, useState, type CSSProperties } from 'react';
import { createPortal } from 'react-dom';
import { Banner, Button, DialogFrame, Input, Spinner } from '../components/ui';
import { GatewayClient, type SessionMatch } from '../lib/gateway';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import type { GatewayConn, Session, SessionUsage, WorkspaceDraft } from '../lib/types';
import { homeifyPath } from '../lib/path';
import { onWake } from '../lib/wake';
import { seedReadMarks, unreadTurnCount, useReadMarks } from '../lib/unread';
import { PencilIcon, SwipeActions, TrashIcon } from '../components/SwipeActions';
import { DEFAULT_SESSION_PAGE_SIZE, getSessionsPerPage, subscribeSessionsPerPage } from '../lib/storage';

const SESSION_LIST_EVENTS = new Set([
  'turn.started',
  'turn.completed',
  'turn.failed',
  'turn.cancelled',
  'session.title_updated',
]);

// A background poll issued right before the OS suspended the webview can never
// settle: it neither resolves nor rejects after the resume. A plain in-flight
// boolean would then stay latched forever and every later refresh would be
// skipped — the list froze until the app was restarted. Anything older than
// this is treated as lost.
const STALE_POLL_MS = 20_000;

// Each project is a collapsible section: collapsed shows its live sessions plus any
// that stopped being live within the last hour (capped), expanded pages the rest of
// its history in place — so the DOM is bounded without a global window over the fleet.
const RECENT_WINDOW_MS = 60 * 60 * 1000;

// Same frames as the session transcript's spinner and the TUI's
// `paint-content-loading!` — one vocabulary for "working" across the product.
// Two placeholder projects with ragged title widths: an even grid reads as a
// rendered table, a ragged one reads as text that has not arrived yet.
const SKELETON_GROUPS = [
  ['w-3/5', 'w-2/5', 'w-1/2'],
  ['w-1/2', 'w-2/3'],
];


function useSessionsPerPage(): number {
  const [pageSize, setPageSize] = useState(DEFAULT_SESSION_PAGE_SIZE);
  useEffect(() => {
    let active = true;
    void getSessionsPerPage().then((value) => {
      if (active) setPageSize(value);
    });
    const unsubscribe = subscribeSessionsPerPage((value) => setPageSize(value));
    return () => {
      active = false;
      unsubscribe();
    };
  }, []);
  return pageSize;
}

// `Date.now()` that advances on an interval, so time-windowed filters age out on
// their own without a manual refresh. Keep the interval coarse (>= 1s): each tick
// recomputes everything that depends on it.
function useNow(intervalMs: number): number {
  const [now, setNow] = useState(() => Date.now());
  useEffect(() => {
    const timer = window.setInterval(() => setNow(Date.now()), intervalMs);
    return () => window.clearInterval(timer);
  }, [intervalMs]);
  return now;
}

/**
 * The start-in menu's desktop width in px. The popover is RIGHT-aligned to the
 * caret, so the anchor math needs the width before the menu has ever been measured;
 * it must stay equal to the `sm:w-80` the menu paints itself at.
 */
const START_MENU_WIDTH = 320;

/**
 * Where a new session begins. `trunk` is the plain session this screen always made:
 * the agent edits the project directly. The other two are DRAFTS — isolated clones
 * parked at `~/.vis/drafts/<repo>/<label>` — either forked fresh (a clone of the repo
 * as it stands, or one rewound to the last commit) or an existing draft someone
 * stashed earlier.
 */
type StartIn =
  | { kind: 'trunk' }
  | { kind: 'fork'; label: string; clean: boolean }
  | { kind: 'resume'; draft: WorkspaceDraft };

interface Props {
  active: GatewayConn | null;
  client: GatewayClient | null;
  subscriptions: SessionSubscriptionHub | null;
  subscribedIds: ReadonlySet<string>;
  /** The gateway stopped answering — the shell decides what to show instead. */
  onUnreachable?: (message: string | null) => void;
  onOpen: (conn: GatewayConn, sid: string, fresh?: boolean) => void | Promise<void>;
}

export function SessionsScreen({ active, client, subscriptions, subscribedIds, onUnreachable, onOpen }: Props) {
  // Seed from the gateway's last known list so returning to this tab repaints the
  // previous frame instantly; the effect below revalidates and reconciles on top.
  const [sessions, setSessions] = useState<Session[] | null>(() => client?.cachedSessions() ?? null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [query, setQuery] = useState('');
  // Keep keystrokes immediate even when a large session fleet is regrouped.
  const deferredQuery = useDeferredValue(query);
  const [transcriptMatches, setTranscriptMatches] = useState<Map<string, SessionMatch> | null>(null);
  const [createBusy, setCreateBusy] = useState(false);
  const [createError, setCreateError] = useState<string | null>(null);
  // "Creating..." is a lie while a 12k-file repo is being cloned, so the busy word
  // follows the WORK: fork, enter, or plain create.
  const [createBusyLabel, setCreateBusyLabel] = useState('Creating...');
  // New session is a SPLIT control. The caret half opens this menu, which is the only
  // place in the app that answers "in which workspace?" — the web twin of `/draft new`,
  // `/draft blank` and `/draft resume`. Portalled and viewport-anchored because the
  // header panel clips its overflow.
  const [startMenu, setStartMenu] = useState<{ top: number; left: number } | null>(null);
  const startAnchorRef = useRef<HTMLButtonElement>(null);
  // null = never read for this menu opening; [] = read, nothing parked.
  const [drafts, setDrafts] = useState<WorkspaceDraft[] | null>(null);
  const [draftsError, setDraftsError] = useState<string | null>(null);
  // Forking asks for the draft's name first: the gateway rejects a blank label, and
  // the name is what `/draft list` and every later resume will show.
  const [namePrompt, setNamePrompt] = useState<{ clean: boolean } | null>(null);
  const [draftLabel, setDraftLabel] = useState('');
  const pollStartedAt = useRef<number | null>(null);
  // Swipe-revealed row actions. One dialog serves both: renaming asks for the new
  // title, deleting asks for consent — a destructive tap two pixels from a thumb
  // rest position must never be one-way.
  const [rowAction, setRowAction] = useState<{ mode: 'rename' | 'delete'; session: Session } | null>(
    null,
  );
  const [renameDraft, setRenameDraft] = useState('');
  const [actionBusy, setActionBusy] = useState(false);
  const [actionError, setActionError] = useState<string | null>(null);
  const listRef = useRef<HTMLDivElement>(null);
  const refreshAnchorRef = useRef<{ id: string; top: number } | null>(null);
  const activeRef = useRef(active);
  const clientRef = useRef(client);
  // Refs mirror the latest props for callbacks that must not re-subscribe on every
  // connection object identity change. Written in an effect so render stays pure.
  useEffect(() => {
    activeRef.current = active;
    clientRef.current = client;
  });
  const activeKey = active ? `${active.url}\u0000${active.token ?? ''}` : '';
  // Repaint when a read mark moves — opening a session clears its badge from here.
  const readMarks = useReadMarks();

  // A session this device has never met is NOT unread: seed it at the turn count
  // it arrived with, so only answers that land AFTER this point raise a badge.
  // Without the seed, a fresh install would paint the whole fleet unread.
  useEffect(() => {
    if (sessions) seedReadMarks(sessions);
  }, [sessions]);

  const load = useCallback(
    async (signal?: AbortSignal, background = false) => {
      const connection = activeRef.current;
      if (!connection) {
        setSessions([]);
        setLoadError(null);
        return;
      }
      if (background) {
        const started = pollStartedAt.current;
        if (started !== null && Date.now() - started < STALE_POLL_MS) return;
        pollStartedAt.current = Date.now();
      }
      try {
        const client = clientRef.current ?? new GatewayClient(connection);
        // Paint the first page the moment it lands instead of waiting for the whole
        // fleet to drain. Only ever called on a cold load (see `listSessions`).
        const next = await client.listSessions(signal, (partial) => {
          if (signal?.aborted) return;
          setSessions((current) => reconcileSessions(current, partial));
        });
        if (!signal?.aborted) {
          // Anchor EVERY reload: the 10s poll can reorder rows under a reading
          // thumb. The layout effect below no-ops at the top of the list, so the
          // first paint is unaffected.
          refreshAnchorRef.current = visibleListAnchor(listRef.current);
          setSessions((current) => reconcileSessions(current, next));
          setLoadError(null);
        }
      } catch (cause) {
        if (!signal?.aborted && !background) {
          setLoadError((cause as Error).message);
        }
      } finally {
        if (background) pollStartedAt.current = null;
      }
    },
    [],
  );

  useEffect(() => {
    const controller = new AbortController();
    const refreshLiveStates = () => {
      if (document.visibilityState === 'visible') void load(controller.signal, true);
    };

    // A gateway switch paints THAT gateway's snapshot rather than the previous
    // one's rows. Same array identity as the seed above, so a plain remount is a
    // no-op re-render, not a repaint.
    const cached = clientRef.current?.cachedSessions() ?? null;
    if (cached) setSessions(cached);
    if ((sessions === null && !cached) || !active) void load(controller.signal);
    else void load(controller.signal, true);
    // 10s: slow enough to stay cheap, fast enough that a phone picked up mid-turn
    // shows the truth. Cheap on BOTH ends — an unchanged fleet comes back as a 304
    // with no body (see `GatewayClient.listSessions`), and `load(_, true)` drops a
    // tick that fires while the previous one is still in flight instead of queueing
    // it. Hidden tabs poll not at all.
    const timer = window.setInterval(refreshLiveStates, 10_000);
    // Waking is the one moment the rows are guaranteed stale, and a suspended
    // poll may still be latched: drop the latch, then refresh.
    const stopWake = onWake(() => {
      pollStartedAt.current = null;
      refreshLiveStates();
    });
    return () => {
      controller.abort();
      window.clearInterval(timer);
      stopWake();
    };
    // A connection identity change should preserve the existing frame until its data arrives.
  }, [activeKey, load]);

  useEffect(() => {
    if (!subscriptions) return;
    let refreshTimer: number | null = null;
    const unsubscribe = subscriptions.subscribeFleet((event) => {
      if (!SESSION_LIST_EVENTS.has(event.type)) return;
      if (refreshTimer !== null) window.clearTimeout(refreshTimer);
      // Coalesce lifecycle bursts, then ask the gateway for its canonical order.
      refreshTimer = window.setTimeout(() => void load(undefined, true), 120);
    });
    return () => {
      unsubscribe();
      if (refreshTimer !== null) window.clearTimeout(refreshTimer);
    };
  }, [load, subscriptions]);

  useLayoutEffect(() => {
    const anchor = refreshAnchorRef.current;
    const viewport = listRef.current;
    refreshAnchorRef.current = null;
    if (!anchor || !viewport || viewport.scrollTop <= 2) return;
    const row = Array.from(viewport.querySelectorAll<HTMLElement>('[data-session-id]'))
      .find((element) => element.dataset.sessionId === anchor.id);
    if (row) viewport.scrollTop += row.getBoundingClientRect().top - anchor.top;
  }, [sessions]);

  // Transcript search runs server-side (matches user requests + LLM responses)
  // and unions its matching ids into the local title/project filter.
  useEffect(() => {
    const needle = deferredQuery.trim();
    // An empty query has no server matches; `matches` below derives that without
    // writing state from this effect.
    if (!needle) return;
    const connection = activeRef.current;
    if (!connection) return;
    const controller = new AbortController();
    const timer = window.setTimeout(() => {
      void (clientRef.current ?? new GatewayClient(connection))
        .searchSessionMatches(needle, controller.signal)
        .then((matches) => {
          if (!controller.signal.aborted) {
            setTranscriptMatches(new Map(matches.map((m) => [m.sessionId, m])));
          }
        })
        .catch(() => {
          if (!controller.signal.aborted) setTranscriptMatches(null);
        });
    }, 200);
    return () => {
      controller.abort();
      window.clearTimeout(timer);
    };
  }, [deferredQuery, activeKey]);

  const matches = deferredQuery.trim() ? transcriptMatches : null;

  const visible = useMemo(() => {
    if (!sessions) return null;
    const needle = deferredQuery.trim().toLowerCase();
    return sessions.filter((session) => {
      if (emptyUntitled(session)) return false;
      return (
        !needle ||
        sessionSearchText(session).includes(needle) ||
        matches?.has(session.id) === true
      );
    });
  }, [deferredQuery, sessions, matches]);

  const totals = useMemo(() => {
    const all = sessions?.length ?? 0;
    const shown = visible?.length ?? 0;
    const projects = new Set(sessions?.map(projectLabel) ?? []).size;
    const live = sessions?.filter(sessionIsLive).length ?? 0;
    const unread = sessions?.filter((session) => unreadTurnCount(session) > 0).length ?? 0;
    return { all, shown, projects, live, unread };
    // `readMarks` is the store version: marks change outside React, so it is the
    // dependency that makes the unread tally recompute.
  }, [sessions, visible, readMarks]);

  /**
   * Drafts are REPO-scoped, and the gateway only lists them through a session that
   * lives in that repo. The picker therefore reads them off the most recent session
   * that has a workspace, and the menu NAMES that repo — a fleet spanning several
   * projects must not be told these drafts belong to whatever it creates next.
   */
  const draftProbe = useMemo(
    () => sessions?.find((session) => projectPath(session)) ?? null,
    [sessions],
  );
  const draftRepo = draftProbe ? projectLabel(draftProbe) : '';

  const openStartMenu = useCallback(() => {
    const rect = startAnchorRef.current?.getBoundingClientRect();
    if (!rect) return;
    setStartMenu({
      top: Math.round(rect.bottom + 6),
      left: Math.round(Math.max(12, rect.right - START_MENU_WIDTH)),
    });
  }, []);

  const closeStartMenu = useCallback((restoreFocus = false) => {
    setStartMenu(null);
    if (restoreFocus) startAnchorRef.current?.focus();
  }, []);

  // Read the parked drafts the first time the menu opens, and again after a fork or
  // resume invalidated the list. A failure is reported IN the menu: the three fixed
  // choices above it still work without it.
  useEffect(() => {
    if (!startMenu || drafts !== null) return;
    const connection = activeRef.current;
    if (!draftProbe || !connection) {
      setDrafts([]);
      return;
    }
    const controller = new AbortController();
    setDraftsError(null);
    void (clientRef.current ?? new GatewayClient(connection))
      .drafts(draftProbe.id, controller.signal)
      .then((rows) => setDrafts(rows))
      .catch((cause) => {
        if (controller.signal.aborted) return;
        setDrafts([]);
        setDraftsError((cause as Error).message);
      });
    return () => controller.abort();
  }, [startMenu, drafts, draftProbe]);

  // An anchored popover whose anchor moved is a lie, so a resize closes it. Escape
  // closes it and hands focus back to the caret it came from.
  useEffect(() => {
    if (!startMenu) return;
    const close = () => setStartMenu(null);
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') closeStartMenu(true);
    };
    window.addEventListener('keydown', onKey);
    window.addEventListener('resize', close);
    return () => {
      window.removeEventListener('keydown', onKey);
      window.removeEventListener('resize', close);
    };
  }, [startMenu, closeStartMenu]);

  /**
   * Create the session, then put it where the user asked. The workspace move is a
   * SECOND call by construction (the gateway forks through the session that will own
   * the draft), so a failed fork must not leave a session sitting on trunk — the one
   * place the user said not to work. It has no turns yet, so it is taken back out.
   */
  async function createSession(startIn: StartIn = { kind: 'trunk' }) {
    if (!active) return;
    setCreateBusy(true);
    setCreateBusyLabel(
      startIn.kind === 'fork' ? 'Forking...' : startIn.kind === 'resume' ? 'Entering...' : 'Creating...',
    );
    setCreateError(null);
    setStartMenu(null);
    try {
      const api = client ?? new GatewayClient(active);
      const session = await api.createSession({});
      if (startIn.kind !== 'trunk') {
        try {
          if (startIn.kind === 'fork')
            await api.createDraft(session.id, startIn.label, false, startIn.clean);
          else await api.resumeDraft(session.id, startIn.draft.workspace_id);
        } catch (cause) {
          await api.deleteSession(session.id).catch(() => {});
          throw cause;
        }
        // The repo's draft list just changed; re-read it next time the menu opens.
        setDrafts(null);
      }
      await load();
      if (session.id) await onOpen(active, session.id, true);
    } catch (cause) {
      setCreateError((cause as Error).message);
    } finally {
      setCreateBusy(false);
    }
  }

  function askDraftName(clean: boolean) {
    setStartMenu(null);
    setDraftLabel('');
    setNamePrompt({ clean });
  }

  function commitDraftName() {
    const label = draftLabel.trim();
    if (!namePrompt || !label) return;
    const clean = namePrompt.clean;
    setNamePrompt(null);
    void createSession({ kind: 'fork', label, clean });
  }

  const startRename = useCallback((session: Session) => {
    setRowAction({ mode: 'rename', session });
    setRenameDraft(session.title?.trim() ?? '');
    setActionError(null);
  }, []);

  const startDelete = useCallback((session: Session) => {
    setRowAction({ mode: 'delete', session });
    setActionError(null);
  }, []);

  function closeRowAction() {
    if (actionBusy) return;
    setRowAction(null);
    setActionError(null);
  }

  async function commitRowAction() {
    if (!rowAction || !active) return;
    const title = renameDraft.trim();
    if (rowAction.mode === 'rename' && !title) {
      setActionError('A session name cannot be empty.');
      return;
    }
    setActionBusy(true);
    setActionError(null);
    try {
      const api = client ?? new GatewayClient(active);
      if (rowAction.mode === 'rename') await api.renameSession(rowAction.session.id, title);
      else await api.deleteSession(rowAction.session.id);
      setRowAction(null);
      await load();
    } catch (cause) {
      setActionError((cause as Error).message);
    } finally {
      setActionBusy(false);
    }
  }

  const pageSize = useSessionsPerPage();

  // Projects collapse to their live sessions and page their history independently,
  // so there is no global window to grow. A search flattens every project open so
  // matches are never hidden behind a collapse.
  const [expanded, setExpanded] = useState<ReadonlySet<string>>(() => new Set());
  const forceExpand = deferredQuery.trim().length > 0;
  const toggleProject = useCallback((project: string) => {
    setExpanded((previous) => {
      const next = new Set(previous);
      if (next.has(project)) next.delete(project);
      else next.add(project);
      return next;
    });
  }, []);

  const groups = useMemo(() => groupByProject(visible ?? []), [visible]);

  // A dead gateway is not a sessions problem: there is nothing to navigate, so the
  // shell drops us on the Machines screen instead of rendering a session list
  // shaped like an error. Reporting it is this screen's only job here.
  //
  // Only TRANSITIONS are reported. A fresh mount starts with `loadError === null`,
  // and announcing "reachable" before the first request has answered is how this
  // screen used to un-gate the shell, get itself re-mounted, fail again, and gate
  // the shell again — a mount/fail/unmount loop that hammered the dead gateway
  // with thousands of requests per second instead of resting on Machines.
  const reportedError = useRef<string | null | undefined>(undefined);
  useEffect(() => {
    if (reportedError.current === undefined && loadError === null) {
      reportedError.current = null;
      return;
    }
    if (reportedError.current === loadError) return;
    reportedError.current = loadError;
    onUnreachable?.(loadError);
  }, [loadError, onUnreachable]);

  if (loadError) return null;

  return (
    <section aria-label="Sessions" className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col pb-0 pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] pt-0 transition-[opacity,transform,translate,scale,rotate] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:px-6 sm:pb-6 sm:pt-6">
      {/* On phones this panel sits FLUSH under the app header, whose own `border-b`
          already draws the rule below the Vis mark. A `border-y` here stacked a
          second hairline on top of it, so the Sessions tab wore a 2px seam while
          Machines (which floats its cards below a gap) wore 1px. Bottom edge only;
          the full box comes back once the panel detaches at `sm`. */}
      <div className="flex h-full min-h-0 flex-col overflow-hidden border-b border-dialog-edge bg-panel sm:border">
        <div className="bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
          <div className="flex items-center justify-between gap-3">
            <div className="min-w-0">
              <p className="font-mono text-body font-bold text-white">Projects</p>
              <p className="mt-0.5 flex flex-wrap items-center gap-x-3 gap-y-0.5 font-mono text-meta text-dialog-hint">
                {sessions === null ? (
                  <>
                    <Spinner className="text-accent-ink" />
                    <span>Reading sessions...</span>
                  </>
                ) : (
                  <>
                    {/* Facts travel as WHOLE units. Every value used to be its own flex
                        child with a bare `·` child between them, so a wrap left the
                        separator dangling at the end of the line ("447 sessions ·") and
                        could strand "●" from its "4 live". Each fact is now nowrap and
                        the groups are separated by SPACE rather than punctuation, so the
                        line can only break between facts. */}
                    <span className="whitespace-nowrap">
                      {totals.projects} {totals.projects === 1 ? 'project' : 'projects'}
                      <span className="px-1 opacity-40">·</span>
                      {totals.all} {totals.all === 1 ? 'session' : 'sessions'}
                    </span>
                    <span className={`whitespace-nowrap ${totals.live > 0 ? 'font-bold text-ok' : ''}`}>
                      {totals.live > 0 ? '●' : '○'} {totals.live} live
                    </span>
                    {totals.unread > 0 && (
                      <span
                        className="whitespace-nowrap font-bold text-accent-ink"
                        role="status"
                        aria-live="polite"
                      >
                        {totals.unread} unread
                      </span>
                    )}
                    {/* No refresh status here on purpose. This line is `flex-wrap`,
                        so appending "· refreshing..." could rewrap it onto a second
                        line and shove the whole list down. The list revalidates
                        itself every 10s, on wake and on fleet events — there is no
                        manual refresh left to report. */}
                  </>
                )}
              </p>
            </div>
            {/* One action, one weight. Refresh used to sit here as a second, quieter
                button; the list now revalidates itself on a timer, and a control whose
                honest label is almost always "already up to date" is noise beside the
                only thing a user actually comes here to do. */}
            {/* SPLIT control: one button, two jobs. The wide half stays the plain new
                session it always was — the common path never grows a click — and the
                caret half is where the workspace question lives, so starting in a draft
                stops being a TUI-only slash command. The caret is painted in the theme's
                title-bar ink, not the accent: two amber halves read as one 33px-wider
                button, while amber + black reads as "there is a second thing here".
                Neither half animates on press — the caret ANCHORS the popover, and a
                transform would drag the menu's measured box with it. The seam is the
                caret's OWN foreground, not the accent: one shipped palette paints its
                title bar amber, and there the halves would otherwise fuse. */}
            <div className="flex shrink-0 items-stretch">
              <Button
                variant="solid"
                pressEffect="none"
                className="min-h-6 border-r-0 px-2 py-0.5 font-mono text-chip sm:min-h-6"
                disabled={createBusy || !active}
                onClick={() => void createSession()}
              >
                {/* Same fixed-width stack as Refresh: "Creating..." is wider than
                    "New" on a narrow phone, and this grid column is content-sized,
                    so the busy state would otherwise shove the header text left. */}
                <span className="grid justify-items-center">
                  <span aria-hidden className="invisible col-start-1 row-start-1">Creating...</span>
                  <span aria-live="polite" className="col-start-1 row-start-1">
                    {createBusy ? (
                      createBusyLabel
                    ) : (
                      <>
                        New<span className="hidden min-[390px]:inline"> session</span>
                      </>
                    )}
                  </span>
                </span>
              </Button>
              <Button
                ref={startAnchorRef}
                variant="contrast"
                pressEffect="none"
                className="min-h-6 border-l-dialog-title-foreground/30 px-2 py-0.5 font-mono text-chip sm:min-h-6"
                disabled={createBusy || !active}
                aria-haspopup="menu"
                aria-expanded={startMenu !== null}
                aria-label="Choose where the new session starts"
                title="Start in a draft"
                onClick={() => (startMenu ? closeStartMenu() : openStartMenu())}
              >
                <span aria-hidden>▾</span>
              </Button>
            </div>
          </div>
          {createError && (
            <div className="mt-2">
              <Banner kind="err">{createError}</Banner>
            </div>
          )}
        </div>

        <div className="flex min-h-10 items-center border-y border-dialog-edge bg-panel px-3 sm:min-h-9 sm:px-4">
          <span className="shrink-0 font-mono text-ui text-accent-ink">›</span>
          <input
            value={query}
            onChange={(event) => setQuery(event.target.value)}
            className="min-w-0 flex-1 bg-transparent px-2 py-2 font-mono text-ui text-white outline-none placeholder:text-dialog-hint"
            placeholder="Filter title, project, session"
            aria-label="Filter sessions"
          />
        </div>

        <div ref={listRef} className="min-h-0 flex-1 touch-pan-y overflow-x-hidden overflow-y-auto overscroll-contain [overflow-anchor:auto] [scrollbar-gutter:stable]">
        {sessions === null ? (
          <NavigatorSkeleton />
        ) : visible?.length === 0 ? (
          <div className="px-5 py-16 text-center">
            <p className="font-mono text-body font-bold text-white/70">
              {query ? 'No matching sessions' : 'No sessions yet'}
            </p>
            <p className="mt-2 font-mono text-ui text-dialog-hint">
              {query ? 'Clear the filter to see all sessions.' : 'Use New session to get started.'}
            </p>
          </div>
        ) : (
          <div className="border-t border-dialog-edge">
            {groups.map(([project, projectSessions]) => (
            <ProjectGroup
              key={project}
              project={project}
              sessions={projectSessions}
              conn={active!}
              subscribedIds={subscribedIds}
              matches={matches}
              needle={deferredQuery.trim()}
              onOpen={onOpen}
              onRename={startRename}
              onDelete={startDelete}
              expanded={expanded.has(project)}
              forceExpand={forceExpand}
              onToggle={toggleProject}
              pageSize={pageSize}
            />
          ))}
          </div>
        )}
        </div>

        <footer className="hidden items-center justify-end border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-meta text-dialog-hint sm:flex sm:px-4">
          <span>{sessions ? `${totals.shown} of ${totals.all} sessions` : 'Reading sessions...'}</span>
        </footer>
      </div>

      {rowAction && createPortal(
        <div
          className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 p-[max(1rem,env(safe-area-inset-top))] pb-[max(1rem,env(safe-area-inset-bottom))] pl-[max(1rem,env(safe-area-inset-left))] pr-[max(1rem,env(safe-area-inset-right))]"
          role="presentation"
          onClick={closeRowAction}
        >
          <div
            className="w-full max-w-md"
            role="presentation"
            onClick={(event) => event.stopPropagation()}
          >
            <DialogFrame
              title={rowAction.mode === 'rename' ? 'Rename session' : 'Delete session'}
              onClose={closeRowAction}
            >
              <div className="space-y-3 p-4">
                <p className="truncate font-mono text-meta text-dialog-hint">
                  {rowAction.session.title?.trim() || 'Untitled session'} ·{' '}
                  {shortId(rowAction.session.id)}
                </p>
                {rowAction.mode === 'rename' ? (
                  <label className="block">
                    <span className="mb-1 block font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
                      Session name
                    </span>
                    <Input
                      autoFocus
                      value={renameDraft}
                      maxLength={200}
                      placeholder="Session name"
                      onChange={(event) => setRenameDraft(event.target.value)}
                      onKeyDown={(event) => {
                        if (event.key === 'Enter') void commitRowAction();
                      }}
                    />
                  </label>
                ) : (
                  <p className="font-mono text-body text-white">
                    Delete this session and its transcript from the gateway? This cannot be undone.
                  </p>
                )}
                {actionError && <Banner kind="err">{actionError}</Banner>}
                <div className="flex justify-end gap-2">
                  <Button variant="ghost" disabled={actionBusy} onClick={closeRowAction}>
                    Cancel
                  </Button>
                  <Button
                    variant={rowAction.mode === 'delete' ? 'danger' : 'solid'}
                    disabled={actionBusy}
                    onClick={() => void commitRowAction()}
                  >
                    {actionBusy
                      ? rowAction.mode === 'rename'
                        ? 'Saving...'
                        : 'Deleting...'
                      : rowAction.mode === 'rename'
                        ? 'Save'
                        : 'Delete'}
                  </Button>
                </div>
              </div>
            </DialogFrame>
          </div>
        </div>,
        document.body,
      )}

      {startMenu && createPortal(
        <div
          className="fixed inset-0 z-50 bg-black/40 sm:bg-transparent"
          role="presentation"
          onClick={() => closeStartMenu(true)}
        >
          {/* Phones get a bottom sheet (thumb-reachable, full width, safe-area aware);
              from `sm` up it becomes a popover pinned under the caret it came from. */}
          <div
            role="menu"
            aria-label="Start the new session in"
            className="absolute inset-x-0 bottom-0 max-h-[70vh] touch-pan-y overflow-y-auto overscroll-contain border-t border-dialog-edge bg-panel pb-[env(safe-area-inset-bottom)] transition-[opacity,transform,translate,scale,rotate] duration-150 starting:translate-y-2 starting:opacity-0 motion-reduce:transition-none sm:inset-x-auto sm:bottom-auto sm:left-[var(--menu-left)] sm:top-[var(--menu-top)] sm:w-80 sm:border sm:pb-0 sm:shadow-[8px_8px_0_var(--dialog-shadow)]"
            style={{ '--menu-top': `${startMenu.top}px`, '--menu-left': `${startMenu.left}px` } as CSSProperties}
            onClick={(event) => event.stopPropagation()}
          >
            <p className="border-b border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
              Start the session in
            </p>
            <StartOption
              title="The project itself"
              hint="Edits land straight in the repo — no isolated copy."
              badge="Default"
              onSelect={() => void createSession()}
            />
            <StartOption
              title="A new draft"
              hint="A private copy of this project exactly as it is now — your uncommitted changes come with it. The real project stays untouched."
              onSelect={() => askDraftName(false)}
            />
            <StartOption
              title="A new draft, without my uncommitted changes"
              hint="A private copy of this project as of your last commit. Your uncommitted work stays here, in the real project, untouched."
              onSelect={() => askDraftName(true)}
            />
            <div className="flex items-baseline justify-between gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2">
              <span className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
                Or a draft you parked
              </span>
              {draftRepo && (
                <span className="truncate font-mono text-chip text-dialog-hint/70">{draftRepo}</span>
              )}
            </div>
            {drafts === null ? (
              <p className="flex items-center gap-2 px-3 py-3 font-mono text-meta text-dialog-hint">
                <Spinner className="text-accent-ink" />
                Reading drafts...
              </p>
            ) : drafts.length === 0 ? (
              <p className="px-3 py-3 font-mono text-meta text-dialog-hint">
                {draftsError ?? 'No drafts parked in this project yet.'}
              </p>
            ) : (
              drafts.map((draft) => (
                <StartOption
                  key={draft.workspace_id}
                  title={draft.label?.trim() || shortId(draft.workspace_id)}
                  hint={draftHint(draft)}
                  badge={draft.is_current ? 'in use' : undefined}
                  onSelect={() => void createSession({ kind: 'resume', draft })}
                />
              ))
            )}
          </div>
        </div>,
        document.body,
      )}

      {namePrompt && createPortal(
        <div
          className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 p-[max(1rem,env(safe-area-inset-top))] pb-[max(1rem,env(safe-area-inset-bottom))] pl-[max(1rem,env(safe-area-inset-left))] pr-[max(1rem,env(safe-area-inset-right))]"
          role="presentation"
          onClick={() => setNamePrompt(null)}
        >
          <div
            className="w-full max-w-md"
            role="presentation"
            onClick={(event) => event.stopPropagation()}
          >
            <DialogFrame
              title={namePrompt.clean ? 'Name the clean draft' : 'Name the draft'}
              onClose={() => setNamePrompt(null)}
            >
              <div className="space-y-3 p-4">
                <p className="font-mono text-meta text-dialog-hint">
                  {namePrompt.clean
                    ? 'A private copy of this project as of your last commit — your uncommitted changes stay here and are not copied in. Applying it later is what moves the work back.'
                    : 'A private copy of this project exactly as it is now, uncommitted changes included. Applying it later is what moves the work back.'}
                </p>
                <label className="block">
                  <span className="mb-1 block font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
                    Draft name
                  </span>
                  <Input
                    autoFocus
                    value={draftLabel}
                    maxLength={80}
                    placeholder="wire-rework"
                    onChange={(event) => setDraftLabel(event.target.value)}
                    onKeyDown={(event) => {
                      if (event.key === 'Enter') commitDraftName();
                    }}
                  />
                </label>
                <div className="flex justify-end gap-2">
                  <Button variant="ghost" onClick={() => setNamePrompt(null)}>
                    Cancel
                  </Button>
                  <Button
                    variant="solid"
                    disabled={!draftLabel.trim()}
                    onClick={commitDraftName}
                  >
                    Create
                  </Button>
                </div>
              </div>
            </DialogFrame>
          </div>
        </div>,
        document.body,
      )}
    </section>
  );
}

// Memoised: a 5.5s poll that changes nothing returns the SAME row objects
// (`reconcileSessions`), so an unchanged group must not re-render its rows.
const ProjectGroup = memo(function ProjectGroup({
  project,
  sessions,
  conn,
  subscribedIds,
  matches,
  needle,
  onOpen,
  onRename,
  onDelete,
  expanded,
  forceExpand,
  onToggle,
  pageSize,
}: {
  project: string;
  sessions: Session[];
  conn: GatewayConn;
  subscribedIds: ReadonlySet<string>;
  matches: Map<string, SessionMatch> | null;
  needle: string;
  onOpen: Props['onOpen'];
  onRename: (session: Session) => void;
  onDelete: (session: Session) => void;
  expanded: boolean;
  forceExpand: boolean;
  onToggle: (project: string) => void;
  pageSize: number;
}) {
  const root = projectRoot(sessions);
  const liveSessions = useMemo(() => sessions.filter(sessionIsLive), [sessions]);
  const liveCount = liveSessions.length;
  const isOpen = expanded || forceExpand;

  // A coarse clock ages the recency window: once a session is more than an hour past
  // its last activity it leaves the collapsed peek on its own, no refresh needed.
  const now = useNow(RECENT_WINDOW_MS / 60);
  const collapsedSessions = useMemo(
    () => sessions.filter((session) => sessionFresh(session, now)),
    [sessions, now],
  );

  // Expanded pages its own history in place; collapsing exposes the live few plus any
  // session active within the last hour.
  const [shown, setShown] = useState(pageSize);
  useEffect(() => {
    setShown(pageSize);
  }, [pageSize]);

  const rows = isOpen ? sessions.slice(0, shown) : collapsedSessions.slice(0, pageSize);
  const remaining = isOpen ? Math.max(0, sessions.length - shown) : 0;

  return (
    <section className="border-t border-dialog-edge first:border-t-0" aria-label={`${project} sessions`}>
      <header className="bg-panel-2">
        <button
          type="button"
          onClick={() => onToggle(project)}
          aria-expanded={isOpen}
          className="flex min-h-11 w-full items-center justify-between gap-3 px-3 py-2 text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none sm:px-4"
        >
          <span className="flex min-w-0 items-center gap-2">
            <span className="shrink-0 font-mono text-ui text-dialog-hint" aria-hidden="true">
              {isOpen ? '▾' : '▸'}
            </span>
            <span className="min-w-0">
              <span className="block truncate font-mono text-ui font-bold text-white">{project}</span>
              <span className="mt-0.5 block truncate font-mono text-chip text-dialog-hint" title={root}>
                {root || 'No workspace path'}
              </span>
            </span>
          </span>
          <span className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
            <span>{sessions.length} {sessions.length === 1 ? 'session' : 'sessions'}</span>
            {liveCount > 0 && (
              <>
                <span className="opacity-40" aria-hidden="true">·</span>
                <span className="inline-flex items-center gap-1 font-bold text-ok">
                  <span className="size-1.5 animate-pulse bg-ok motion-reduce:animate-none" />
                  {liveCount} live
                </span>
              </>
            )}
          </span>
        </button>
      </header>
      {rows.length > 0 && (
        <div className="border-t border-dialog-edge">
          {rows.map((session) => (
            <SessionRow
              key={session.id}
              session={session}
              conn={conn}
              subscribed={subscribedIds.has(session.id)}
              match={matches?.get(session.id) ?? null}
              needle={needle}
              onOpen={onOpen}
              onRename={onRename}
              onDelete={onDelete}
            />
          ))}
          {remaining > 0 && (
            <button
              type="button"
              onClick={() => setShown((current) => current + pageSize)}
              className="flex w-full items-center justify-center gap-2 border-t border-dialog-edge px-3 py-2.5 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint transition-colors duration-150 hover:bg-hover hover:text-white focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none sm:px-4"
            >
              Show {remaining} more
            </button>
          )}
        </div>
      )}
    </section>
  );
});

// How long the row's disclosure takes to open or close. It is duplicated by the
// `duration-200` utilities below on purpose: the class drives the paint, this
// number only decides when the panel may leave the tree.
const STATS_MOTION_MS = 200;

const SessionRow = memo(function SessionRow({
  session,
  conn,
  subscribed,
  match,
  needle,
  onOpen,
  onRename,
  onDelete,
}: {
  session: Session;
  conn: GatewayConn;
  subscribed: boolean;
  match: SessionMatch | null;
  needle: string;
  onOpen: Props['onOpen'];
  onRename: (session: Session) => void;
  onDelete: (session: Session) => void;
}) {
  const status = statusLabel(session);
  const timestamp = session.modified_at ?? session.last_active_at ?? session.created_at;
  const title = session.title?.trim() || 'Untitled session';
  const live = sessionIsLive(session);
  const turns = Number(session.turn_count ?? 0);
  // Turns that finished while this session was closed: the one thing a relative
  // timestamp cannot announce.
  const unread = unreadTurnCount(session);
  // The left chevron is a real DISCLOSURE, not decoration: it opens this
  // session's usage rollup in place. It stays a sibling of the open-session
  // button, never nested inside it, so "tell me more" cannot navigate away.
  const [statsOpen, setStatsOpen] = useState(false);
  // The rollup FETCHES on mount and aborts on unmount, so it cannot simply stay
  // mounted while closed. Dropping it on the same commit that closes the row
  // would pull the content out from under the collapse, so it outlives
  // `statsOpen` by exactly one transition and is only then let go.
  const [statsMounted, setStatsMounted] = useState(false);
  useEffect(() => {
    if (statsOpen || !statsMounted) return;
    const timer = window.setTimeout(() => setStatsMounted(false), STATS_MOTION_MS);
    return () => window.clearTimeout(timer);
  }, [statsOpen, statsMounted]);
  // Mount and open in ONE commit. The grid wrapper never leaves the tree, so
  // 0fr -> 1fr is a transition on a persistent element — no `@starting-style`,
  // which WebKit applies a frame late to freshly inserted nodes.
  const toggleStats = useCallback(() => {
    setStatsMounted(true);
    setStatsOpen((open) => !open);
  }, []);
  // A draft is a per-session clone of the project; the row says so instead of
  // the list inventing a project for it.
  const draftName = isDraftWorkspace(session) ? session.workspace?.label?.trim() : '';

  return (
    <div className="[&+&]:border-t [&+&]:border-dialog-edge">
      <SwipeActions
        label={title}
        actions={[
          {
            key: 'rename',
            label: 'Rename',
            icon: <PencilIcon />,
            onSelect: () => onRename(session),
          },
          {
            key: 'delete',
            label: 'Delete',
            icon: <TrashIcon />,
            tone: 'danger',
            onSelect: () => onDelete(session),
          },
        ]}
      >
      <div className="flex items-stretch">
        <button
          type="button"
          aria-expanded={statsOpen}
          aria-label={`${statsOpen ? 'Hide' : 'Show'} details for ${title}`}
          onClick={toggleStats}
          className={`flex w-8 shrink-0 items-start justify-center pt-2.5 font-mono text-body text-accent-ink transition-[background-color,opacity] duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none sm:w-9 sm:pt-2 ${
            statsOpen ? 'bg-hover opacity-100' : 'opacity-40 hover:opacity-100'
          }`}
        >
          {/* One glyph that TURNS, not two glyphs that swap: the quarter turn is
              the same gesture as the panel below it, and a swap has no motion to
              read at all. */}
          <span
            aria-hidden="true"
            className={`inline-block transition-[rotate] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
              statsOpen ? 'rotate-90' : 'rotate-0'
            }`}
          >
            {'\u203a'}
          </span>
        </button>
        <button
          type="button"
          className="group flex min-h-14 min-w-0 flex-1 items-start py-2.5 pl-2 pr-3 text-left transition-colors duration-150 hover:bg-hover active:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none sm:min-h-12 sm:py-2 sm:pr-4"
          data-session-id={session.id}
          onClick={() => void onOpen(conn, session.id)}
        >
        <span className="min-w-0 flex-1">
          <span className="flex min-w-0 items-start justify-between gap-3">
            <span
              className={`block min-w-0 truncate font-mono text-ui font-semibold ${
                session.title?.trim() ? 'text-white' : 'text-white/45'
              }`}
            >
              {title}
            </span>
          <span className="flex shrink-0 items-center gap-1.5">
            {unread > 0 && (
              <span className="inline-flex items-center bg-accent px-1 font-mono text-chip font-bold uppercase tracking-[0.08em] text-accent-foreground">
                {unread > 1 ? `${unread} new` : 'new'}
              </span>
            )}
            <span className={`inline-flex shrink-0 items-center gap-1 font-mono text-chip font-bold tracking-[0.08em] ${statusTone(session)}`}>
              <span className={`size-1.5 ${statusDot(session)} ${live ? 'animate-pulse motion-reduce:animate-none' : ''}`} />
              {status}
            </span>
          </span>
          </span>
          <span className="mt-1 flex flex-wrap items-center gap-x-2 gap-y-1 font-mono text-chip text-dialog-hint">
            <span className="text-white/55">{shortId(session.id)}</span>
            <span className="opacity-40" aria-hidden="true">·</span>
            <span>{turns} {turns === 1 ? 'turn' : 'turns'}</span>
            {draftName !== '' && (
              <>
                <span className="opacity-40" aria-hidden="true">·</span>
                <span
                  className="inline-flex items-center font-bold uppercase tracking-[0.08em] text-warn-strong"
                  title={session.workspace?.root}
                >
                  draft {draftName || ''}
                </span>
              </>
            )}
            {subscribed && (
              <>
                <span className="opacity-40" aria-hidden="true">·</span>
                <span className="inline-flex items-center gap-1 font-bold text-accent-ink">
                  <span className="size-1 bg-accent-ink" /> OPEN
                </span>
              </>
            )}
            <span className="ml-auto shrink-0 pl-2" title={formatExact(timestamp)}>{relativeTime(timestamp)}</span>
          </span>
        </span>
        </button>
      </div>
      </SwipeActions>
      {/* Height eases through a 0fr -> 1fr grid track: the one pure-CSS way to
          animate to CONTENT height without measuring it, and unlike a mount it
          plays in BOTH directions. The inner clip keeps the rollup from
          spilling over the next row while the track is still closing. */}
      <div
        aria-hidden={!statsOpen}
        className={`grid transition-[grid-template-rows] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
          statsOpen ? 'grid-rows-[1fr]' : 'grid-rows-[0fr]'
        }`}
      >
        <div className="overflow-hidden">
          <div
            className={`transition-[opacity,translate] duration-200 ease-[cubic-bezier(0.22,0.61,0.36,1)] motion-reduce:transition-none ${
              statsOpen ? 'opacity-100' : '-translate-y-1 opacity-0'
            }`}
          >
            {statsMounted && <SessionStats session={session} conn={conn} />}
          </div>
        </div>
      </div>
      {match && <MatchPreview match={match} needle={needle} />}
    </div>
  );
});

// The expanded half of a session row: everything the list cannot afford to
// carry for every session at once. It is fetched HERE only when the row opens,
// and aborted if the row closes first; the gateway memoizes decoded tool tallies.
function SessionStats({ session, conn }: { session: Session; conn: GatewayConn }) {
  const [usage, setUsage] = useState<SessionUsage | null>(null);
  const [phase, setPhase] = useState<'loading' | 'ready' | 'error'>('loading');

  useEffect(() => {
    const controller = new AbortController();
    setPhase('loading');
    new GatewayClient(conn)
      .sessionUsage(session.id, controller.signal)
      .then((next) => {
        if (controller.signal.aborted) return;
        setUsage(next);
        setPhase('ready');
      })
      .catch(() => {
        if (!controller.signal.aborted) setPhase('error');
      });
    return () => controller.abort();
  }, [conn, session.id]);

  const cacheHit = usage?.cache_hit_rate;
  const tools = usage?.top_tools ?? [];
  const errors = usage?.top_errors ?? [];

  return (
    <div className="border-t border-dialog-edge bg-panel-2 py-2.5 pl-10 pr-3 sm:pl-11 sm:pr-4">
      {phase === 'loading' && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          Reading usage…
        </p>
      )}
      {phase === 'error' && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-warn-strong">
          Usage unavailable
        </p>
      )}
      {phase === 'ready' && !usage && (
        <p className="font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          No turns yet
        </p>
      )}
      {phase === 'ready' && usage && (
        <>
          <dl className="grid grid-cols-4 gap-x-3 gap-y-2">
            <Stat label="Turns" value={compactCount(usage.turn_count)} />
            <Stat label="Iters" value={compactCount(usage.iteration_count)} />
            <Stat label="Tools" value={compactCount(usage.tool_call_count)} />
            <Stat label="Folds" value={compactCount(usage.fold_count)} />
            <Stat label="In" value={compactCount(usage.input_tokens)} />
            <Stat label="Out" value={compactCount(usage.output_tokens)} />
            <Stat
              label="Cache"
              value={typeof cacheHit === 'number' ? `${Math.round(cacheHit * 100)}%` : '—'}
            />
            <Stat label="Cost" value={formatUsd(usage.cost_usd)} />
          </dl>
          <dl className="mt-2.5 flex flex-wrap items-baseline gap-x-3 gap-y-1 border-t border-dialog-edge/40 pt-2">
            {/* `/usage` names the model the session actually RAN on, but only
                once a turn has finished stamping it — a live session's newest
                turn has none. The pin (list row) and the state's root model are
                the standing answers, so fall back to those instead of a dash. */}
            <Meta
              label="Model"
              value={usage.model || session.model_pref?.model || session.model || '—'}
              title={usage.provider || session.model_pref?.provider}
            />
            <Meta
              label="Active"
              value={formatDuration(usage.duration_ms)}
              title="Time spent inside turns"
            />
            {tools.length > 0 && (
              <Meta
                label="Top tools"
                value={tools
                  .slice(0, 3)
                  .map((tool) => `${tool.name} ${tool.count}`)
                  .join(' · ')}
                title={tools.map((tool) => `${tool.name} ${tool.count}`).join(' · ')}
              />
            )}
            {/* Volume alone hides where a session actually struggled, so failed
                calls get their own labelled pair instead of disappearing into
                the TOP TOOLS totals. */}
            {errors.length > 0 && (
              <Meta
                label="Top errors"
                value={errors
                  .slice(0, 3)
                  .map((tool) => `${tool.name} ${tool.count}`)
                  .join(' · ')}
                title={`${usage.error_count ?? 0} failed tool calls · ${errors
                  .map((tool) => `${tool.name} ${tool.count}`)
                  .join(' · ')}`}
                tone="warn"
              />
            )}
          </dl>
        </>
      )}
    </div>
  );
}

function Stat({ label, value }: { label: string; value: string }) {
  return (
    <div className="min-w-0">
      <dt className="truncate font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </dt>
      <dd className="truncate font-mono text-meta font-bold tabular-nums text-white">{value}</dd>
    </div>
  );
}

// The grid above answers "how much"; this row answers "of what, for how long".
// It reuses the grid's dim-key/strong-value grammar so the three facts read as
// labelled data instead of one faint unlabelled sentence.
function Meta({
  label,
  value,
  title,
  tone = 'default',
}: {
  label: string;
  value: string;
  title?: string;
  tone?: 'default' | 'warn';
}) {
  return (
    <div className="flex min-w-0 items-baseline gap-1.5" title={title}>
      <dt className="shrink-0 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
        {label}
      </dt>
      <dd
        className={`min-w-0 truncate font-mono text-meta font-bold ${
          tone === 'warn' ? 'text-warn-strong' : 'text-white'
        }`}
      >
        {value}
      </dd>
    </div>
  );
}

function compactCount(value?: number): string {
  const n = Number(value ?? 0);
  if (!Number.isFinite(n)) return '—';
  if (n < 1_000) return String(n);
  if (n < 1_000_000) return `${(n / 1_000).toFixed(n < 10_000 ? 1 : 0)}k`;
  if (n < 1_000_000_000) return `${(n / 1_000_000).toFixed(n < 10_000_000 ? 1 : 0)}M`;
  return `${(n / 1_000_000_000).toFixed(1)}B`;
}

// Sub-cent totals must not read as "$0.00" — a session that cost something is
// never free.
function formatUsd(value?: number): string {
  const n = Number(value ?? 0);
  if (!Number.isFinite(n) || n <= 0) return '$0';
  if (n < 0.01) return '<$0.01';
  if (n < 1_000) return `$${n.toFixed(2)}`;
  return `$${Math.round(n).toLocaleString()}`;
}

function formatDuration(value?: number): string {
  const ms = Number(value ?? 0);
  if (!Number.isFinite(ms) || ms <= 0) return '0s';
  const seconds = Math.round(ms / 1_000);
  if (seconds < 60) return `${seconds}s`;
  const minutes = seconds / 60;
  if (minutes < 60) return `${Math.round(minutes)}m`;
  const hours = minutes / 60;
  return hours < 24 ? `${hours.toFixed(1)}h` : `${(hours / 24).toFixed(1)}d`;
}

// The list has nothing to paint yet. Two rules keep this honest:
//
// 1. Colour: the previous skeleton was drawn in panel tints — and `--panel2`
//    EQUALS `--surface` in the shipped themes (light: both #faf3eb), so it
//    rendered as invisible boxes. Bars use `--color-muted`, a mid grey that
//    separates from every gateway surface, and the Braille spinner says it in
//    words — the same one the transcript and the TUI use.
// 2. GEOMETRY: a placeholder that is not the exact height of the thing it
//    stands for makes the whole list jump when data lands. So the skeleton
//    mirrors `ProjectGroup`'s header and `SessionRow` class for class
//    (`min-h-11` / `min-h-14 sm:min-h-12`, same padding, same `mt-*`), and each
//    bar is centred inside an INVISIBLE glyph of the real type step. That sizer
//    is what makes the line box identical — a bare `h-2` bar is 8px where a
//    `text-ui` line is not, and three such rows per group is a visible lurch.
//    Measured in a browser at 390/768/1200px: header 46px and rows 48/49/49px
//    on both sides, group total 193px.
// 3. NOTHING EXTRA IN THE FLOW. The skeleton used to lead with its own
//    "Loading sessions…" strip — 37px plus a hairline that the loaded list does
//    not have, so every group below it slid up the moment data arrived. That
//    was the only remaining mismatch. The strip is gone; the panel header above
//    already says "Reading sessions..." in a line that exists in BOTH states, so
//    the signal costs zero pixels. The skeleton root carries the `border-t` the
//    loaded list's wrapper carries, so the top hairline matches too.
function SkeletonBar({
  type,
  width,
  bar,
  tone,
}: {
  type: string;
  width: string;
  bar: string;
  tone: string;
}) {
  return (
    <span className={`grid ${width}`}>
      <span className={`col-start-1 row-start-1 invisible font-mono ${type}`}>&nbsp;</span>
      <span className={`col-start-1 row-start-1 self-center ${bar} ${tone}`} />
    </span>
  );
}

function NavigatorSkeleton() {
  return (
    <div
      role="status"
      aria-live="polite"
      aria-label="Loading sessions"
      className="border-t border-dialog-edge"
    >
      <div className="animate-pulse motion-reduce:animate-none" aria-hidden="true">
        {SKELETON_GROUPS.map((rows, group) => (
          <div key={group} className="border-t border-dialog-edge first:border-t-0">
            {/* mirrors ProjectGroup's <header> */}
            <div className="flex min-h-11 items-center justify-between gap-3 bg-panel-2 px-3 py-2 sm:px-4">
              <div className="min-w-0">
                <SkeletonBar type="text-ui" width="w-28" bar="h-2.5" tone="bg-muted/40" />
                <div className="mt-0.5">
                  <SkeletonBar type="text-chip" width="w-40" bar="h-1.5" tone="bg-muted/20" />
                </div>
              </div>
              <div className="shrink-0">
                <SkeletonBar type="text-chip" width="w-14" bar="h-1.5" tone="bg-muted/25" />
              </div>
            </div>
            {/* mirrors SessionRow's <button> */}
            <div className="border-t border-dialog-edge">
              {rows.map((width, row) => (
                <div
                  key={row}
                  className="flex min-h-14 w-full items-start gap-2 px-3 py-2.5 [&+&]:border-t [&+&]:border-dialog-edge sm:min-h-12 sm:px-4 sm:py-2"
                >
                  <span className="mt-0.5 invisible shrink-0 font-mono text-body">›</span>
                  <span className="min-w-0 flex-1">
                    <span className="flex min-w-0 items-start justify-between gap-3">
                      <SkeletonBar type="text-ui" width={width} bar="h-2.5" tone="bg-muted/30" />
                      <span className="shrink-0">
                        <SkeletonBar type="text-chip" width="w-12" bar="h-1.5" tone="bg-muted/25" />
                      </span>
                    </span>
                    <span className="mt-1 flex items-center gap-x-2 font-mono text-chip">
                      <SkeletonBar type="text-chip" width="w-10" bar="h-1.5" tone="bg-muted/20" />
                      <SkeletonBar type="text-chip" width="w-14" bar="h-1.5" tone="bg-muted/20" />
                      <span className="ml-auto shrink-0 pl-2">
                        <SkeletonBar type="text-chip" width="w-12" bar="h-1.5" tone="bg-muted/20" />
                      </span>
                    </span>
                  </span>
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

function visibleListAnchor(viewport: HTMLDivElement | null): { id: string; top: number } | null {
  if (!viewport || viewport.scrollTop <= 2) return null;
  const viewportTop = viewport.getBoundingClientRect().top;
  const row = Array.from(viewport.querySelectorAll<HTMLElement>('[data-session-id]'))
    .find((element) => element.getBoundingClientRect().bottom > viewportTop);
  return row?.dataset.sessionId ? { id: row.dataset.sessionId, top: row.getBoundingClientRect().top } : null;
}

function sessionViewFingerprint(session: Session): string {
  return JSON.stringify([
    session.id,
    session.title,
    session.status,
    session.live,
    session.current_turn_id,
    session.turn_count,
    session.modified_at,
    session.last_active_at,
    session.created_at,
    session.project_id,
    session.project_name,
    session.project_position,
    session.workspace?.root,
    session.workspace?.repo_root,
    session.workspace?.label,
  ]);
}

function reconcileSessions(current: Session[] | null, incoming: Session[]): Session[] {
  if (!current) return incoming;
  const previousById = new Map(current.map((session) => [session.id, session]));
  const next = incoming.map((session) => {
    const previous = previousById.get(session.id);
    return previous && sessionViewFingerprint(previous) === sessionViewFingerprint(session)
      ? previous
      : session;
  });
  return current.length === next.length && current.every((session, index) => session === next[index])
    ? current
    : next;
}

function shortId(id: string): string {
  return id.split('-')[0]?.slice(0, 8) || id.slice(0, 8);
}

function projectLabel(session: Session): string {
  // NEVER `workspace.label` for a draft: that is the DRAFT's name, and using it
  // as the grouping key gave every draft its own bogus top-level "project".
  const named =
    session.project_name?.trim() ||
    (isDraftWorkspace(session) ? '' : session.workspace?.label?.trim());
  if (named) return homeifyPath(named);
  const root = projectPath(session);
  if (root) return root.split('/').pop() || homeifyPath(root);
  return 'No project';
}

function projectRoot(sessions: Session[]): string {
  return homeifyPath(sessions.map(projectPath).find(Boolean));
}

// A DRAFT is a per-session clone parked at ~/.vis/drafts/<repo>/<label>; it is
// a workspace of the session, never a project of its own. `is_draft` is the
// gateway fact (list rows carry it); the path shape is the fallback for a
// gateway older than the flag, so an out-of-date daemon does not resurrect the
// one-project-per-draft bug.
const DRAFT_ROOT = /(^|\/)\.vis\/drafts\//;

function isDraftWorkspace(session: Session): boolean {
  const workspace = session.workspace;
  if (!workspace) return false;
  if (typeof workspace.is_draft === 'boolean') return workspace.is_draft;
  return DRAFT_ROOT.test(workspace.root ?? '');
}

// The path a session GROUPS under: the repo it belongs to, which for a draft is
// `repo_root` and not the clone it happens to be checked out in.
function projectPath(session: Session): string {
  const workspace = session.workspace;
  if (!workspace) return '';
  const path = isDraftWorkspace(session)
    ? workspace.repo_root || workspace.root
    : workspace.root || workspace.repo_root;
  return path?.replace(/\/+$/, '') || '';
}

function sessionIsLive(session: Session): boolean {
  return session.live ?? session.status === 'running';
}

function sessionFresh(session: Session, now: number): boolean {
  // Live sessions are always shown collapsed; a session that STOPPED being live still
  // lingers for one hour so it doesn't vanish the instant it goes idle. Uses the same
  // timestamp chain the row displays, so "fresh" == "its clock reads < 1h ago".
  if (sessionIsLive(session)) return true;
  const millis = dateMillis(session.modified_at ?? session.last_active_at ?? session.created_at);
  return millis > 0 && now - millis <= RECENT_WINDOW_MS;
}

function statusLabel(session: Session): string {
  if (sessionIsLive(session)) return 'LIVE';
  if (session.status === 'suspended') return 'WAITING';
  return 'IDLE';
}

function statusTone(session: Session): string {
  if (sessionIsLive(session)) return 'text-ok';
  if (session.status === 'suspended') return 'text-warn-strong';
  return 'text-dialog-hint';
}

function statusDot(session: Session): string {
  if (sessionIsLive(session)) return 'animate-pulse bg-ok motion-reduce:animate-none';
  if (session.status === 'suspended') return 'bg-warn-strong';
  return 'border border-dialog-hint';
}

function emptyUntitled(session: Session): boolean {
  return (
    !session.title?.trim() &&
    Number(session.turn_count ?? 0) === 0 &&
    !sessionIsLive(session)
  );
}

function sessionSearchText(session: Session): string {
  return [
    session.title,
    session.id,
    session.project_name,
    session.workspace?.label,
    session.workspace?.root,
    session.status,
    sessionIsLive(session) ? 'live running' : 'idle',
  ]
    .filter(Boolean)
    .join(' ')
    .toLowerCase();
}

function groupByProject(sessions: Session[]): Array<[string, Session[]]> {
  const groups = new Map<string, Session[]>();
  for (const session of sessions) {
    const key = projectLabel(session);
    const group = groups.get(key) ?? [];
    group.push(session);
    groups.set(key, group);
  }

  // Map insertion order preserves the gateway's canonical live-first order.
  return [...groups.entries()];
}

function dateMillis(value?: string): number {
  if (!value) return 0;
  const millis = new Date(value).getTime();
  return Number.isFinite(millis) ? millis : 0;
}

function relativeTime(value?: string): string {
  const millis = dateMillis(value);
  if (!millis) return '-';
  const seconds = Math.round((millis - Date.now()) / 1000);
  const absolute = Math.abs(seconds);
  const formatter = new Intl.RelativeTimeFormat(undefined, { numeric: 'auto' });
  if (absolute < 60) return formatter.format(seconds, 'second');
  if (absolute < 3_600) return formatter.format(Math.round(seconds / 60), 'minute');
  if (absolute < 86_400) return formatter.format(Math.round(seconds / 3_600), 'hour');
  if (absolute < 604_800) return formatter.format(Math.round(seconds / 86_400), 'day');
  return new Intl.DateTimeFormat(undefined, { month: 'short', day: 'numeric' }).format(millis);
}

function formatExact(value?: string): string {
  const millis = dateMillis(value);
  return millis ? new Date(millis).toLocaleString() : '';
}

// Search hits stay subordinate to their session: compact transcript rows, not cards.
function MatchPreview({ match, needle }: { match: SessionMatch; needle: string }) {
  const rows =
    match.hits.length > 0
      ? match.hits
      : [
          { side: 'request' as const, snippet: match.requestSnippet?.trim() ?? '', at: null },
          { side: 'reply' as const, snippet: match.replySnippet?.trim() ?? '', at: null },
        ].filter((h) => h.snippet.length > 0);
  if (rows.length === 0) return null;
  return (
    <div className="border-t border-dialog-edge bg-ink/30 py-1.5 pl-10 pr-3 sm:pl-11 sm:pr-4">
      <div className="divide-y divide-dialog-edge/70">
        {rows.map((hit, index) => (
          <div
            key={`${hit.side}-${hit.at ?? index}`}
            className="grid grid-cols-[2.5rem_minmax(0,1fr)] gap-2 py-1.5"
          >
            <span
              className={`font-mono text-meta font-bold ${
                hit.side === 'request' ? 'text-you-role' : 'text-vis-role'
              }`}
            >
              {hit.side === 'request' ? 'You' : 'Vis'}
            </span>
            <p className="line-clamp-2 whitespace-pre-wrap break-words font-mono text-ui text-dialog-foreground">
              {highlightNeedle(hit.snippet, needle)}
            </p>
          </div>
        ))}
      </div>
    </div>
  );
}

// Splits `text` on the (case-insensitive) needle and wraps each hit in a
// contrast <mark> that reads on both rail colors.
function highlightNeedle(text: string, needle: string) {
  if (!needle) return text;
  const parts = text.split(new RegExp(`(${escapeRegExp(needle)})`, 'ig'));
  return parts.map((part, index) =>
    part.toLowerCase() === needle.toLowerCase() && part.length > 0 ? (
      <mark key={index} className="rounded-[2px] bg-accent/30 px-0.5 font-bold text-white">
        {part}
      </mark>
    ) : (
      <span key={index}>{part}</span>
    ),
  );
}

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// One row of the start-in menu. Title carries the choice, hint carries the
// consequence — a workspace decision is unrecoverable-ish once the agent starts
// writing, so no row is allowed to be a bare noun. `min-h-11` keeps every row a
// real thumb target on a phone sheet.
function StartOption({
  title,
  hint,
  badge,
  onSelect,
}: {
  title: string;
  hint: string;
  badge?: string;
  onSelect: () => void;
}) {
  return (
    <button
      type="button"
      role="menuitem"
      className="flex min-h-11 w-full items-start gap-2 border-b border-dialog-edge px-3 py-2 text-left transition-colors duration-150 hover:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none"
      onClick={onSelect}
    >
      <span className="min-w-0 flex-1">
        <span className="block truncate font-mono text-ui font-bold text-white">{title}</span>
        <span className="mt-0.5 block font-mono text-meta text-dialog-hint">{hint}</span>
      </span>
      {badge && (
        <span className="mt-0.5 shrink-0 border border-edge px-1 font-mono text-chip uppercase tracking-[0.08em] text-dialog-hint">
          {badge}
        </span>
      )}
    </button>
  );
}

// A parked draft says WHEN it forked, because that — not its name — is what tells
// you whether it still matches the project. Drafts with no fork time (blank ones)
// name their clone instead of inventing a date.
function draftHint(draft: WorkspaceDraft): string {
  const forked = draft.fork_ms
    ? relativeTime(new Date(draft.fork_ms).toISOString())
    : '';
  return forked ? `forked ${forked}` : homeifyPath(draft.root ?? '') || 'isolated workspace';
}

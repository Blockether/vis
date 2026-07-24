import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';
import { Banner, Button } from '../components/ui';
import { GatewayClient } from '../lib/gateway';
import { SessionSubscriptionHub } from '../lib/subscriptions';
import type { GatewayConn, Session } from '../lib/types';
import { homeifyPath } from '../lib/path';

const SESSION_LIST_EVENTS = new Set([
  'turn.started',
  'turn.completed',
  'turn.failed',
  'turn.cancelled',
  'session.title_updated',
]);


interface Props {
  active: GatewayConn | null;
  client: GatewayClient | null;
  subscriptions: SessionSubscriptionHub | null;
  subscribedIds: ReadonlySet<string>;
  gatewayCount: number;
  onOpen: (conn: GatewayConn, sid: string, fresh?: boolean) => void | Promise<void>;
}

export function SessionsScreen({ active, client, subscriptions, subscribedIds, gatewayCount, onOpen }: Props) {
  const [sessions, setSessions] = useState<Session[] | null>(null);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [query, setQuery] = useState('');
  const [transcriptIds, setTranscriptIds] = useState<Set<string> | null>(null);
  const [showEmpty, setShowEmpty] = useState(false);
  const [createBusy, setCreateBusy] = useState(false);
  const [createError, setCreateError] = useState<string | null>(null);
  const pollInFlight = useRef(false);
  const listRef = useRef<HTMLDivElement>(null);
  const refreshAnchorRef = useRef<{ id: string; top: number } | null>(null);
  const activeRef = useRef(active);
  const clientRef = useRef(client);
  activeRef.current = active;
  clientRef.current = client;
  const activeKey = active ? `${active.url}\u0000${active.token ?? ''}` : '';

  const load = useCallback(
    async (signal?: AbortSignal, background = false) => {
      const connection = activeRef.current;
      if (!connection) {
        setSessions([]);
        setLoadError(null);
        return;
      }
      if (background && pollInFlight.current) return;
      if (background) pollInFlight.current = true;
      try {
        const next = await (clientRef.current ?? new GatewayClient(connection)).listSessions(signal);
        if (!signal?.aborted) {
          if (background) refreshAnchorRef.current = visibleListAnchor(listRef.current);
          setSessions((current) => reconcileSessions(current, next));
          setLoadError(null);
        }
      } catch (cause) {
        if (!signal?.aborted && !background) {
          setLoadError((cause as Error).message);
        }
      } finally {
        if (background) pollInFlight.current = false;
      }
    },
    [activeKey],
  );

  useEffect(() => {
    const controller = new AbortController();
    const refreshLiveStates = () => {
      if (document.visibilityState === 'visible') void load(controller.signal, true);
    };

    setLoadError(null);
    if (sessions === null || !active) void load(controller.signal);
    else void load(controller.signal, true);
    const timer = window.setInterval(refreshLiveStates, 5_500);
    document.addEventListener('visibilitychange', refreshLiveStates);
    return () => {
      controller.abort();
      window.clearInterval(timer);
      document.removeEventListener('visibilitychange', refreshLiveStates);
    };
    // A connection identity change should preserve the existing frame until its data arrives.
    // eslint-disable-next-line react-hooks/exhaustive-deps
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
    const needle = query.trim();
    if (!needle) {
      setTranscriptIds(null);
      return;
    }
    const connection = activeRef.current;
    if (!connection) return;
    const controller = new AbortController();
    const timer = window.setTimeout(() => {
      void (clientRef.current ?? new GatewayClient(connection))
        .searchSessionIds(needle, controller.signal)
        .then((ids) => {
          if (!controller.signal.aborted) setTranscriptIds(new Set(ids));
        })
        .catch(() => {
          if (!controller.signal.aborted) setTranscriptIds(null);
        });
    }, 200);
    return () => {
      controller.abort();
      window.clearTimeout(timer);
    };
  }, [query, activeKey]);

  const visible = useMemo(() => {
    if (!sessions) return null;
    const needle = query.trim().toLowerCase();
    return sessions.filter((session) => {
      if (!showEmpty && emptyUntitled(session)) return false;
      return (
        !needle ||
        sessionSearchText(session).includes(needle) ||
        transcriptIds?.has(session.id) === true
      );
    });
  }, [query, sessions, showEmpty, transcriptIds]);

  const totals = useMemo(() => {
    const all = sessions?.length ?? 0;
    const shown = visible?.length ?? 0;
    const hiddenEmpty = sessions?.filter(emptyUntitled).length ?? 0;
    const projects = new Set(sessions?.map(projectLabel) ?? []).size;
    const live = sessions?.filter(sessionIsLive).length ?? 0;
    return { all, shown, hiddenEmpty, projects, live };
  }, [sessions, visible]);

  async function createSession() {
    if (!active) return;
    setCreateBusy(true);
    setCreateError(null);
    try {
      const session = await (client ?? new GatewayClient(active)).createSession({});
      await load();
      if (session.id) await onOpen(active, session.id, true);
    } catch (cause) {
      setCreateError((cause as Error).message);
    } finally {
      setCreateBusy(false);
    }
  }

  const groups = groupByProject(visible ?? []);

  if (loadError) {
    return (
      <section className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col px-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] transition-[opacity,transform] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:px-6 sm:py-6">
        <div className="flex h-full min-h-0 flex-col items-center justify-center gap-4 border-y border-dialog-edge bg-panel px-6 py-16 text-center sm:border">
          <span className="font-mono text-3xl leading-none text-err" aria-hidden="true">○</span>
          <div className="space-y-1.5">
            <p className="font-mono text-sm font-black uppercase tracking-[0.1em] text-err">Gateway offline</p>
            <p className="mx-auto max-w-sm font-mono text-[11px] leading-relaxed text-dialog-hint">
              {gatewayCount > 1
                ? 'This gateway is not responding. Switch to another from the Gateways tab, or retry.'
                : 'The gateway is not responding. Check the URL, your network / Tailscale, or that the gateway is running.'}
            </p>
            <p className="mx-auto max-w-sm break-all font-mono text-[10px] text-dialog-hint/60" title={loadError}>
              {loadError}
            </p>
          </div>
          <Button variant="ghost" className="px-4 py-1.5 text-xs" onClick={() => void load()}>
            Retry
          </Button>
        </div>
      </section>
    );
  }

  return (
    <section className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col pb-0 pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] pt-0 transition-[opacity,transform] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:px-6 sm:pb-6 sm:pt-6">
      <div className="flex h-full min-h-0 flex-col overflow-hidden border-y border-dialog-edge bg-panel sm:border">
        <header className="relative flex min-h-11 items-center justify-center bg-dialog-title px-4 py-2 text-dialog-title-foreground sm:min-h-10">
          <h1 className="truncate font-mono text-[11px] font-black uppercase tracking-[0.1em]">Session navigator</h1>
        </header>

        <div className="border-t border-dialog-edge bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
          <div className="flex items-center justify-between gap-3">
            <div className="min-w-0">
              <p className="font-mono text-xs font-bold text-white">Projects</p>
              <p className="mt-0.5 flex flex-wrap items-center gap-x-1 font-mono text-[10px] text-dialog-hint">
                {sessions === null ? (
                  'Reading sessions...'
                ) : (
                  <>
                    <span>{totals.projects} {totals.projects === 1 ? 'project' : 'projects'}</span>
                    <span className="opacity-40">·</span>
                    <span>{totals.all} {totals.all === 1 ? 'session' : 'sessions'}</span>
                    <span className="opacity-40">·</span>
                    <span className={totals.live > 0 ? 'font-bold text-ok' : ''}>
                      {totals.live > 0 ? '●' : '○'} {totals.live} live
                    </span>
                  </>
                )}
              </p>
            </div>
            <div className="grid shrink-0 grid-cols-2 gap-1.5">
              <Button
                variant="ghost"
                className="px-2.5 py-1 font-mono text-[10px]"
                onClick={() => void load()}
              >
                Refresh
              </Button>
              <Button
                className="px-2.5 py-1 font-mono text-[10px]"
                disabled={createBusy || !active}
                onClick={() => void createSession()}
              >
                {createBusy ? (
                  'Creating...'
                ) : (
                  <>
                    New<span className="hidden min-[390px]:inline"> session</span>
                  </>
                )}
              </Button>
            </div>
          </div>
          {createError && (
            <div className="mt-2">
              <Banner kind="err">{createError}</Banner>
            </div>
          )}
        </div>

        <div className="flex min-h-12 items-center border-y border-dialog-edge bg-panel px-3 sm:min-h-11 sm:px-4">
          <span className="shrink-0 font-mono text-xs text-accent-ink">›</span>
          <input
            value={query}
            onChange={(event) => setQuery(event.target.value)}
            className="min-w-0 flex-1 bg-transparent px-2 py-3 font-mono text-base text-white outline-none placeholder:text-dialog-hint sm:text-xs"
            placeholder="Filter title, project, or session"
            aria-label="Filter sessions"
          />
          <button
            type="button"
            className={`ml-1 min-h-10 shrink-0 border-l border-dialog-edge px-2 font-mono text-[9px] transition-colors sm:ml-3 sm:px-3 sm:text-[10px] ${
              showEmpty ? 'bg-accent text-accent-foreground' : 'text-dialog-hint hover:text-white'
            }`}
            onClick={() => setShowEmpty((value) => !value)}
          >
            empty:{showEmpty ? 'on' : 'off'} ({totals.hiddenEmpty})
          </button>
        </div>

        <div ref={listRef} className="min-h-0 flex-1 overflow-y-auto overscroll-contain [overflow-anchor:auto] [scrollbar-gutter:stable]">
        {sessions === null ? (
          <NavigatorSkeleton />
        ) : visible?.length === 0 ? (
          <div className="px-5 py-16 text-center">
            <p className="font-mono text-xs font-bold text-white/70">
              {query ? 'No matching sessions' : 'No sessions yet'}
            </p>
            <p className="mt-2 font-mono text-[11px] text-dialog-hint">
              {query ? 'Clear the filter or reveal empty sessions.' : 'Use New session to get started.'}
            </p>
          </div>
        ) : (
          <div className="border-t border-dialog-edge">
            <div className="hidden grid-cols-[minmax(14rem,1fr)_7rem_8rem_8rem] bg-ink/45 font-mono text-[9px] font-bold uppercase tracking-[0.1em] text-dialog-hint md:grid">
              <div className="border-r border-dialog-edge px-3 py-2">Title</div>
              <div className="border-r border-dialog-edge px-3 py-2">Session</div>
              <div className="border-r border-dialog-edge px-3 py-2">Status</div>
              <div className="px-3 py-2 text-right">Modified</div>
            </div>
            {groups.map(([project, projectSessions]) => (
              <ProjectGroup
                key={project}
                project={project}
                sessions={projectSessions}
                conn={active!}
                subscribedIds={subscribedIds}
                onOpen={onOpen}
              />
            ))}
          </div>
        )}
        </div>

        <footer className="hidden items-center justify-end border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-[10px] text-dialog-hint sm:flex sm:px-4">
          <span>{sessions ? `${totals.shown} of ${totals.all} sessions` : 'Reading sessions...'}</span>
        </footer>
      </div>
    </section>
  );
}


function ProjectGroup({
  project,
  sessions,
  conn,
  subscribedIds,
  onOpen,
}: {
  project: string;
  sessions: Session[];
  conn: GatewayConn;
  subscribedIds: ReadonlySet<string>;
  onOpen: Props['onOpen'];
}) {
  const root = projectRoot(sessions);
  const liveCount = sessions.filter(sessionIsLive).length;

  return (
    <section className="border-t border-dialog-edge first:border-t-0">
      <header className="grid min-h-14 grid-cols-[minmax(0,1fr)_auto] items-stretch bg-panel-2 sm:grid-cols-[auto_minmax(0,1fr)_auto]">
        <div className="hidden min-w-20 place-items-center bg-dialog-title px-3 font-mono text-[9px] font-black tracking-[0.14em] text-dialog-title-foreground sm:grid">
          PROJECT
        </div>
        <div className="min-w-0 border-l-2 border-accent px-3 py-2 sm:border-l-0 sm:border-r sm:border-dialog-edge">
          <h2 className="truncate font-mono text-[13px] font-bold leading-tight text-white">{project}</h2>
          <p className="mt-1 truncate font-mono text-[10px] text-dialog-hint" title={root}>
            {root || 'No workspace path'}
          </p>
        </div>
        <div className="flex min-w-[4.5rem] flex-col items-center justify-center gap-0.5 border-l border-dialog-edge px-2 text-center font-mono sm:min-w-20 sm:border-l-0 sm:px-3">
          <span className="leading-none">
            <strong className="text-xs text-white">{sessions.length}</strong>
            <span className="ml-1 text-[9px] text-dialog-hint sm:ml-0 sm:block">
              {sessions.length === 1 ? 'session' : 'sessions'}
            </span>
          </span>
          {liveCount > 0 && (
            <span className="flex items-center gap-1 text-[9px] font-bold uppercase tracking-wide text-ok">
              <span className="size-1.5 animate-pulse bg-ok motion-reduce:animate-none" />
              {liveCount} live
            </span>
          )}
        </div>
      </header>
      <div className="border-t border-dialog-edge">
        {sessions.map((session) => (
          <SessionRow
            key={session.id}
            session={session}
            conn={conn}
            subscribed={subscribedIds.has(session.id)}
            onOpen={onOpen}
          />
        ))}
      </div>
    </section>
  );
}

function SessionRow({
  session,
  conn,
  subscribed,
  onOpen,
}: {
  session: Session;
  conn: GatewayConn;
  subscribed: boolean;
  onOpen: Props['onOpen'];
}) {
  const status = statusLabel(session);
  const timestamp = session.modified_at ?? session.last_active_at ?? session.created_at;
  const title = session.title?.trim() || 'Untitled session';
  const live = sessionIsLive(session);

  return (
    <button
      type="button"
      className="group grid min-h-14 w-full text-left transition-[background-color,transform] duration-150 active:translate-x-0.5 active:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none md:min-h-11 md:grid-cols-[minmax(14rem,1fr)_7rem_8rem_8rem] md:active:translate-x-0 [&+&]:border-t [&+&]:border-dialog-edge"
      data-session-id={session.id}
      onClick={() => void onOpen(conn, session.id)}
    >
      <span className="relative flex min-w-0 flex-col justify-center px-3 py-2.5 sm:px-4 md:border-r md:border-dialog-edge md:py-3">
        <span className="absolute inset-y-2 left-0 w-0.5 bg-accent opacity-0 transition-opacity group-hover:opacity-100 group-focus-visible:opacity-100" />
        <span
          className={`block truncate font-mono text-xs font-semibold ${
            session.title?.trim() ? 'text-white' : 'text-white/45'
          }`}
        >
          {title}
        </span>
        <span className="mt-1.5 flex flex-wrap items-center gap-x-2 gap-y-1 font-mono text-[10px] text-dialog-hint md:hidden">
          <span className={`inline-flex shrink-0 items-center gap-1 font-bold tracking-[0.08em] ${statusTone(session)}`}>
            <span className={`size-1.5 ${statusDot(session)} ${live ? 'animate-pulse motion-reduce:animate-none' : ''}`} />
            {status}
          </span>
          <span className="shrink-0 opacity-40" aria-hidden="true">·</span>
          <span className="shrink-0">{shortId(session.id)}</span>
          {subscribed && (
            <span className="inline-flex shrink-0 items-center gap-1 font-bold tracking-[0.08em] text-accent-ink">
              <span className="size-1 bg-accent" /> SUB
            </span>
          )}
          <span className="ml-auto flex shrink-0 items-center gap-1.5 pl-2">
            <span>{relativeTime(timestamp)}</span>
            <span className="text-accent-ink opacity-60" aria-hidden="true">›</span>
          </span>
        </span>
      </span>
      <span className="hidden items-center border-r border-dialog-edge px-3 font-mono text-[10px] text-white/55 md:flex">
        {shortId(session.id)}
      </span>
      <span
        className={`hidden items-center gap-2 border-r border-dialog-edge px-3 font-mono text-[10px] font-bold tracking-[0.08em] md:flex ${statusTone(session)}`}
        title={`${Number(session.turn_count ?? 0)} turns`}
      >
        <span className={`size-1.5 ${statusDot(session)} ${live ? 'animate-pulse motion-reduce:animate-none' : ''}`} />
        {status}
      </span>
      <span
        className="hidden items-center justify-end px-3 font-mono text-[10px] text-dialog-hint md:flex"
        title={formatExact(timestamp)}
      >
        {relativeTime(timestamp)}
      </span>
    </button>
  );
}

function NavigatorSkeleton() {
  return (
    <div className="animate-pulse motion-reduce:animate-none" aria-label="Loading sessions">
      {[0, 1].map((index) => (
        <div key={index} className="border-t border-dialog-edge first:border-t-0">
          <div className="h-14 bg-panel-2" />
          <div className="h-11 border-t border-dialog-edge" />
          <div className="h-11 border-t border-dialog-edge" />
        </div>
      ))}
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
  const named = session.project_name?.trim() || session.workspace?.label?.trim();
  if (named) return homeifyPath(named);
  const root = session.workspace?.root?.replace(/\/+$/, '');
  if (root) return root.split('/').pop() || homeifyPath(root);
  return 'No project';
}

function projectRoot(sessions: Session[]): string {
  const workspace = sessions.find(
    (session) => session.workspace?.root || session.workspace?.repo_root,
  )?.workspace;
  return homeifyPath(workspace?.root || workspace?.repo_root);
}

function sessionIsLive(session: Session): boolean {
  return session.live ?? session.status === 'running';
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

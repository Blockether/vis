import { useCallback, useDeferredValue, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';
import { Banner, Button } from '../components/ui';
import { GatewayClient, type SessionMatch } from '../lib/gateway';
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
  const pollInFlight = useRef(false);
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
    const timer = window.setInterval(refreshLiveStates, 5_500);
    document.addEventListener('visibilitychange', refreshLiveStates);
    return () => {
      controller.abort();
      window.clearInterval(timer);
      document.removeEventListener('visibilitychange', refreshLiveStates);
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
    return { all, shown, projects, live };
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

  // A dead gateway is not a sessions problem: there is nothing to navigate, so the
  // shell drops us on the Gateways screen instead of rendering a session list
  // shaped like an error. Reporting it is this screen's only job here.
  useEffect(() => {
    onUnreachable?.(loadError);
  }, [loadError, onUnreachable]);

  if (loadError) return null;

  return (
    <section className="mx-auto flex h-full min-h-0 w-full max-w-[1400px] flex-col pb-0 pl-[env(safe-area-inset-left)] pr-[env(safe-area-inset-right)] pt-0 transition-[opacity,transform] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:px-6 sm:pb-6 sm:pt-6">
      <div className="flex h-full min-h-0 flex-col overflow-hidden border-y border-dialog-edge bg-panel sm:border">
        <header className="relative flex min-h-9 items-center justify-center bg-dialog-title px-4 py-1.5 text-dialog-title-foreground sm:min-h-8">
          <h1 className="truncate font-mono text-ui font-black uppercase tracking-[0.1em]">Session navigator</h1>
        </header>

        <div className="border-t border-dialog-edge bg-panel-2 px-3 py-2.5 sm:px-4 sm:py-3">
          <div className="flex items-center justify-between gap-3">
            <div className="min-w-0">
              <p className="font-mono text-body font-bold text-white">Projects</p>
              <p className="mt-0.5 flex flex-wrap items-center gap-x-1 font-mono text-meta text-dialog-hint">
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
            <div className="grid shrink-0 grid-cols-2 gap-1">
              <Button
                variant="ghost"
                className="min-h-6 px-2 py-0.5 font-mono text-chip sm:min-h-6"
                onClick={() => void load()}
              >
                Refresh
              </Button>
              <Button
                variant="solid"
                className="min-h-6 px-2 py-0.5 font-mono text-chip sm:min-h-6"
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
              />
            ))}
          </div>
        )}
        </div>

        <footer className="hidden items-center justify-end border-t border-dialog-edge bg-panel-2 px-3 py-2 font-mono text-meta text-dialog-hint sm:flex sm:px-4">
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
  matches,
  needle,
  onOpen,
}: {
  project: string;
  sessions: Session[];
  conn: GatewayConn;
  subscribedIds: ReadonlySet<string>;
  matches: Map<string, SessionMatch> | null;
  needle: string;
  onOpen: Props['onOpen'];
}) {
  const root = projectRoot(sessions);
  const liveCount = sessions.filter(sessionIsLive).length;

  return (
    <section className="border-t border-dialog-edge first:border-t-0" aria-label={`${project} sessions`}>
      <header className="flex min-h-11 items-center justify-between gap-3 bg-panel-2 px-3 py-2 sm:px-4">
        <div className="min-w-0">
          <h2 className="truncate font-mono text-ui font-bold text-white">{project}</h2>
          <p className="mt-0.5 truncate font-mono text-chip text-dialog-hint" title={root}>
            {root || 'No workspace path'}
          </p>
        </div>
        <div className="flex shrink-0 items-center gap-2 font-mono text-chip text-dialog-hint">
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
        </div>
      </header>
      <div className="border-t border-dialog-edge">
        {sessions.map((session) => (
          <SessionRow
            key={session.id}
            session={session}
            conn={conn}
            subscribed={subscribedIds.has(session.id)}
            match={matches?.get(session.id) ?? null}
            needle={needle}
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
  match,
  needle,
  onOpen,
}: {
  session: Session;
  conn: GatewayConn;
  subscribed: boolean;
  match: SessionMatch | null;
  needle: string;
  onOpen: Props['onOpen'];
}) {
  const status = statusLabel(session);
  const timestamp = session.modified_at ?? session.last_active_at ?? session.created_at;
  const title = session.title?.trim() || 'Untitled session';
  const live = sessionIsLive(session);
  const turns = Number(session.turn_count ?? 0);

  return (
    <div className="[&+&]:border-t [&+&]:border-dialog-edge">
      <button
        type="button"
        className="group flex min-h-14 w-full items-start gap-2 px-3 py-2.5 text-left transition-colors duration-150 hover:bg-hover active:bg-hover focus-visible:bg-hover focus-visible:outline-none motion-reduce:transition-none sm:min-h-12 sm:px-4 sm:py-2"
        data-session-id={session.id}
        onClick={() => void onOpen(conn, session.id)}
      >
        <span className="mt-0.5 shrink-0 font-mono text-body text-accent-ink opacity-40 transition-opacity group-hover:opacity-100 group-focus-visible:opacity-100" aria-hidden="true">›</span>
        <span className="min-w-0 flex-1">
          <span className="flex min-w-0 items-start justify-between gap-3">
            <span
              className={`block min-w-0 truncate font-mono text-ui font-semibold ${
                session.title?.trim() ? 'text-white' : 'text-white/45'
              }`}
            >
              {title}
            </span>
            <span className={`inline-flex shrink-0 items-center gap-1 font-mono text-chip font-bold tracking-[0.08em] ${statusTone(session)}`}>
              <span className={`size-1.5 ${statusDot(session)} ${live ? 'animate-pulse motion-reduce:animate-none' : ''}`} />
              {status}
            </span>
          </span>
          <span className="mt-1 flex flex-wrap items-center gap-x-2 gap-y-1 font-mono text-chip text-dialog-hint">
            <span className="text-white/55">{shortId(session.id)}</span>
            <span className="opacity-40" aria-hidden="true">·</span>
            <span>{turns} {turns === 1 ? 'turn' : 'turns'}</span>
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
      {match && <MatchPreview match={match} needle={needle} />}
    </div>
  );
}

function NavigatorSkeleton() {
  return (
    <div className="animate-pulse motion-reduce:animate-none" aria-label="Loading sessions">
      {[0, 1].map((index) => (
        <div key={index} className="border-t border-dialog-edge first:border-t-0">
          <div className="h-11 bg-panel-2" />
          <div className="h-12 border-t border-dialog-edge" />
          <div className="h-12 border-t border-dialog-edge" />
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
    <div className="border-t border-dialog-edge bg-ink/30 px-3 py-1.5 sm:px-10">
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

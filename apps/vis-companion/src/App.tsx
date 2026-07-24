import { useCallback, useEffect, useMemo, useState } from 'react';
import type { GatewayConn } from './lib/types';
import { GatewayClient } from './lib/gateway';
import {
  getActiveConnection,
  loadConnections,
  loadSubscribedSessions,
  rememberSubscribedSession,
  setActiveUrl,
  upsertConnection,
  removeConnection,
} from './lib/storage';
import { SessionSubscriptionHub } from './lib/subscriptions';
import { parsePairing } from './lib/pairing';
import { onPairingLink } from './lib/deeplink';
import { applyGatewayTheme, resolveLocalTheme, resolveTheme } from './lib/theme';
import { getThemePref } from './lib/storage';
import { ConnectScreen } from './screens/ConnectScreen';
import { SessionsScreen } from './screens/SessionsScreen';
import { GatewaySettingsDialog } from './screens/SettingsScreen';
import { SessionScreen } from './screens/SessionScreen';
import { parseRoute, sessionHash, tabHash } from './lib/router';

type Tab = 'sessions' | 'connect';

export function App() {
  const [conns, setConns] = useState<GatewayConn[]>([]);
  const [active, setActive] = useState<GatewayConn | null>(null);
  const [tab, setTab] = useState<Tab>('sessions');
  const [openTarget, setOpenTarget] = useState<{ conn: GatewayConn; sid: string; fresh?: boolean } | null>(null);
  const [settingsTarget, setSettingsTarget] = useState<GatewayConn | null>(null);
  const [subscribedIds, setSubscribedIds] = useState<Set<string>>(new Set());
  const [ready, setReady] = useState(false);

  const sessionConn = openTarget?.conn ?? active;
  const connectionKey = sessionConn ? `${sessionConn.url}\u0000${sessionConn.token ?? ''}` : '';
  const client = useMemo(
    () => sessionConn ? new GatewayClient(sessionConn) : null,
    // A URL/token pair is the transport identity; labels do not require a new stream.
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [connectionKey],
  );
  const subscriptions = useMemo(
    () => client ? new SessionSubscriptionHub(client) : null,
    [client],
  );
  // Stable client for the open settings dialog: a fresh `new GatewayClient(...)`
  // per render made the dialog's `load` re-fire on every unrelated App re-render
  // (session polling, subscriptions), re-applying the theme and flickering it.
  const settingsClient = useMemo(
    () => settingsTarget ? new GatewayClient(settingsTarget) : null,
    [settingsTarget],
  );

  const refresh = useCallback(async () => {
    setConns(await loadConnections());
    setActive(await getActiveConnection());
  }, []);

  const addConnection = useCallback(
    async (conn: GatewayConn, makeActive = true) => {
      const next = await upsertConnection(conn);
      if (makeActive) await setActiveUrl(conn.url);
      setConns(next);
      setActive(await getActiveConnection());
      if (makeActive) setTab('sessions');
    },
    [],
  );

  const selectConnection = useCallback(async (url: string) => {
    await setActiveUrl(url);
    setActive(await getActiveConnection());
    setOpenTarget(null);
    setTab('sessions');
  }, []);

  const activateConnection = useCallback(async (url: string | null) => {
    await setActiveUrl(url);
    setActive(await getActiveConnection());
  }, []);

  const openGatewaySession = useCallback(async (conn: GatewayConn, sid: string, fresh = false) => {
    await setActiveUrl(conn.url);
    const ids = await rememberSubscribedSession(conn.url, sid);
    setSubscribedIds(new Set(ids));
    setActive(conn);
    setOpenTarget({ conn, sid, fresh });
  }, []);

  // Hash routing: a session is a shareable URL (#/s/<sid>?gw=<gateway-url>).
  // Apply the current hash to view state, and follow browser back/forward and
  // pasted links via `hashchange`. Opening still needs the gateway paired
  // locally (its bearer token is never in the link).
  const applyRoute = useCallback(
    (hash: string) => {
      const route = parseRoute(hash);
      if (route.name === 'session') {
        const conn =
          (route.gw && conns.find((c) => c.id === route.gw || c.url === route.gw)) || active;
        if (conn) {
          void openGatewaySession(conn, route.sid);
          return;
        }
        setOpenTarget(null);
        setTab('connect');
        return;
      }
      setOpenTarget(null);
      setTab(route.name === 'connect' ? 'connect' : 'sessions');
    },
    [conns, active, openGatewaySession],
  );

  // Apply the initial hash once connections are loaded, then track hashchange.
  const [routeApplied, setRouteApplied] = useState(false);
  useEffect(() => {
    if (!ready) return;
    if (!routeApplied) {
      applyRoute(window.location.hash);
      setRouteApplied(true);
    }
    const onHash = () => applyRoute(window.location.hash);
    window.addEventListener('hashchange', onHash);
    return () => window.removeEventListener('hashchange', onHash);
  }, [ready, routeApplied, applyRoute]);

  // Reflect view state back into the URL so the address bar is always shareable.
  useEffect(() => {
    if (!ready || !routeApplied) return;
    // Prefer the freshest captured gateway id (backfilled after open) over the
    // one snapshotted into openTarget, so the shareable URL cleans up in place.
    const gwId =
      conns.find((c) => c.url === openTarget?.conn.url)?.id ?? openTarget?.conn.id;
    const desired = openTarget
      ? sessionHash(openTarget.sid, gwId)
      : tabHash(tab === 'connect' ? 'connect' : 'sessions');
    const current = window.location.hash || '#/';
    if (current !== desired) {
      history.replaceState(null, '', desired);
    }
  }, [openTarget, tab, ready, routeApplied, conns]);

  useEffect(() => {
    void refresh().finally(() => setReady(true));
  }, [refresh]);

  // Paint the app-local theme (default light) immediately on mount, before any
  // gateway connects and regardless of a stale browser-cached stylesheet. The
  // gateway effect below re-resolves once a connection's palette is available.
  useEffect(() => {
    void getThemePref().then((pref) => applyGatewayTheme(resolveLocalTheme(pref)));
  }, []);

  // Deep-linked pairing: vis://gateway?url=…&token=…
  useEffect(() => {
    let dispose = () => {};
    void onPairingLink((url) => {
      const conn = parsePairing(url);
      if (conn) void addConnection(conn);
    }).then((d) => (dispose = d));
    return () => dispose();
  }, [addConnection]);

  // The gateway owns the set of themes and their exact colors; the app pins its
  // own local preference on top (defaults to light) without mutating the shared
  // TUI/gateway theme.
  useEffect(() => {
    if (!active) return;
    const ctrl = new AbortController();
    void Promise.all([new GatewayClient(active).theme(ctrl.signal), getThemePref()])
      .then(([theme, pref]) => applyGatewayTheme(resolveTheme(theme, pref)))
      .catch(() => undefined);
    return () => ctrl.abort();
  }, [active]);

  // Backfill each paired gateway's stable id (from /healthz) so a shareable link
  // can name its gateway by id instead of leaking the gateway URL. Cheap: it
  // only probes a connection that has no id captured yet, then converges.
  useEffect(() => {
    const missing = conns.filter((c) => !c.id);
    if (missing.length === 0) return;
    let cancelled = false;
    const ctrl = new AbortController();
    void (async () => {
      let changed = false;
      for (const conn of missing) {
        const id = await new GatewayClient(conn).identify(ctrl.signal).catch(() => null);
        if (id) {
          await upsertConnection({ ...conn, id });
          changed = true;
        }
      }
      if (changed && !cancelled) {
        const next = await loadConnections();
        if (cancelled) return;
        setConns(next);
        setActive(await getActiveConnection());
      }
    })();
    return () => {
      cancelled = true;
      ctrl.abort();
    };
  }, [conns]);

  // Restore the bounded, per-gateway watch list. One multiplexed SSE stream
  // keeps every visited session live even while another view is open.
  useEffect(() => {
    let cancelled = false;
    if (!subscriptions || !sessionConn) {
      setSubscribedIds(new Set());
      return;
    }
    void loadSubscribedSessions(sessionConn.url).then((ids) => {
      if (cancelled) return;
      const next = openTarget?.sid && !ids.includes(openTarget.sid)
        ? [openTarget.sid, ...ids]
        : ids;
      subscriptions.watchSessions(next);
      setSubscribedIds(new Set(next));
    });
    return () => {
      cancelled = true;
    };
  }, [openTarget?.sid, sessionConn, subscriptions]);

  useEffect(() => () => subscriptions?.dispose(), [subscriptions]);

  if (!ready) return <Splash />;

  const hasConn = conns.length > 0 && !!active;

  return (
    <div className="isolate flex h-dvh min-h-0 flex-col overflow-hidden bg-ink text-[13px]">
      {!openTarget && <Header tab={hasConn ? tab : 'connect'} hasConn={hasConn} onTab={setTab} />}

      <main className={`min-h-0 flex-1 ${openTarget ? 'overflow-hidden' : 'overflow-y-auto'}`}>
        {!hasConn || tab === 'connect' ? (
          <ConnectScreen
            conns={conns}
            active={active}
            onAdd={addConnection}
            onSettings={setSettingsTarget}
          />
        ) : openTarget && client && subscriptions ? (
          <SessionScreen
            key={`${openTarget.conn.url}:${openTarget.sid}`}
            client={client}
            subscriptions={subscriptions}
            sid={openTarget.sid}
            fresh={openTarget.fresh}
            onBack={() => setOpenTarget(null)}
            onOpenSession={(sid, fresh) => void openGatewaySession(openTarget.conn, sid, fresh)}
          />
        ) : (
          <SessionsScreen
            active={active}
            client={client}
            subscriptions={subscriptions}
            subscribedIds={subscribedIds}
            gatewayCount={conns.length}
            onOpen={openGatewaySession}
          />
        )}
      </main>

      {hasConn && !openTarget && <TabBar tab={tab} onTab={setTab} />}

      {settingsTarget && settingsClient && (
        <GatewaySettingsDialog
          key={settingsTarget.url}
          client={settingsClient}
          gateway={settingsTarget}
          isActive={settingsTarget.url === active?.url}
          onActivate={() => void selectConnection(settingsTarget.url)}
          onDeactivate={() => {
            void activateConnection(null);
            setSettingsTarget(null);
          }}
          onRename={async (label) => {
            const updated = { ...settingsTarget, label };
            await upsertConnection(updated);
            setSettingsTarget(updated);
            await refresh();
          }}
          onRemove={async () => {
            await removeConnection(settingsTarget.url);
            await refresh();
          }}
          onClose={() => setSettingsTarget(null)}
        />
      )}
    </div>
  );
}

function Header({ tab, hasConn, onTab }: { tab: Tab; hasConn: boolean; onTab: (tab: Tab) => void }) {
  return (
    <header className="relative z-30 shrink-0 border-b border-dialog-edge bg-panel-2 pt-[env(safe-area-inset-top)]">
      <div className="mx-auto flex h-12 w-full max-w-[1400px] items-center justify-between px-[max(0.75rem,env(safe-area-inset-left))] pr-[max(0.75rem,env(safe-area-inset-right))] sm:px-6">
        <div className="flex items-center gap-2.5" aria-label="Vis">
          <img src="/vis-logo.png" alt="" className="h-[18px] w-5 object-contain" />
          <span className="font-mono text-[13px] font-black tracking-[0.18em] text-white">VIS</span>
        </div>
        <nav className="hidden h-full items-stretch sm:flex" aria-label="Primary navigation">
          {(hasConn ? (['sessions', 'connect'] as Tab[]) : (['connect'] as Tab[])).map((item) => (
            <button
              type="button"
              key={item}
              onClick={() => onTab(item)}
              className={`relative flex min-w-28 items-center justify-center gap-2 px-4 font-mono text-[10px] font-bold uppercase tracking-[0.1em] transition-[color,background-color] duration-150 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent ${
                tab === item
                  ? 'bg-panel text-white after:absolute after:inset-x-3 after:bottom-0 after:h-0.5 after:bg-accent'
                  : 'text-dialog-hint hover:bg-hover hover:text-white'
              }`}
              aria-current={tab === item ? 'page' : undefined}
            >
              <NavIcon id={item} />
              {item === 'sessions' ? 'Sessions' : 'Gateways'}
            </button>
          ))}
        </nav>
      </div>
    </header>
  );
}

function TabBar({ tab, onTab }: { tab: Tab; onTab: (tab: Tab) => void }) {
  const items: { id: Tab; label: string }[] = [
    { id: 'sessions', label: 'Sessions' },
    { id: 'connect', label: 'Gateways' },
  ];

  return (
    <nav
      className="relative z-30 grid shrink-0 grid-cols-2 border-t border-dialog-edge bg-panel-2 px-[max(0.5rem,env(safe-area-inset-left))] pb-[max(0.35rem,env(safe-area-inset-bottom))] pt-1 sm:hidden"
      aria-label="Primary navigation"
    >
      {items.map((item) => (
        <button
          type="button"
          key={item.id}
          onClick={() => onTab(item.id)}
          className={`relative flex min-h-12 items-center justify-center gap-2 font-mono text-[10px] font-bold uppercase tracking-[0.08em] transition-[color,background-color,transform] duration-150 active:scale-[0.98] motion-reduce:transition-none ${
            tab === item.id
              ? 'bg-panel text-white after:absolute after:inset-x-5 after:top-0 after:h-0.5 after:bg-accent'
              : 'text-dialog-hint active:bg-hover active:text-white'
          }`}
          aria-current={tab === item.id ? 'page' : undefined}
        >
          <NavIcon id={item.id} />
          <span>{item.label}</span>
        </button>
      ))}
    </nav>
  );
}

function NavIcon({ id }: { id: Tab }) {
  if (id === 'sessions') {
    return (
      <svg viewBox="0 0 20 20" className="size-4" fill="none" stroke="currentColor" strokeWidth="1.5" aria-hidden="true">
        <path d="M3.5 4.5h13v11h-13zM6.5 7.5h7M6.5 10h7M6.5 12.5h4" />
      </svg>
    );
  }
  return (
    <svg viewBox="0 0 20 20" className="size-4" fill="none" stroke="currentColor" strokeWidth="1.5" aria-hidden="true">
      <path d="M4 4.5h12v4H4zM4 11.5h12v4H4zM6.5 6.5h.01M6.5 13.5h.01" />
    </svg>
  );
}

function Splash() {
  return (
    <div className="flex h-full items-center justify-center bg-ink" aria-label="Loading Vis">
      <img src="/vis-logo.png" alt="Vis" className="h-16 w-auto animate-pulse object-contain" />
    </div>
  );
}

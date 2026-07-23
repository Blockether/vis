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
} from './lib/storage';
import { SessionSubscriptionHub } from './lib/subscriptions';
import { parsePairing } from './lib/pairing';
import { onPairingLink } from './lib/deeplink';
import { applyGatewayTheme } from './lib/theme';
import { ConnectScreen } from './screens/ConnectScreen';
import { SessionsScreen } from './screens/SessionsScreen';
import { GatewaySettingsDialog } from './screens/SettingsScreen';
import { SessionScreen } from './screens/SessionScreen';

type Tab = 'sessions' | 'connect';

export function App() {
  const [conns, setConns] = useState<GatewayConn[]>([]);
  const [active, setActive] = useState<GatewayConn | null>(null);
  const [tab, setTab] = useState<Tab>('sessions');
  const [openTarget, setOpenTarget] = useState<{ conn: GatewayConn; sid: string } | null>(null);
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

  const openGatewaySession = useCallback(async (conn: GatewayConn, sid: string) => {
    await setActiveUrl(conn.url);
    const ids = await rememberSubscribedSession(conn.url, sid);
    setSubscribedIds(new Set(ids));
    setActive(conn);
    setOpenTarget({ conn, sid });
  }, []);

  useEffect(() => {
    void refresh().finally(() => setReady(true));
  }, [refresh]);

  // Deep-linked pairing: vis://gateway?url=…&token=…
  useEffect(() => {
    let dispose = () => {};
    void onPairingLink((url) => {
      const conn = parsePairing(url);
      if (conn) void addConnection(conn);
    }).then((d) => (dispose = d));
    return () => dispose();
  }, [addConnection]);

  // The gateway exposes the exact palette selected in the TUI's persisted settings.
  useEffect(() => {
    if (!active) return;
    const ctrl = new AbortController();
    void new GatewayClient(active)
      .theme(ctrl.signal)
      .then(applyGatewayTheme)
      .catch(() => undefined);
    return () => ctrl.abort();
  }, [active]);

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
            onSelect={selectConnection}
            onSettings={setSettingsTarget}
            onChanged={refresh}
          />
        ) : openTarget && client && subscriptions ? (
          <SessionScreen
            key={`${openTarget.conn.url}:${openTarget.sid}`}
            client={client}
            subscriptions={subscriptions}
            sid={openTarget.sid}
            onBack={() => setOpenTarget(null)}
            onOpenSession={(sid) => void openGatewaySession(openTarget.conn, sid)}
          />
        ) : (
          <SessionsScreen
            active={active}
            client={client}
            subscriptions={subscriptions}
            subscribedIds={subscribedIds}
            onOpen={openGatewaySession}
          />
        )}
      </main>

      {hasConn && !openTarget && <TabBar tab={tab} onTab={setTab} />}

      {settingsTarget && (
        <GatewaySettingsDialog
          key={settingsTarget.url}
          client={new GatewayClient(settingsTarget)}
          gateway={settingsTarget}
          isActive={settingsTarget.url === active?.url}
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
          <span className="hidden h-3 w-px bg-dialog-edge sm:block" aria-hidden="true" />
          <span className="hidden font-mono text-[9px] uppercase tracking-[0.14em] text-dialog-hint sm:block">
            gateway console
          </span>
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

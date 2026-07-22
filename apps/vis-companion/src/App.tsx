import { useCallback, useEffect, useState } from 'react';
import type { GatewayConn } from './lib/types';
import { GatewayClient } from './lib/gateway';
import {
  getActiveConnection,
  loadConnections,
  setActiveUrl,
  upsertConnection,
} from './lib/storage';
import { parsePairing } from './lib/pairing';
import { onPairingLink } from './lib/deeplink';
import { applyGatewayTheme } from './lib/theme';
import { ConnectScreen } from './screens/ConnectScreen';
import { SessionsScreen } from './screens/SessionsScreen';
import { SettingsScreen } from './screens/SettingsScreen';
import { SessionScreen } from './screens/SessionScreen';

type Tab = 'sessions' | 'settings' | 'connect';

export function App() {
  const [conns, setConns] = useState<GatewayConn[]>([]);
  const [active, setActive] = useState<GatewayConn | null>(null);
  const [tab, setTab] = useState<Tab>('sessions');
  const [openSid, setOpenSid] = useState<string | null>(null);
  const [ready, setReady] = useState(false);

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
    setTab('sessions');
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

  if (!ready) return <Splash />;

  const client = active ? new GatewayClient(active) : null;
  const hasConn = conns.length > 0 && !!active;

  return (
    <div className="flex h-full flex-col bg-ink text-[15px]">
      {!openSid && <Header active={active} />}

      <main className={`min-h-0 flex-1 ${openSid ? 'overflow-hidden' : 'overflow-y-auto'}`}>
        {!hasConn || tab === 'connect' ? (
          <ConnectScreen
            conns={conns}
            active={active}
            onAdd={addConnection}
            onSelect={selectConnection}
            onChanged={refresh}
          />
        ) : openSid && client ? (
          <SessionScreen
            client={client}
            sid={openSid}
            onBack={() => setOpenSid(null)}
          />
        ) : tab === 'sessions' && client ? (
          <SessionsScreen client={client} onOpen={setOpenSid} />
        ) : tab === 'settings' && client ? (
          <SettingsScreen client={client} />
        ) : null}
      </main>

      {hasConn && !openSid && (
        <TabBar tab={tab} onTab={setTab} />
      )}
    </div>
  );
}

function Header({ active }: { active: GatewayConn | null }) {
  return (
    <header className="flex items-center justify-between border-b border-edge bg-panel px-4 py-3">
      <div className="flex items-center gap-2">
        <span className="font-mono text-accent">vis</span>
        <span className="text-sm text-white/50">companion</span>
      </div>
      {active && (
        <span className="max-w-[55%] truncate font-mono text-xs text-white/40">
          {active.label ?? active.url}
        </span>
      )}
    </header>
  );
}

function TabBar({ tab, onTab }: { tab: Tab; onTab: (t: Tab) => void }) {
  const items: { id: Tab; label: string; icon: string }[] = [
    { id: 'sessions', label: 'Sessions', icon: '◧' },
    { id: 'settings', label: 'Settings', icon: '⚙' },
    { id: 'connect', label: 'Gateway', icon: '⇄' },
  ];
  return (
    <nav className="flex border-t border-edge bg-panel">
      {items.map((it) => (
        <button
          key={it.id}
          onClick={() => onTab(it.id)}
          className={`flex flex-1 flex-col items-center gap-0.5 py-2.5 text-xs transition-colors ${
            tab === it.id ? 'text-accent' : 'text-white/45'
          }`}
        >
          <span className="text-lg leading-none">{it.icon}</span>
          {it.label}
        </button>
      ))}
    </nav>
  );
}

function Splash() {
  return (
    <div className="flex h-full items-center justify-center bg-ink">
      <span className="animate-pulse font-mono text-accent">vis companion…</span>
    </div>
  );
}

import { useCallback, useEffect, useState } from 'react';
import type { GatewayConn } from '../lib/types';
import { GatewayClient, GatewayError } from '../lib/gateway';
import { parsePairing } from '../lib/pairing';
import { removeConnection } from '../lib/storage';
import { scanQr } from '../lib/scan';
import { Banner, Button, Input } from '../components/ui';

interface Props {
  conns: GatewayConn[];
  active: GatewayConn | null;
  onAdd: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  onSelect: (url: string) => Promise<void>;
  onSettings: (conn: GatewayConn) => void;
  onChanged: () => Promise<void>;
}

export function ConnectScreen({
  conns,
  active,
  onAdd,
  onSelect,
  onSettings,
  onChanged,
}: Props) {
  const [payload, setPayload] = useState('');
  const [url, setUrl] = useState('');
  const [token, setToken] = useState('');
  const [msg, setMsg] = useState<{ kind: 'ok' | 'err'; text: string } | null>(null);
  const [busy, setBusy] = useState(false);
  const [health, setHealth] = useState<Record<string, GwHealth>>({});

  const probe = useCallback(async (list: GatewayConn[]) => {
    await Promise.all(
      list.map(async (conn) => {
        setHealth((h) => ({
          ...h,
          [conn.url]: { state: h[conn.url]?.state ?? 'checking', ms: h[conn.url]?.ms },
        }));
        const started = Date.now();
        try {
          const reachable = await new GatewayClient(conn).ping();
          setHealth((h) => ({
            ...h,
            [conn.url]: { state: reachable ? 'online' : 'offline', ms: Date.now() - started },
          }));
        } catch (e) {
          const unauthorized = e instanceof GatewayError && e.status === 401;
          setHealth((h) => ({
            ...h,
            [conn.url]: { state: unauthorized ? 'auth' : 'offline', ms: Date.now() - started },
          }));
        }
      }),
    );
  }, []);

  // Live reachability: probe every saved gateway on mount, then every 6s, so the
  // list shows which gateways are actually online, offline, or unauthorized —
  // not just which one is selected.
  useEffect(() => {
    if (conns.length === 0) return;
    void probe(conns);
    const id = window.setInterval(() => void probe(conns), 6000);
    return () => window.clearInterval(id);
  }, [conns, probe]);

  async function tryConn(conn: GatewayConn) {
    setBusy(true);
    setMsg(null);
    try {
      const client = new GatewayClient(conn);
      await client.ping();
      await onAdd(conn);
      setMsg({ kind: 'ok', text: `Paired with ${conn.label ?? conn.url}` });
      setPayload('');
      setUrl('');
      setToken('');
    } catch (e) {
      await onAdd(conn, false);
      setMsg({ kind: 'err', text: `Saved, but not reachable yet: ${(e as Error).message}` });
    } finally {
      setBusy(false);
    }
  }

  async function addFromPayload() {
    const conn = parsePairing(payload);
    if (!conn) {
      setMsg({ kind: 'err', text: 'Not a vis:// pairing link or gateway URL' });
      return;
    }
    await tryConn(conn);
  }

  async function addManual() {
    if (!/^https?:\/\//i.test(url.trim())) {
      setMsg({ kind: 'err', text: 'URL must start with http:// or https://' });
      return;
    }
    const u = url.trim();
    await tryConn({ url: u, token: token.trim() || undefined, label: hostOf(u) });
  }

  async function scan() {
    try {
      const raw = await scanQr();
      if (!raw) {
        setMsg({ kind: 'err', text: 'No QR code found — try again or paste the link' });
        return;
      }
      const conn = parsePairing(raw);
      if (!conn) {
        setMsg({ kind: 'err', text: 'QR is not a Vis pairing code' });
        return;
      }
      await tryConn(conn);
    } catch (cause) {
      const text = (cause as Error).message || '';
      // A user-dismissed camera is not an error — stay silent.
      if (/cancel/i.test(text)) return;
      setMsg({ kind: 'err', text: text || 'Camera unavailable' });
    }
  }

  return (
    <div className="mx-auto w-full max-w-3xl space-y-5 px-[max(0.75rem,env(safe-area-inset-left))] pb-[max(2rem,env(safe-area-inset-bottom))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-4 transition-[opacity,transform] duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:space-y-6 sm:px-6 sm:py-6">
      <header className="border-b border-dialog-edge pb-3">
        <p className="font-mono text-[10px] font-bold uppercase tracking-[0.18em] text-accent-ink">
          Connections
        </p>
        <div className="mt-1 flex items-end justify-between gap-4">
          <div>
            <h1 className="font-mono text-base font-black text-white">Gateways</h1>
            <p className="mt-1 max-w-xl text-xs leading-5 text-dialog-hint">
              Choose where Vis runs. Each gateway owns its sessions, theme, and settings.
            </p>
          </div>
          {conns.length > 0 && (
            <span className="shrink-0 font-mono text-[10px] text-dialog-hint">
              {conns.length} saved
            </span>
          )}
        </div>
      </header>

      {msg && <Banner kind={msg.kind === 'ok' ? 'ok' : 'err'}>{msg.text}</Banner>}

      {conns.length > 0 && (
        <section className="overflow-hidden border border-dialog-edge bg-panel shadow-none sm:shadow-[4px_4px_0_var(--dialog-shadow)]">
          <header className="flex min-h-9 items-center justify-between bg-dialog-title px-3 py-2 text-dialog-title-foreground">
            <h2 className="font-mono text-xs font-black uppercase tracking-[0.12em]">Saved gateways</h2>
            <span className="font-mono text-[9px] font-bold uppercase tracking-wider opacity-65">
              Select · configure
            </span>
          </header>
          <div className="divide-y divide-dialog-edge border-t border-dialog-edge">
            {conns.map((conn) => {
              const selected = active?.url === conn.url;
              const hv = healthView(health[conn.url]);
              return (
                <div
                  key={conn.url}
                  className={`grid min-w-0 grid-cols-1 gap-1.5 px-2 py-2 transition-[background-color,opacity,transform] duration-150 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:grid-cols-[minmax(0,1fr)_auto] sm:items-center sm:gap-2 sm:px-3 ${
                    selected ? 'border-l-2 border-accent bg-panel-2' : 'bg-panel hover:bg-hover'
                  }`}
                >
                  <button
                    type="button"
                    className="group flex min-h-11 min-w-0 items-center gap-3 px-1 text-left focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-accent/60"
                    onClick={() => onSelect(conn.url)}
                    aria-current={selected ? 'true' : undefined}
                  >
                    <span
                      className={`shrink-0 font-mono text-sm ${hv.dotClass} ${hv.state === 'checking' ? 'animate-pulse' : ''}`}
                      aria-hidden="true"
                      title={hv.label}
                    >
                      {hv.glyph}
                    </span>
                    <span className="min-w-0">
                      <span className="flex min-w-0 items-center gap-2">
                        <span className="truncate font-mono text-xs font-bold text-white">
                          {conn.label ?? hostOf(conn.url)}
                        </span>
                        {selected && (
                          <span className="shrink-0 font-mono text-[8px] font-black uppercase tracking-wider text-ok">
                            Active
                          </span>
                        )}
                      </span>
                      <span className="flex min-w-0 items-center gap-2">
                        <span className="block truncate font-mono text-[9px] text-dialog-hint">
                          {conn.url}
                        </span>
                        <span className={`shrink-0 font-mono text-[9px] font-bold uppercase tracking-wider ${hv.textClass}`}>
                          {hv.label}
                          {hv.ms != null && hv.state === 'online' ? ` \u00b7 ${hv.ms}ms` : ''}
                        </span>
                      </span>
                    </span>
                  </button>

                  <div className="grid grid-cols-2 gap-1.5 sm:flex sm:shrink-0 sm:items-center">
                    <Button
                      variant="ghost"
                      className="w-full px-2.5 py-1.5 font-mono text-[10px] sm:w-auto"
                      onClick={() => onSettings(conn)}
                      aria-label={`Settings for ${conn.label ?? hostOf(conn.url)}`}
                    >
                      Settings
                    </Button>
                    <Button
                      variant="danger"
                      className="w-full px-2.5 py-1.5 font-mono text-[10px] sm:w-auto"
                      onClick={async () => {
                        await removeConnection(conn.url);
                        await onChanged();
                      }}
                      aria-label={`Remove ${conn.label ?? hostOf(conn.url)}`}
                    >
                      Remove
                    </Button>
                  </div>
                </div>
              );
            })}
          </div>
        </section>
      )}

      <section className="transition-[opacity,transform] delay-75 duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
        <div className="mb-3 flex items-center gap-3">
          <h2 className="font-mono text-xs font-black uppercase tracking-[0.12em] text-white">
            Add a gateway
          </h2>
          <span className="h-px flex-1 bg-dialog-edge" />
        </div>

        <div className="grid min-w-0 items-start gap-3 md:grid-cols-2">
          <div className="overflow-hidden border border-dialog-edge bg-panel transition-colors focus-within:border-accent">
            <header className="border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
              <h3 className="font-mono text-xs font-bold text-white">Pairing link</h3>
              <p className="mt-0.5 text-[11px] leading-4 text-dialog-hint">
                Paste the link from <code className="text-accent-ink">vis gateway pair</code>.
              </p>
            </header>
            <div className="space-y-2.5 p-3">
              <Input
                placeholder="vis://gateway?url=…&amp;token=…"
                value={payload}
                onChange={(event) => setPayload(event.target.value)}
                autoCapitalize="none"
                autoCorrect="off"
              />
              <div className="flex gap-2">
                <Button className="min-h-11 flex-1 sm:min-h-10" onClick={addFromPayload} disabled={busy || !payload}>
                  Pair
                </Button>
                <Button variant="ghost" className="min-h-11 sm:min-h-10" onClick={scan} disabled={busy}>
                  Scan QR
                </Button>
              </div>
            </div>
          </div>

          <div className="overflow-hidden border border-dialog-edge bg-panel transition-colors focus-within:border-accent">
            <header className="border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
              <h3 className="font-mono text-xs font-bold text-white">URL + token</h3>
              <p className="mt-0.5 text-[11px] leading-4 text-dialog-hint">
                LAN, Tailscale, and Cloudflare tunnel addresses are supported.
              </p>
            </header>
            <div className="space-y-2.5 p-3">
              <Input
                placeholder="https://gateway.example.com"
                value={url}
                onChange={(event) => setUrl(event.target.value)}
                autoCapitalize="none"
                autoCorrect="off"
              />
              <Input
                placeholder="Bearer token (optional on loopback)"
                value={token}
                onChange={(event) => setToken(event.target.value)}
                autoCapitalize="none"
                autoCorrect="off"
              />
              <Button className="min-h-11 w-full sm:min-h-10" onClick={addManual} disabled={busy || !url}>
                Connect
              </Button>
            </div>
          </div>
        </div>
      </section>
    </div>
  );
}

function hostOf(url: string): string {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}

type GwState = 'checking' | 'online' | 'offline' | 'auth';
interface GwHealth {
  state: GwState;
  ms?: number;
}

interface GwHealthView {
  state: GwState;
  glyph: string;
  label: string;
  ms?: number;
  dotClass: string;
  textClass: string;
}

function healthView(h?: GwHealth): GwHealthView {
  const state = h?.state ?? 'checking';
  switch (state) {
    case 'online':
      return { state, glyph: '\u25cf', label: 'Online', ms: h?.ms, dotClass: 'text-ok', textClass: 'text-ok' };
    case 'offline':
      return { state, glyph: '\u25cf', label: 'Offline', ms: h?.ms, dotClass: 'text-err', textClass: 'text-err' };
    case 'auth':
      return { state, glyph: '\u25cf', label: 'Unauthorized', ms: h?.ms, dotClass: 'text-warn-strong', textClass: 'text-warn-strong' };
    default:
      return { state, glyph: '\u25cc', label: 'Checking\u2026', ms: h?.ms, dotClass: 'text-dialog-hint', textClass: 'text-dialog-hint' };
  }
}

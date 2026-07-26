import { useCallback, useEffect, useRef, useState } from 'react';
import type { GatewayConn } from '../lib/types';
import { GatewayClient, GatewayError } from '../lib/gateway';
import { parsePairing } from '../lib/pairing';
import { scanQr } from '../lib/scan';
import { Banner, Button, Input } from '../components/ui';

interface Props {
  conns: GatewayConn[];
  active: GatewayConn | null;
  onAdd: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  onSettings: (conn: GatewayConn) => void;
}

/**
 * Last probed verdict per gateway URL, kept for the tab's lifetime. This screen
 * unmounts whenever you leave the Connect tab; without this, every return would
 * repaint each gateway as a pulsing "Checking…" until the next ping lands.
 */
const lastHealth: Record<string, GwHealth> = {};

export function ConnectScreen({
  conns,
  active,
  onAdd,
  onSettings,
}: Props) {
  const [payload, setPayload] = useState('');
  const [url, setUrl] = useState('');
  const [token, setToken] = useState('');
  const [msg, setMsg] = useState<{ kind: 'ok' | 'err'; text: string } | null>(null);
  const [busy, setBusy] = useState(false);
  const [health, setHealth] = useState<Record<string, GwHealth>>(() => ({ ...lastHealth }));
  const probeInFlight = useRef(false);

  // One sweep at a time: a probe fans out to EVERY saved gateway, and an
  // unreachable one only settles when its request times out — well past the 6s
  // tick. Without this guard those sweeps overlap and multiply.
  const probe = useCallback(async (list: GatewayConn[]) => {
    if (probeInFlight.current) return;
    probeInFlight.current = true;
    // One writer for both the rendered state and the cross-mount cache, so the
    // dots survive leaving and re-entering this tab.
    const remember = (url: string, entry: GwHealth) => {
      lastHealth[url] = entry;
      setHealth((h) => ({ ...h, [url]: entry }));
    };
    try {
      await Promise.all(
        list.map(async (conn) => {
          remember(conn.url, {
            state: lastHealth[conn.url]?.state ?? 'checking',
            ms: lastHealth[conn.url]?.ms,
          });
          const started = Date.now();
          try {
            const reachable = await new GatewayClient(conn).ping();
            remember(conn.url, {
              state: reachable ? 'online' : 'offline',
              ms: Date.now() - started,
            });
          } catch (e) {
            const unauthorized = e instanceof GatewayError && e.status === 401;
            remember(conn.url, {
              state: unauthorized ? 'auth' : 'offline',
              ms: Date.now() - started,
            });
          }
        }),
      );
    } finally {
      probeInFlight.current = false;
    }
  }, []);

  // Live reachability: probe every saved gateway on mount, then every 6s, so the
  // list shows which gateways are actually online, offline, or unauthorized —
  // not just which one is selected.
  useEffect(() => {
    if (conns.length === 0) return;
    void probe(conns);
    const id = window.setInterval(() => {
      if (document.visibilityState === 'hidden') return;
      void probe(conns);
    }, 6000);
    return () => window.clearInterval(id);
  }, [conns, probe]);

  // Handshake first: never save a gateway we cannot reach. `ping()` returns true
  // on a 2xx /healthz, throws GatewayError(401) when reachable-but-unauthorized, and
  // returns false on a genuine network failure. We only persist a gateway that
  // actually answered — an unreachable URL/QR is rejected with a clear reason.
  async function tryConn(conn: GatewayConn) {
    setBusy(true);
    setMsg(null);
    try {
      const client = new GatewayClient(conn);
      const reachable = await client.ping();
      if (!reachable) {
        setMsg({
          kind: 'err',
          text: `Can't reach ${hostOf(conn.url)}. Check the URL, that you're on the same network/Tailscale, and that the gateway is running.`,
        });
        return;
      }
      await onAdd(conn);
      setMsg({ kind: 'ok', text: `Connected to ${conn.label ?? hostOf(conn.url)}` });
      setPayload('');
      setUrl('');
      setToken('');
    } catch (e) {
      if (e instanceof GatewayError && e.status === 401) {
        setMsg({
          kind: 'err',
          text: `${hostOf(conn.url)} is reachable but rejected the token. Check the bearer token from \u2018vis gateway pair\u2019.`,
        });
        return;
      }
      setMsg({ kind: 'err', text: `Can't reach ${hostOf(conn.url)}: ${(e as Error).message}` });
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
      <header className="flex items-end justify-between gap-4 border-b border-dialog-edge pb-3">
        <h1 className="font-mono text-subhead font-black text-white">Gateways</h1>
        {conns.length > 0 && (
          <span className="shrink-0 font-mono text-meta text-dialog-hint">
            {conns.length} saved
          </span>
        )}
      </header>

      {msg && <Banner kind={msg.kind === 'ok' ? 'ok' : 'err'}>{msg.text}</Banner>}

      {conns.length > 0 && (
        <section className="overflow-hidden border border-dialog-edge bg-panel shadow-none sm:shadow-[4px_4px_0_var(--dialog-shadow)]">
          <header className="flex min-h-9 items-center bg-dialog-title px-3 py-2 text-dialog-title-foreground">
            <h2 className="font-mono text-body font-black uppercase tracking-[0.12em]">Saved gateways</h2>
          </header>
          <div className="divide-y divide-dialog-edge border-t border-dialog-edge">
            {conns.map((conn) => {
              const selected = active?.url === conn.url;
              const hv = healthView(health[conn.url]);
              return (
                <button
                  type="button"
                  key={conn.url}
                  onClick={() => onSettings(conn)}
                  className={`flex w-full min-w-0 items-center gap-3 px-2 py-2.5 text-left transition-[background-color,transform] duration-150 hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none sm:px-3 ${
                    selected ? 'border-l-2 border-accent bg-panel-2' : 'border-l-2 border-transparent bg-panel'
                  }`}
                >
                  <span
                    className={`shrink-0 font-mono text-title ${hv.dotClass} ${hv.state === 'checking' ? 'animate-pulse' : ''}`}
                    aria-hidden="true"
                    title={hv.label}
                  >
                    {hv.glyph}
                  </span>
                  <span className="min-w-0 flex-1">
                    <span className="flex min-w-0 items-center gap-2">
                      <span className="truncate font-mono text-body font-bold text-white">
                        {conn.label ?? hostOf(conn.url)}
                      </span>
                      {selected && (
                        <span className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-accent-ink">
                          Active
                        </span>
                      )}
                    </span>
                    <span className="flex min-w-0 items-center gap-2">
                      <span className="block truncate font-mono text-chip text-dialog-hint">
                        {conn.url}
                      </span>
                      <span className={`shrink-0 font-mono text-chip font-bold uppercase tracking-wider ${hv.textClass}`}>
                        {hv.state === 'online'
                          ? (hv.ms != null ? `${hv.ms}ms` : '')
                          : hv.label}
                      </span>
                    </span>
                  </span>
                  <span
                    className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-dialog-hint"
                    aria-hidden="true"
                  >
                    Settings ›
                  </span>
                </button>
              );
            })}
          </div>
        </section>
      )}

      <section className="transition-[opacity,transform] delay-75 duration-200 starting:translate-y-1 starting:opacity-0 motion-reduce:transition-none">
        <div className="mb-3 flex items-center gap-3">
          <h2 className="font-mono text-body font-black uppercase tracking-[0.12em] text-white">
            Add a gateway
          </h2>
          <span className="h-px flex-1 bg-dialog-edge" />
        </div>

        <div className="grid min-w-0 items-start gap-3 md:grid-cols-2">
          <div className="overflow-hidden border border-dialog-edge bg-panel transition-colors focus-within:border-accent">
            <header className="border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
              <h3 className="font-mono text-body font-bold text-white">Pairing link</h3>
              <p className="mt-0.5 text-ui text-dialog-hint">
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
                <Button className="min-h-9 flex-1 sm:min-h-8" onClick={addFromPayload} disabled={busy || !payload}>
                  {busy ? 'Checking\u2026' : 'Pair'}
                </Button>
                <Button variant="ghost" className="min-h-9 sm:min-h-8" onClick={scan} disabled={busy}>
                  Scan QR
                </Button>
              </div>
            </div>
          </div>

          <div className="overflow-hidden border border-dialog-edge bg-panel transition-colors focus-within:border-accent">
            <header className="border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
              <h3 className="font-mono text-body font-bold text-white">URL + token</h3>
              <p className="mt-0.5 text-ui text-dialog-hint">
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
              <Button className="min-h-9 w-full sm:min-h-8" onClick={addManual} disabled={busy || !url}>
                {busy ? 'Checking\u2026' : 'Connect'}
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
      return { state, glyph: '\u25cf', label: 'Online', ms: h?.ms, dotClass: 'text-ok', textClass: 'text-dialog-hint' };
    case 'offline':
      return { state, glyph: '\u25cf', label: 'Offline', ms: h?.ms, dotClass: 'text-err', textClass: 'text-err' };
    case 'auth':
      return { state, glyph: '\u25cf', label: 'Unauthorized', ms: h?.ms, dotClass: 'text-warn-strong', textClass: 'text-warn-strong' };
    default:
      return { state, glyph: '\u25cc', label: 'Checking\u2026', ms: h?.ms, dotClass: 'text-dialog-hint', textClass: 'text-dialog-hint' };
  }
}

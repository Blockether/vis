import { Suspense, lazy, useCallback, useEffect, useRef, useState } from 'react';
import type { GatewayConn } from '../lib/types';
import { GatewayClient, GatewayError } from '../lib/gateway';
import { parsePairing } from '../lib/pairing';
import {
  REACH_LABEL,
  bestAddress,
  mergeAddresses,
  normalizeGatewayUrl,
  reachOf,
} from '../lib/endpoints';
import { Banner, Button, DialogHeader, Input } from '../components/ui';
import { ChevronIcon } from '../components/icons';

// The QR scanner drags in jsqr (~250 kB of source, a fifth of the launch chunk)
// plus the getUserMedia/canvas plumbing, and it is only ever mounted after an
// explicit tap on "Scan". Keep it out of the launch chunk and warm it on idle
// once this screen is up, so the tap still opens the camera immediately.
const QrScanner = lazy(() =>
  import('../components/QrScanner').then((m) => ({ default: m.QrScanner })),
);

function prefetchScanner() {
  void import('../components/QrScanner');
}

interface Props {
  conns: GatewayConn[];
  active: GatewayConn | null;
  primary: GatewayConn | null;
  onAdd: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  onSettings: (conn: GatewayConn) => void;
  /** Why the active gateway was dropped, when it stopped answering. */
  offlineError?: string | null;
  /** Retry the active gateway and go back to sessions if it answers. */
  onRetry?: () => void;
}

/**
 * Last probed verdict per gateway URL, kept for the tab's lifetime. This screen
 * unmounts whenever you leave the Connect tab; without this, every return would
 * repaint each gateway as a pulsing "Checking…" until the next ping lands.
 */
const lastHealth: Record<string, GwHealth> = {};

/**
 * How long ONE reachability probe gets before the address counts as unreachable.
 *
 * Without an explicit deadline the probe inherits the platform's TCP timeout —
 * over a minute on iOS — and because the sweep below is serialised behind
 * `probeInFlight`, a single dead address freezes EVERY other gateway's dot at
 * its last verdict for that whole minute. A tailnet address that needs a
 * handshake or a DERP fallback is exactly the address that gets stuck red.
 */
const PROBE_TIMEOUT_MS = 9000;

/**
 * A red dot with no reason is undiagnosable: "offline" looks identical whether
 * the phone denied local-network access, the tunnel never came up, iOS killed
 * the socket, or the gateway answered with an HTTP error. So the probe reports
 * WHY, and the row prints it.
 *
 * An HTTP answer — any status — proves the address routes here; only a
 * transport failure means the address is unusable from this device.
 */
async function probeOnce(conn: GatewayConn): Promise<{ state: GwState; why?: string }> {
  const ctrl = new AbortController();
  const deadline = setTimeout(() => ctrl.abort(), PROBE_TIMEOUT_MS);
  try {
    await new GatewayClient(conn).health(ctrl.signal);
    return { state: 'online' };
  } catch (e) {
    if (e instanceof GatewayError) {
      // `status === 0` is this client's marker for "no HTTP answer at all" — a
      // transport failure or the deadline above — and it is NOT evidence that
      // the address routes here. Reading it as online painted a dead gateway
      // green with a latency, and, worse, told the shell the gateway had
      // recovered: that un-gated the session list, which failed, which gated it
      // again, mounting and failing thousands of times per second.
      if (e.status === 401) return { state: 'auth' };
      if (e.status > 0) return { state: 'online', why: e.message };
      return { state: 'offline', why: e.message };
    }
    if (ctrl.signal.aborted) return { state: 'offline', why: `no answer in ${PROBE_TIMEOUT_MS / 1000}s` };
    return { state: 'offline', why: (e as Error).message || 'network error' };
  } finally {
    clearTimeout(deadline);
  }
}

export function ConnectScreen({
  conns,
  active,
  primary,
  onAdd,
  onSettings,
  offlineError,
  onRetry,
}: Props) {
  const [payload, setPayload] = useState('');
  const [url, setUrl] = useState('');
  const [token, setToken] = useState('');
  const [msg, setMsg] = useState<{ kind: 'ok' | 'err'; text: string } | null>(null);
  const [busy, setBusy] = useState(false);
  const [scanning, setScanning] = useState(false);
  const [health, setHealth] = useState<Record<string, GwHealth>>(() => ({ ...lastHealth }));
  const probeInFlight = useRef(false);

  // The live probe is the ONLY truth about reachability. Refs (not deps) so the
  // 6s sweep below never closes over a stale active gateway or handler.
  const activeUrlRef = useRef<string | null>(null);
  const recoverRef = useRef<(() => void) | undefined>(undefined);
  activeUrlRef.current = active?.url ?? null;
  recoverRef.current = offlineError ? onRetry : undefined;

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
      // Answering again IS being back: clear the offline gate the moment the
      // active gateway pings, instead of showing a red banner over a green dot.
      if (entry.state === 'online' && url === activeUrlRef.current) recoverRef.current?.();
    };
    try {
      await Promise.all(
        list.map(async (conn) => {
          remember(conn.url, {
            state: lastHealth[conn.url]?.state ?? 'checking',
            ms: lastHealth[conn.url]?.ms,
          });
          const started = Date.now();
          const { state, why } = await probeOnce(conn);
          remember(conn.url, { state, ms: Date.now() - started, ...(why ? { why } : {}) });
        }),
      );
    } finally {
      probeInFlight.current = false;
    }
  }, []);

  // Live reachability: probe every saved gateway on mount, then every 6s, so the
  // list shows which gateways are actually online, offline, or unauthorized —
  // not just which one is selected.
  // `conns` is a fresh array on every parent refresh, and the parent refreshes
  // whenever a gateway answers or fails, so keying this effect on the array
  // identity restarted the interval AND fired an extra sweep per refresh — a
  // dead gateway turned that into a probe storm. Key on the addresses instead
  // and read the live list from a ref.
  const connsRef = useRef(conns);
  connsRef.current = conns;
  const connsKey = conns.map((conn) => conn.url).join('\n');
  useEffect(() => {
    if (connsKey === '') return;
    void probe(connsRef.current);
    const id = window.setInterval(() => {
      if (document.visibilityState === 'hidden') return;
      void probe(connsRef.current);
    }, 6000);
    return () => window.clearInterval(id);
  }, [connsKey, probe]);

  // Warm the scanner chunk once this screen is idle. Whoever is here is one tap
  // away from "Scan", and the fetch is off the launch critical path.
  useEffect(() => {
    const ric = window.requestIdleCallback;
    if (typeof ric !== 'function') {
      const id = window.setTimeout(prefetchScanner, 1200);
      return () => window.clearTimeout(id);
    }
    const handle = ric(prefetchScanner, { timeout: 3000 });
    return () => window.cancelIdleCallback?.(handle);
  }, []);

  // Handshake first: never save a gateway we cannot reach. `ping()` returns true
  // on a 2xx /healthz, throws GatewayError(401) when reachable-but-unauthorized, and
  // returns false on a genuine network failure. We only persist a gateway that
  // actually answered — an unreachable URL/QR is rejected with a clear reason.
  //
  // A pairing payload carries `alts` (the gateway's other reachable hosts:
  // Tailscale, LAN, tunnel). Every candidate is probed CONCURRENTLY and the most
  // DURABLE one that answers wins — not the first to answer. Pairing happens
  // standing next to the machine, where the LAN address is always the quickest
  // to reply; saving that one pins the app to an address that dies the moment
  // the phone leaves the house. All candidates are kept on the connection so a
  // later failover/upgrade has somewhere to go.
  async function tryConn(conn: GatewayConn) {
    setBusy(true);
    setMsg(null);
    const { alts, ...base } = conn;
    const candidates = mergeAddresses([base.url], alts);
    let unauthorized = false;
    try {
      const reachable = (
        await Promise.all(
          candidates.map(async (url) => {
            try {
              return (await new GatewayClient({ ...base, url }).ping()) ? url : null;
            } catch (e) {
              if (e instanceof GatewayError && e.status === 401) unauthorized = true;
              return null;
            }
          }),
        )
      ).filter((url): url is string => url !== null);

      const chosen = bestAddress(reachable);
      if (chosen) {
        const candidate: GatewayConn = {
          ...base,
          url: chosen,
          label: hostLabel(chosen),
          ...(candidates.length > 1 ? { alts: candidates } : {}),
        };
        await onAdd(candidate);
        const via = REACH_LABEL[reachOf(chosen)].toLowerCase();
        setMsg({ kind: 'ok', text: `Connected to ${candidate.label} (${via})` });
        setPayload('');
        setUrl('');
        setToken('');
        return;
      }
      if (unauthorized) {
        setMsg({
          kind: 'err',
          text: `${hostOf(conn.url)} is reachable but rejected the token. Check the bearer token from \u2018vis gateway pair\u2019.`,
        });
        return;
      }
      setMsg({
        kind: 'err',
        text:
          candidates.length > 1
            ? `Can't reach ${candidates.map(hostOf).join(' or ')}. Check you're on the same network/Tailscale and that vis is running there.`
            : `Can't reach ${hostOf(conn.url)}. Check the URL, that you're on the same network/Tailscale, and that vis is running on that machine.`,
      });
    } catch (e) {
      setMsg({ kind: 'err', text: `Can't reach ${hostOf(conn.url)}: ${(e as Error).message}` });
    } finally {
      setBusy(false);
    }
  }

  function hostLabel(url: string): string {
    return hostOf(url);
  }

  async function addFromPayload() {
    const conn = parsePairing(payload);
    if (!conn) {
      setMsg({ kind: 'err', text: 'Not a vis:// pairing link or machine URL' });
      return;
    }
    await tryConn(conn);
  }

  async function addManual() {
    // A typed address may skip the scheme: supply it instead of refusing.
    const u = normalizeGatewayUrl(url);
    if (!u) {
      setMsg({ kind: 'err', text: `"${url.trim()}" is not a machine address` });
      return;
    }
    await tryConn({ url: u, token: token.trim() || undefined, label: hostOf(u) });
  }

  async function onScanned(raw: string) {
    setScanning(false);
    const conn = parsePairing(raw);
    if (!conn) {
      setMsg({ kind: 'err', text: 'QR is not a Vis pairing code' });
      return;
    }
    await tryConn(conn);
  }

  return (
    <div className="mx-auto w-full max-w-[1400px] space-y-5 px-[max(0.75rem,env(safe-area-inset-left))] pb-[max(2rem,env(safe-area-inset-bottom))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-4 transition-[opacity,translate] duration-300 ease-[cubic-bezier(0.22,0.61,0.36,1)] starting:translate-y-1.5 starting:opacity-0 motion-reduce:transition-none sm:space-y-6 sm:px-6 sm:py-6">
      {scanning && (
        <Suspense fallback={null}>
          <QrScanner
            onResult={(raw) => void onScanned(raw)}
            onCancel={() => setScanning(false)}
          />
        </Suspense>
      )}

      {conns.length > 0 && (
        <section className="overflow-hidden border border-dialog-edge bg-panel shadow-none sm:shadow-[4px_4px_0_var(--dialog-shadow)]">
          <DialogHeader title="Saved machines" />
          <div className="divide-y divide-dialog-edge border-t border-dialog-edge">
            {conns.map((conn) => {
              const selected = active?.url === conn.url;
              const isPrimary = primary?.url === conn.url;
              const hv = healthView(health[conn.url]);
              return (
                <button
                  type="button"
                  key={conn.url}
                  onClick={() => onSettings(conn)}
                  className={`flex w-full min-w-0 items-center gap-3 px-2 py-2.5 text-left transition-colors duration-150 hover:bg-hover focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-inset focus-visible:ring-accent/60 motion-reduce:transition-none sm:px-3 ${
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
                      {isPrimary && (
                        <span className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-accent-ink">
                          Primary
                        </span>
                      )}
                      {selected && (
                        <span className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-dialog-hint">
                          Current
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
                      {health[conn.url]?.why && hv.state !== 'online' && (
                        <span className="min-w-0 truncate font-mono text-chip text-dialog-hint">
                          {health[conn.url]?.why}
                        </span>
                      )}
                    </span>
                  </span>
                  <span
                    className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-dialog-hint"
                    aria-hidden="true"
                  >
                    Settings
                  </span>
                  <ChevronIcon className="size-3 text-dialog-hint" aria-hidden />
                </button>
              );
            })}
          </div>
        </section>
      )}

      {/* No entry transition of its own: this screen already fades in as ONE
          surface (the container above). A nested @starting-style fade multiplies
          with the parent's, and the two cards read as a flicker inside a page
          that is itself still fading in. */}
      <section>
        <div className="mb-3 flex items-center gap-3">
          <h2 className="font-mono text-body font-black uppercase tracking-[0.12em] text-white">
            Add a machine
          </h2>
          <span className="h-px flex-1 bg-dialog-edge" />
        </div>

        <div className="grid min-w-0 items-start gap-3 md:grid-cols-2">
          <div className="overflow-hidden border border-dialog-edge bg-panel transition-colors focus-within:border-accent">
            <header className="border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
              <h3 className="font-mono text-body font-bold text-white">Pairing link</h3>
              <p className="mt-0.5 text-ui text-dialog-hint">
                Paste the link from <code className="text-accent-ink">vis gateway pair</code> on
                that machine.
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
                <Button
                  className="flex-1"
                  onClick={addFromPayload}
                  disabled={busy || !payload}
                >
                  {busy ? 'Checking\u2026' : 'Pair'}
                </Button>
                <Button variant="ghost" onClick={() => setScanning(true)} disabled={busy}>
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
                placeholder="my-machine.example.com or 10.0.0.5:7890"
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
              <Button
                className="w-full"
                onClick={addManual}
                disabled={busy || !url}
              >
                {busy ? 'Checking\u2026' : 'Connect'}
              </Button>
            </div>
          </div>
        </div>

        {msg && (
          <div className="mt-3">
            <Banner kind={msg.kind === 'ok' ? 'ok' : 'err'}>{msg.text}</Banner>
          </div>
        )}
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
  /** Why the last probe failed — printed on the row so red is diagnosable. */
  why?: string;
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

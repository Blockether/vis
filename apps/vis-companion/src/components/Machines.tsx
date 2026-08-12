import { Suspense, lazy, useCallback, useEffect, useRef, useState } from 'react';
import type { GatewayConn } from '../lib/types';
import { GatewayClient, GatewayError } from '../lib/gateway';
import { parsePairing } from '../lib/pairing';
import {
  REACH_LABEL,
  bestAddress,
  hostOf,
  mergeAddresses,
  normalizeGatewayUrl,
  reachOf,
} from '../lib/endpoints';
import { Banner, Button, Input, ListRow } from './ui';
import { ChevronIcon } from './icons';

/**
 * THE MACHINES THIS DEVICE IS PAIRED WITH, AND THE WAY TO ADD ONE — two pieces,
 * not a screen.
 *
 * They used to be welded into `ConnectScreen`, which the shell reached only when
 * nothing was paired or the active gateway had died. Everywhere else the list of
 * machines was a switcher of bare names inside Settings, and pairing was a button
 * there that CLOSED the dialog and navigated to that screen. So the answer to
 * "which machines does this app know, and how do I add one?" lived behind a door
 * the app bar did not have — reported as: this should open when I click the cog.
 *
 * The pieces live here so the cog's Settings dialog and the unpaired/offline screen
 * render the SAME list and the SAME pairing controls, with one probe implementation
 * behind both.
 */

// The QR scanner drags in jsqr (~250 kB of source, a fifth of the launch chunk)
// plus the getUserMedia/canvas plumbing, and it is only ever mounted after an
// explicit tap on "Scan". Keep it out of the launch chunk and warm it on idle
// once these controls are up, so the tap still opens the camera immediately.
const QrScanner = lazy(() =>
  import('./QrScanner').then((m) => ({ default: m.QrScanner })),
);

function prefetchScanner() {
  void import('./QrScanner');
}

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

export type GwState = 'checking' | 'online' | 'offline' | 'auth';

export interface GwHealth {
  state: GwState;
  ms?: number;
  /** Why the last probe failed — printed on the row so red is diagnosable. */
  why?: string;
}

/**
 * Last probed verdict per gateway URL, kept for the app's lifetime. Both callers
 * unmount whenever their surface closes; without this, every return would repaint
 * each gateway as a pulsing "Checking…" until the next ping lands.
 */
const lastHealth: Record<string, GwHealth> = {};

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

/**
 * Live reachability for every saved machine: probe on mount, then every 6s, so a
 * list shows which gateways are actually online, offline, or unauthorized — not
 * just which one is selected.
 *
 * `onRecovered` is how the offline gate lifts itself: answering again IS being
 * back, so the caller that put up the gate clears it the moment its own gateway
 * pings, instead of showing a red banner over a green dot.
 */
export function useFleetHealth(
  conns: GatewayConn[],
  watch?: { url?: string | null; onRecovered?: () => void },
): Record<string, GwHealth> {
  const [health, setHealth] = useState<Record<string, GwHealth>>(() => ({ ...lastHealth }));
  const probeInFlight = useRef(false);

  // The live probe is the ONLY truth about reachability. Refs (not deps) so the
  // 6s sweep below never closes over a stale watched gateway or handler.
  const watchedUrlRef = useRef<string | null>(null);
  const recoverRef = useRef<(() => void) | undefined>(undefined);
  watchedUrlRef.current = watch?.url ?? null;
  recoverRef.current = watch?.onRecovered;

  // One sweep at a time: a probe fans out to EVERY saved gateway, and an
  // unreachable one only settles when its request times out — well past the 6s
  // tick. Without this guard those sweeps overlap and multiply.
  const probe = useCallback(async (list: GatewayConn[]) => {
    if (probeInFlight.current) return;
    probeInFlight.current = true;
    // One writer for both the rendered state and the cross-mount cache, so the
    // dots survive leaving and re-entering the surface.
    const remember = (url: string, entry: GwHealth) => {
      lastHealth[url] = entry;
      setHealth((h) => ({ ...h, [url]: entry }));
      if (entry.state === 'online' && url === watchedUrlRef.current) recoverRef.current?.();
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

  return health;
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

/**
 * THE PAIRED MACHINES, one pressable row each: what it is called, where it
 * answers, how fast, and whether this device is using it right now.
 *
 * `selectedUrl` is what the surface is SHOWING (the settings column's machine);
 * `activeUrl` is what the app is USING. They are the same row most of the time
 * and different exactly when you are reading another machine's settings, so they
 * are two marks rather than one.
 */
export function MachineRows({
  conns,
  selectedUrl,
  activeUrl,
  primaryUrl,
  health,
  onPick,
  actionLabel,
}: {
  conns: GatewayConn[];
  selectedUrl?: string | null;
  activeUrl?: string | null;
  primaryUrl?: string | null;
  health: Record<string, GwHealth>;
  onPick: (conn: GatewayConn) => void;
  /** The word on the trailing edge when the row LEAVES for somewhere else. */
  actionLabel?: string;
}) {
  return (
    <div className="divide-y divide-dialog-edge">
      {conns.map((conn) => {
        const hv = healthView(health[conn.url]);
        return (
          <ListRow
            key={conn.url}
            isSelected={conn.url === selectedUrl}
            onClick={() => onPick(conn)}
            className="gap-3"
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
                {conn.url === primaryUrl && (
                  <span className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-accent-ink">
                    Primary
                  </span>
                )}
                {conn.url === activeUrl && (
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
            {actionLabel && (
              <span
                className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-dialog-hint"
                aria-hidden="true"
              >
                {actionLabel}
              </span>
            )}
            {actionLabel && <ChevronIcon className="size-3 text-dialog-hint" aria-hidden />}
          </ListRow>
        );
      })}
    </div>
  );
}

/**
 * THE TWO WAYS IN: the pairing link (or its QR) printed by `vis gateway pair`, and
 * an address typed by hand. Both end in the same handshake, so both report the same
 * reason when a machine cannot be reached.
 */
export function AddMachine({
  onAdd,
  isStacked = false,
}: {
  onAdd: (conn: GatewayConn, makeActive?: boolean) => Promise<void>;
  /**
   * The two ways in sit side by side on a page and STACK inside a settings column:
   * a media query cannot see that the column is half a dialog, so on a desktop the
   * pair of cards split 446px between them and the field for a pairing LINK came
   * out 175px wide.
   */
  isStacked?: boolean;
}) {
  const [payload, setPayload] = useState('');
  const [url, setUrl] = useState('');
  const [token, setToken] = useState('');
  const [msg, setMsg] = useState<{ kind: 'ok' | 'err'; text: string } | null>(null);
  const [busy, setBusy] = useState(false);
  const [scanning, setScanning] = useState(false);

  // Warm the scanner chunk once these controls are idle. Whoever is here is one
  // tap away from "Scan", and the fetch is off the launch critical path.
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
          candidates.map(async (candidate) => {
            try {
              return (await new GatewayClient({ ...base, url: candidate }).ping()) ? candidate : null;
            } catch (e) {
              if (e instanceof GatewayError && e.status === 401) unauthorized = true;
              return null;
            }
          }),
        )
      ).filter((candidate): candidate is string => candidate !== null);

      const chosen = bestAddress(reachable);
      if (chosen) {
        const candidate: GatewayConn = {
          ...base,
          url: chosen,
          label: hostOf(chosen),
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

  return (
    <div className="min-w-0">
      {scanning && (
        <Suspense fallback={null}>
          <QrScanner
            onResult={(raw) => {
              setScanning(false);
              const conn = parsePairing(raw);
              if (!conn) {
                setMsg({ kind: 'err', text: 'QR is not a Vis pairing code' });
                return;
              }
              void tryConn(conn);
            }}
            onCancel={() => setScanning(false)}
          />
        </Suspense>
      )}

      <div
        className={`grid min-w-0 items-start gap-3 ${isStacked ? '' : 'md:grid-cols-2'}`}
      >
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
              <Button variant="secondary" onClick={() => setScanning(true)} disabled={busy}>
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
    </div>
  );
}

import { Suspense, lazy, useCallback, useEffect, useRef, useState } from 'react';
import type { GatewayConn } from '../lib/types';
import { GatewayClient, GatewayError } from '../lib/gateway';
import { parsePairing } from '../lib/pairing';
import {
  REACH_LABEL,
  hostOf,
  mergeAddresses,
  normalizeGatewayUrl,
  reachOf,
} from '../lib/endpoints';
import { onWake } from '../lib/wake';
import { warm } from '../lib/warm';
import { Banner, Button, ConfirmRow, Input, ListRow, Spinner } from './ui';
import { ChevronIcon, PencilIcon, StarIcon, TrashIcon } from './icons';
import { SwipeActions, type SwipeAction } from './SwipeActions';

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
  warm(import('./QrScanner'));
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

/** How often the sweep re-asks every saved gateway whether it is still there. */
const PROBE_INTERVAL_MS = 6000;

/**
 * How long a verdict counts as EVIDENCE. Past this it is only a memory of a
 * gateway, and a memory must never be painted as a fact: a laptop whose lid
 * closes stops answering without telling anyone, so the last thing this app
 * measured — "online, 50ms" — stays literally true and completely wrong.
 *
 * One sweep period plus one probe deadline is the longest a machine that is
 * still answering can go without a fresh verdict, so anything older than that
 * is not late, it is unknown, and the row says so.
 */
const HEALTH_FRESH_MS = PROBE_INTERVAL_MS + PROBE_TIMEOUT_MS;

export type GwState = 'checking' | 'online' | 'offline' | 'auth';

export interface GwHealth {
  state: GwState;
  /** When this verdict was MEASURED. Required: a verdict with no age is a claim. */
  at: number;
  ms?: number;
  /** Why the last probe failed — printed on the row so red is diagnosable. */
  why?: string;
}

/**
 * Last probed verdict per gateway URL, kept for the app's lifetime. Both callers
 * unmount whenever their surface closes; without this, every return would repaint
 * each gateway as a pulsing "Checking…" until the next ping lands.
 *
 * It is a HEAD START, never a source of truth: `isFresh` decides whether an entry
 * may still be shown, so re-entering a surface hours later shows "Checking…" and
 * not the verdict from the last time the machine was alive.
 */
const lastHealth: Record<string, GwHealth> = {};

/** Is this verdict recent enough to be shown as the machine's state? */
function isFresh(h?: GwHealth): h is GwHealth {
  return h != null && Date.now() - h.at < HEALTH_FRESH_MS;
}

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
    };
    try {
      await Promise.all(
        list.map(async (conn) => {
          // While this probe is in flight the row keeps the previous verdict only
          // while that verdict is still evidence; an expired one becomes
          // "Checking…", because the honest answer to "is it up?" during the
          // first probe after a wake is that nobody knows yet.
          const known = lastHealth[conn.url];
          if (!isFresh(known)) remember(conn.url, { state: 'checking', at: Date.now() });
          const started = Date.now();
          const { state, why } = await probeOnce(conn);
          remember(conn.url, {
            state,
            at: Date.now(),
            ms: Date.now() - started,
            ...(why ? { why } : {}),
          });
          // Recovery is declared by a PROBE, never by the memory of one: the
          // offline gate lifts on this call, and lifting it on a remembered
          // "online" put the shell in a mount/fail/gate loop.
          if (state === 'online' && conn.url === watchedUrlRef.current) recoverRef.current?.();
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
    // No `document.visibilityState` guard: a Capacitor iOS webview keeps
    // reporting `hidden` after the app is already foreground (see `lib/wake`),
    // so that guard skipped the sweep exactly while the reader was looking at
    // the dots. A backgrounded app runs no timers anyway, which is the real
    // reason this does not drain a phone.
    const id = window.setInterval(() => void probe(connsRef.current), PROBE_INTERVAL_MS);
    // A frozen app measures nothing, so every verdict a wake finds is as old as
    // the sleep. Probe on the app's own wake bus rather than waiting out a tick.
    const offWake = onWake(() => void probe(connsRef.current));
    return () => {
      window.clearInterval(id);
      offWake();
    };
  }, [connsKey, probe]);

  return health;
}

interface GwHealthView {
  state: GwState;
  glyph: string;
  label: string;
  ms?: number;
  why?: string;
  dotClass: string;
  textClass: string;
}

/**
 * The face of ONE machine's reachability, and the seam where a verdict stops
 * being shown. Painting the last measurement forever is the report this exists
 * for: the lid closed, nothing has answered since, and the dot stayed green
 * wearing the latency of a machine that was already gone.
 */
function healthView(h?: GwHealth): GwHealthView {
  // A latency and a reason belong to the verdict that measured them, so an
  // expired verdict takes both with it — nothing below reads the stale entry.
  const fresh = isFresh(h) ? h : undefined;
  const state = fresh?.state ?? 'checking';
  const { ms, why } = fresh ?? {};
  switch (state) {
    case 'online':
      return { state, glyph: '\u25cf', label: 'Online', ms, why, dotClass: 'text-ok', textClass: 'text-dialog-hint' };
    case 'offline':
      return { state, glyph: '\u25cf', label: 'Offline', ms, why, dotClass: 'text-err', textClass: 'text-err' };
    case 'auth':
      return { state, glyph: '\u25cf', label: 'Unauthorized', ms, why, dotClass: 'text-warn-strong', textClass: 'text-warn-strong' };
    default:
      return { state, glyph: '\u25cc', label: 'Checking\u2026', ms, why, dotClass: 'text-dialog-hint', textClass: 'text-dialog-hint' };
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
 *
 * A MACHINE'S OWN VERBS WAIT UNDER ITS OWN ROW, and a slide is what reaches
 * them. They were a `Saved connection` panel under the list first — a name field,
 * `Make primary` and `Forget this machine` standing permanently open, three
 * controls for ONE machine, acting on whichever row the column happened to be
 * reading rather than on the row under the thumb. Then a left swipe with a `⋯`
 * beside it, and the mark is what was reported: it says nothing and opens a menu
 * holding exactly what the gesture already holds. Then a strip of full-width
 * WORDS under the one row being read — a second list, not a row's verbs — and
 * then marks painted permanently in every row's trailing cell, which took the
 * width the machine's own name and address came for. The slide is the part the
 * reports kept asking for: `SwipeActions`, the same strip a session row carries,
 * with nothing standing beside it. Rename still edits IN the row and Forget still
 * asks IN it, so neither verb opens a surface over the list it acts on.
 *
 * A verb exists here only when its handler does, so `ConnectScreen`'s list —
 * where a row is a place to GO, not a thing to manage — stays exactly as it was.
 */
export function MachineRows({
  conns,
  selectedUrl,
  activeUrl,
  primaryUrl,
  health,
  onPick,
  actionLabel,
  onMakePrimary,
  onRename,
  onForget,
}: {
  conns: GatewayConn[];
  selectedUrl?: string | null;
  activeUrl?: string | null;
  primaryUrl?: string | null;
  health: Record<string, GwHealth>;
  onPick: (conn: GatewayConn) => void;
  /** The word on the trailing edge when the row LEAVES for somewhere else. */
  actionLabel?: string;
  /** Rank this machine first: the app opens on it, and the row wears `PRIMARY`. */
  onMakePrimary?: (conn: GatewayConn) => void | Promise<void>;
  /** The name THIS DEVICE shows; `undefined` gives the machine its host back. */
  onRename?: (conn: GatewayConn, label: string | undefined) => void | Promise<void>;
  /** Deletes this machine's address and token from this device. */
  onForget?: (conn: GatewayConn) => void | Promise<void>;
}) {
  const [renaming, setRenaming] = useState<string | null>(null);
  const [draft, setDraft] = useState('');
  const [forgetting, setForgetting] = useState<string | null>(null);

  // Escape unwinds THIS row's own surface first. Settings closes itself on an
  // Escape it hears on the window, so a rename opened inside it used to leave with
  // the whole dialog on one keystroke; a capture listener always runs before
  // that one, whatever order the two mounted in.
  useEffect(() => {
    if (renaming === null && forgetting === null) return;
    const onKey = (event: KeyboardEvent) => {
      if (event.key !== 'Escape') return;
      event.stopPropagation();
      setRenaming(null);
      setForgetting(null);
    };
    window.addEventListener('keydown', onKey, true);
    return () => window.removeEventListener('keydown', onKey, true);
  }, [renaming, forgetting]);

  function startRename(conn: GatewayConn) {
    setDraft(conn.label ?? '');
    setRenaming(conn.url);
  }

  // An unnamed machine wears its host, so an EMPTY field means "no name of its
  // own" — never the host typed back in as a label. Leaving commits, as it does
  // in every other name this app edits in place: a phone keyboard is dismissed
  // far more often than Enter is pressed.
  function commitRename(conn: GatewayConn) {
    setRenaming(null);
    const next = draft.trim() || undefined;
    if (next !== (conn.label ?? undefined)) void onRename?.(conn, next);
  }

  return (
    <div className="divide-y divide-dialog-edge">
      {conns.map((conn) => {
        const hv = healthView(health[conn.url]);
        const name = conn.label ?? hostOf(conn.url);

        if (renaming === conn.url)
          return (
            <div key={conn.url} className="flex min-h-12 items-center px-3 py-2">
              <Input
                autoFocus
                value={draft}
                aria-label={`Rename ${name}`}
                placeholder={hostOf(conn.url)}
                autoCapitalize="none"
                autoCorrect="off"
                onChange={(event) => setDraft(event.target.value)}
                onBlur={() => commitRename(conn)}
                onKeyDown={(event) => {
                  if (event.key === 'Enter') event.currentTarget.blur();
                }}
              />
            </div>
          );

        if (forgetting === conn.url)
          return (
            <div key={conn.url}>
              {/* What forgetting COSTS, in the row it is being asked in: the
                  panel this verb came from spent a paragraph on it, and the
                  answer is worthless without the sentence. */}
              <p className="px-3 pt-2 font-mono text-chip text-dialog-hint">
                Deletes {hostOf(conn.url)} and its access token from this device. Pairing again
                needs the link or QR code from &lsquo;vis gateway pair&rsquo;.
              </p>
              <ConfirmRow
                question={`Forget ${name}?`}
                confirmLabel="Yes, forget"
                onKeep={() => setForgetting(null)}
                onConfirm={() => {
                  setForgetting(null);
                  void onForget?.(conn);
                }}
              />
            </div>
          );

        // THE VERBS OF THIS MACHINE, waiting under its own row's trailing edge and
        // reached by sliding it. A verb exists only when its handler does, so
        // `ConnectScreen`'s list carries none and never slides; the rank verb is
        // missing from the machine that already holds the rank.
        const isReading = conn.url === selectedUrl;
        const actions: SwipeAction[] = [];
        if (onMakePrimary && conn.url !== primaryUrl)
          actions.push({
            key: 'primary',
            label: 'Primary',
            name: `Make ${name} primary`,
            icon: <StarIcon className="size-4" />,
            // The one verb here that is a RANK rather than an edit, so it wears the
            // amber every rank mark in this app wears — the same slab `Star` has.
            tone: 'accent',
            onSelect: () => void onMakePrimary(conn),
          });
        if (onRename)
          actions.push({
            key: 'rename',
            label: 'Rename',
            name: `Rename ${name}`,
            icon: <PencilIcon className="size-4" />,
            onSelect: () => startRename(conn),
          });
        if (onForget)
          actions.push({
            key: 'forget',
            label: 'Forget',
            name: `Forget ${name}`,
            icon: <TrashIcon className="size-4" />,
            tone: 'danger',
            onSelect: () => setForgetting(conn.url),
          });

        return (
          <SwipeActions key={conn.url} label={name} actions={actions}>
            <ListRow
              isSelected={isReading}
              onClick={() => onPick(conn)}
              className="min-w-0 gap-3"
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
                      {name}
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
                    {hv.why && hv.state !== 'online' && (
                      <span className="min-w-0 truncate font-mono text-chip text-dialog-hint">
                        {hv.why}
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
          </SwipeActions>
        );
      })}
    </div>
  );
}

/**
 * The wait BEFORE the camera: the scanner is a lazy chunk, so on a cold tap
 * there is a fetch between "Scan QR" and the scanner's own first frame. With a
 * `null` fallback that fetch was a blank screen with nothing on it at all — the
 * first of the silences this pairing flow was reported for. The scanner narrates
 * every wait it owns; this narrates the one before it exists.
 */
function ScannerOpening({ onCancel }: { onCancel: () => void }) {
  return (
    <div className="fixed inset-0 z-50 flex flex-col bg-black">
      <div className="grid flex-1 place-items-center px-6" role="status" aria-live="polite">
        <div className="flex flex-col items-center gap-2 text-center">
          <span className="font-mono text-display text-accent">
            <Spinner />
          </span>
          <p className="font-mono text-ui text-white">Opening the scanner…</p>
        </div>
      </div>
      <div className="border-t border-dialog-edge bg-panel px-[max(0.75rem,env(safe-area-inset-left))] pb-[max(0.75rem,env(safe-area-inset-bottom))] pr-[max(0.75rem,env(safe-area-inset-right))] pt-3">
        <Button variant="secondary" className="w-full" onClick={onCancel}>
          Cancel
        </Button>
      </div>
    </div>
  );
}

/**
 * One advertised address of the machine being paired, and the last thing this
 * device learned about it. `probing` is a fact with an age like any other
 * verdict here — it is what the panel is counting down against.
 */
interface CandidateProbe {
  url: string;
  state: 'probing' | 'online' | 'unauthorized' | 'unreachable';
  /** Measured round trip, so an address that answered says how well. */
  ms?: number;
  /** Why this address failed — the row prints it, so red is diagnosable. */
  why?: string;
}

/** A pairing attempt in flight: which machine, which addresses, since when. */
interface PairRun {
  /** The machine as the payload named it, before any address has won. */
  label: string;
  candidates: CandidateProbe[];
  startedAt: number;
}

/**
 * Probe ONE advertised address, under this app's own deadline.
 *
 * The client's request timeout is 30s, which is the right budget for a screen
 * asking a gateway it already trusts for real work, and completely wrong for
 * pairing: a QR carries one `alt=` per routable interface on that machine, most
 * of which are virtual bridges no phone can reach, and every one of them cost
 * the full 30s before this had a deadline of its own. `PROBE_TIMEOUT_MS` is the
 * same bound the fleet sweep uses, for the same reason.
 *
 * `health()` rather than `ping()` because `ping` swallows the reason: an address
 * that fails must be able to say WHY on its row. The verdicts are identical —
 * only a 2xx `/healthz` is a pairable address, a 401 is reachable-but-refused.
 */
async function probeCandidate(
  base: Omit<GatewayConn, 'alts'>,
  url: string,
  signal: AbortSignal,
): Promise<Omit<CandidateProbe, 'url'>> {
  const started = Date.now();
  const deadline = new AbortController();
  const timer = window.setTimeout(() => deadline.abort(), PROBE_TIMEOUT_MS);
  const giveUp = () => deadline.abort();
  signal.addEventListener('abort', giveUp, { once: true });
  try {
    await new GatewayClient({ ...base, url }).health(deadline.signal);
    return { state: 'online', ms: Date.now() - started };
  } catch (e) {
    const ms = Date.now() - started;
    if (e instanceof GatewayError && e.status === 401) return { state: 'unauthorized', ms };
    if (signal.aborted) return { state: 'unreachable', ms, why: 'stopped' };
    if (deadline.signal.aborted)
      return { state: 'unreachable', ms, why: `no answer in ${PROBE_TIMEOUT_MS / 1000}s` };
    const why = e instanceof GatewayError ? e.message : (e as Error).message;
    return { state: 'unreachable', ms, why: why || 'no route to this address' };
  } finally {
    window.clearTimeout(timer);
    signal.removeEventListener('abort', giveUp);
  }
}

/**
 * The most DURABLE address that works, decided as early as it can be known.
 *
 * Every candidate is probed at once and each verdict is reported the moment it
 * lands, so the panel fills in rather than sitting on one word. Durability, not
 * speed, picks the winner — pairing happens standing next to the machine, where
 * the LAN address always replies first, and saving that one pins the app to an
 * address that dies at the front door.
 *
 * Because `candidates` arrives most-durable-first, the winner is the first
 * address that answers with nothing unresolved ahead of it: no address still in
 * flight below that rank can beat it, so none is waited on. With the tailnet
 * address up that ends pairing in its own round trip instead of holding the
 * whole run open for every dead bridge address in the QR.
 */
function bestReachable(
  base: Omit<GatewayConn, 'alts'>,
  candidates: readonly string[],
  report: (url: string, verdict: Omit<CandidateProbe, 'url'>) => void,
  signal: AbortSignal,
): Promise<{ chosen?: string; unauthorized: boolean }> {
  return new Promise((resolve) => {
    const verdicts: (boolean | undefined)[] = candidates.map(() => undefined);
    let unauthorized = false;
    let done = false;
    const decide = () => {
      if (done) return;
      for (let i = 0; i < verdicts.length; i += 1) {
        // A more durable address has not answered yet, and it would outrank
        // anything already in hand: nothing can be decided.
        if (verdicts[i] === undefined) return;
        if (verdicts[i]) {
          done = true;
          resolve({ chosen: candidates[i], unauthorized });
          return;
        }
      }
      done = true;
      resolve({ chosen: undefined, unauthorized });
    };
    candidates.forEach((url, index) => {
      void probeCandidate(base, url, signal).then((verdict) => {
        verdicts[index] = verdict.state === 'online';
        if (verdict.state === 'unauthorized') unauthorized = true;
        report(url, verdict);
        decide();
      });
    });
    decide();
  });
}

/** What one candidate row says about itself right now. */
function candidateView(probe: CandidateProbe): { note: string; textClass: string } {
  switch (probe.state) {
    case 'online':
      return { note: probe.ms != null ? `${probe.ms}ms` : 'answered', textClass: 'text-ok' };
    case 'unauthorized':
      return { note: 'token refused', textClass: 'text-warn-strong' };
    case 'unreachable':
      return { note: probe.why ?? 'no answer', textClass: 'text-err' };
    default:
      return { note: 'trying\u2026', textClass: 'text-dialog-hint' };
  }
}

/**
 * WHAT IS HAPPENING WHILE A MACHINE IS BEING PAIRED — reported as it happens.
 *
 * This is the panel the report exists for: the scanner narrates every wait it
 * has, states that iOS may be about to ask for permission and says when it is
 * taking too long, and then it decoded a code and handed off to a probe whose
 * entire account of itself was the word `Checking…` on a button it had just
 * disabled. Which machine, how many addresses it advertised, which of them have
 * already answered, why the others failed and how much longer this can possibly
 * take were all known here and none of it was on screen.
 *
 * So the panel names the machine, lists every advertised address by the reach
 * that makes it durable, marks each one as it settles, and counts the deadline
 * down — a wait with a stated end. `Stop` is there because a wait nobody can
 * end is the same dead screen with a spinner on it.
 */
function PairingProgress({
  run,
  secondsLeft,
  onStop,
}: {
  run: PairRun;
  secondsLeft: number;
  onStop: () => void;
}) {
  const settled = run.candidates.filter((c) => c.state !== 'probing').length;
  const plural = run.candidates.length === 1 ? 'address' : 'addresses';
  return (
    <div
      className="mt-3 overflow-hidden border border-dialog-edge bg-panel"
      role="status"
      aria-live="polite"
    >
      <header className="flex items-center gap-2 border-b border-dialog-edge bg-panel-2 px-3 py-2.5">
        <Spinner tone="accent" />
        <span className="min-w-0 flex-1">
          <span className="block truncate font-mono text-body font-bold text-white">
            Pairing with {run.label}
          </span>
          <span className="block font-mono text-chip text-dialog-hint">
            Trying {run.candidates.length} {plural} · {settled} of{' '}
            {run.candidates.length} answered · up to {secondsLeft}s left
          </span>
        </span>
        <Button variant="secondary" onClick={onStop}>
          Stop
        </Button>
      </header>
      <ul className="divide-y divide-dialog-edge">
        {run.candidates.map((probe) => {
          const view = candidateView(probe);
          return (
            <li key={probe.url} className="flex items-baseline gap-2 px-3 py-2">
              <span className="shrink-0 font-mono text-chip font-black uppercase tracking-wider text-dialog-hint">
                {REACH_LABEL[reachOf(probe.url)]}
              </span>
              <span className="min-w-0 flex-1 truncate font-mono text-chip text-dialog-hint">
                {hostOf(probe.url)}
              </span>
              <span className={`shrink-0 font-mono text-chip font-bold ${view.textClass}`}>
                {view.note}
              </span>
            </li>
          );
        })}
      </ul>
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
  const [msg, setMsg] = useState<{ kind: 'ok' | 'warn' | 'err'; text: string } | null>(null);
  const [busy, setBusy] = useState(false);
  const [scanning, setScanning] = useState(false);
  const [run, setRun] = useState<PairRun | null>(null);
  // The countdown's clock. A stated deadline that does not move is a label, not
  // a report, so this ticks only while a run is live and stops with it.
  const [now, setNow] = useState(0);
  const stopRef = useRef<AbortController | null>(null);

  useEffect(() => {
    if (!run) return undefined;
    setNow(Date.now());
    const id = window.setInterval(() => setNow(Date.now()), 250);
    return () => window.clearInterval(id);
  }, [run?.startedAt]);

  // Nothing may keep probing for a surface that is gone: leaving the dialog
  // mid-pair aborted nothing and the panel's timer went on ticking.
  useEffect(() => () => stopRef.current?.abort(), []);

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

  // Handshake first: never save a gateway we cannot reach. We only persist a
  // gateway that actually answered — an unreachable URL/QR is rejected with a
  // clear reason, and every address it advertised says which one it was.
  //
  // A pairing payload carries `alts` (the gateway's other reachable hosts:
  // Tailscale, LAN, tunnel). All of them are kept on the connection so a later
  // failover/upgrade has somewhere to go; `bestReachable` decides which one this
  // device connects on, and reports the whole race while it runs.
  async function tryConn(conn: GatewayConn) {
    const { alts, ...base } = conn;
    const candidates = mergeAddresses([base.url], alts);
    const ctrl = new AbortController();
    stopRef.current?.abort();
    stopRef.current = ctrl;
    setBusy(true);
    setMsg(null);
    setRun({
      label: hostOf(conn.url),
      startedAt: Date.now(),
      candidates: candidates.map((candidate) => ({ url: candidate, state: 'probing' as const })),
    });
    const report = (candidate: string, verdict: Omit<CandidateProbe, 'url'>) =>
      setRun((current) =>
        current
          ? {
              ...current,
              candidates: current.candidates.map((probe) =>
                probe.url === candidate ? { ...probe, ...verdict } : probe,
              ),
            }
          : current,
      );

    try {
      const { chosen, unauthorized } = await bestReachable(base, candidates, report, ctrl.signal);
      // Stop means STOP, and the banner promises nothing was saved. Aborting the
      // run settles whatever was still in flight, so an address that had already
      // answered would otherwise be chosen and paired by the act of giving up.
      if (ctrl.signal.aborted) {
        setMsg({ kind: 'warn', text: `Stopped pairing with ${hostOf(conn.url)}. Nothing was saved.` });
        return;
      }
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
      if (stopRef.current === ctrl) stopRef.current = null;
      setRun(null);
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

  const secondsLeft = run
    ? Math.max(0, Math.ceil((run.startedAt + PROBE_TIMEOUT_MS - Math.max(now, run.startedAt)) / 1000))
    : 0;

  return (
    <div className="min-w-0">
      {scanning && (
        <Suspense fallback={<ScannerOpening onCancel={() => setScanning(false)} />}>
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
                {busy ? 'Pairing\u2026' : 'Pair'}
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
              {busy ? 'Connecting\u2026' : 'Connect'}
            </Button>
          </div>
        </div>
      </div>

      {run && (
        <PairingProgress
          run={run}
          secondsLeft={secondsLeft}
          onStop={() => stopRef.current?.abort()}
        />
      )}

      {msg && (
        <div className="mt-3">
          <Banner kind={msg.kind}>{msg.text}</Banner>
        </div>
      )}
    </div>
  );
}

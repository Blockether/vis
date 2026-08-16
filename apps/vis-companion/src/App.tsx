import {
  type ComponentProps,
  lazy,
  type ReactNode,
  Suspense,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import type { GatewayConn } from "./lib/types";
import { type Compat, compatFromHealth } from "./lib/compat";
import { GatewayClient, ROUTER_TTL_MS } from "./lib/gateway";
import {
  getPrimaryConnection,
  loadConnections,
  loadConnectionsSync,
  loadSubscribedSessions,
  rememberSubscribedSession,
  setActiveUrl,
  setPrimaryUrl,
  switchConnectionUrl,
  upsertConnection,
  removeConnection,
} from "./lib/storage";
import {
  bestAddress,
  hostOf,
  isUpgrade,
  mergeAddresses,
} from "./lib/endpoints";
import { onWake } from "./lib/wake";
import { warm } from "./lib/warm";
import { SessionSubscriptionHub } from "./lib/subscriptions";
import { parsePairing } from "./lib/pairing";
import { onPairingLink } from "./lib/deeplink";
import {
  hydratePendingShare,
  parseShareLink,
  receiveSharedText,
} from "./lib/share-intake";
import { applyTheme, resolveTheme } from "./lib/theme";
import { getThemePref } from "./lib/storage";
import { BackButton, IconButton, SearchField } from "./components/ui";
import { SearchIcon, SettingsIcon } from "./components/icons";
import { ConnectScreen } from "./screens/ConnectScreen";
import { SessionsScreen } from "./screens/SessionsScreen";
import { IncompatibleScreen } from "./screens/IncompatibleScreen";
import {
  isSessionEntered,
  parseRoute,
  parseSessionDeepLink,
  screenKey,
  sessionHash,
  tabHash,
} from "./lib/router";
import {
  reclaimViewportForExternalNavigation,
  useVisualViewportShell,
} from "./lib/viewport";
import { App as CapacitorApp } from "@capacitor/app";
import {
  acquirePushToken,
  cachedPushToken,
  deviceRegistration,
  ensureAndroidChannel,
  isPushSupported,
  onPushTap,
  pushPermission,
} from "./lib/push";
import { drainPushRevocations, syncPushRegistrations } from "./lib/notify";
import {
  drainWebPushRevocations,
  isWebNotificationsPlatform,
  registerWebServiceWorker,
  syncWebPushRegistrations,
} from "./lib/web-push";
import { registerForPush, unregisterFromPush } from "./lib/relay";
import { isShellChromeVisible, shellScreen } from "./lib/shell";
import {
  pushIntentFrom,
  resolvePushIntent,
  type PushIntent,
} from "./lib/push-intent";

type Tab = "sessions" | "connect";

/** How long a parked share still steers navigation on the next launch. */
const RESUMABLE_SHARE_MS = 5 * 60 * 1000;

// The splash is allowed to be a moment, never a state. Reading the stored
// gateways is a native bridge call, and an iOS bridge can go silent after the
// OS recycled the webview in the background (`lib/bridge.ts`), so the reveal is
// also on a timer: painting a connection-less shell beats painting nothing
// until the user force-quits.
const BOOT_REVEAL_MS = 3_000;

// Losing the gateway must cost a bounded amount of work. Every recovery sweep
// re-reads capabilities and pings each known address, and each failed ping
// reports "unreachable" again: unthrottled, that feeds itself. Measured on the
// simulator against a dead gateway it produced ~300 fetches per second, which
// pinned the WebKit process and left the session list stuck on its skeleton
// forever. Sweep on the leading edge, then at most once per window.
const RECOVERY_SWEEP_MIN_GAP_MS = 5_000;

// The launch screen is the session LIST, but the transcript renderer behind it —
// `SessionScreen` with `ChatContent`, react-markdown/remark and Prism plus a
// dozen grammars — sat in the SAME chunk, so every cold start parsed and ran all
// 857 kB before painting a row. Split both heavy screens out and warm them the
// moment the list is up: by the time anything is tapped the module is resolved,
// `lazy` mounts it in the same commit, and no boundary ever falls back.
const SessionScreenLazy = lazy(async () => ({
  default: (await import("./screens/SessionScreen")).SessionScreen,
}));
const SettingsDialogLazy = lazy(async () => ({
  default: (await import("./screens/SettingsScreen")).SettingsDialog,
}));

// Each split screen keeps its own boundary so a miss costs that screen, never the
// app shell. The transcript's fallback is the same ink sheet its own veil paints,
// so a cold open reads as the load it already was; a dialog just arrives a frame
// later.
function SessionScreen(props: ComponentProps<typeof SessionScreenLazy>) {
  return (
    <Suspense fallback={<div className="h-full bg-ink" />}>
      <SessionScreenLazy {...props} />
    </Suspense>
  );
}

function SettingsDialog(props: ComponentProps<typeof SettingsDialogLazy>) {
  return (
    <Suspense fallback={null}>
      <SettingsDialogLazy {...props} />
    </Suspense>
  );
}

function prefetchScreens() {
  warm(import("./screens/SessionScreen"));
  warm(import("./screens/SettingsScreen"));
}

export function App() {
  // Warm the split screens once the shell is up — off the critical path, so the
  // launch frame stays the list and the first tap still opens instantly.
  useEffect(() => {
    if (typeof window.requestIdleCallback === "function") {
      const handle = window.requestIdleCallback(prefetchScreens, {
        timeout: 2_000,
      });
      return () => window.cancelIdleCallback(handle);
    }
    const timer = window.setTimeout(prefetchScreens, 300);
    return () => window.clearTimeout(timer);
  }, []);

  // localStorage mirrors the native connection store, so seed the first frame without
  // waiting for Capacitor Preferences to resolve asynchronously.
  const [conns, setConns] = useState<GatewayConn[]>(loadConnectionsSync);
  const [active, setActive] = useState<GatewayConn | null>(null);
  const [primary, setPrimary] = useState<GatewayConn | null>(null);
  const [tab, setTab] = useState<Tab>("sessions");
  // The search question is fleet-wide, so the SHELL owns it: the bar asks it and the
  // list answers it. Kept here rather than in `SessionsScreen` so the field can sit
  // above every machine chip instead of under the one that names a machine.
  const [query, setQuery] = useState("");
  // The search PAGE: the bar becomes a way back plus the field, and the list under it
  // is the answer. It is shell state rather than the header's own because leaving it
  // clears the query the list is reading.
  const [searching, setSearching] = useState(false);
  const [openTarget, setOpenTarget] = useState<{
    conn: GatewayConn;
    sid: string;
    fresh?: boolean;
  } | null>(null);
  const [settingsTarget, setSettingsTarget] = useState<GatewayConn | null>(
    null,
  );
  const [settingsOpen, setSettingsOpen] = useState(false);
  // ONE settings dialog, opened from two places at two aims: the cog opens it on the
  // machine the app is already using, a machine's `⋯` opens it on that machine. Both
  // land in the same box, so neither is a different destination.
  const openSettings = useCallback((conn: GatewayConn | null) => {
    if (conn) setSettingsTarget(conn);
    setSettingsOpen(true);
  }, []);
  const [ready, setReady] = useState(false);
  // Set when the sessions screen finds the active gateway unreachable. While it
  // holds a message there is nothing to navigate, so the shell shows Machines
  // only — no Sessions tab, no session list shaped like an error page.
  const [offline, setOffline] = useState<string | null>(null);
  // A Wi-Fi → mobile handoff keeps `navigator.onLine` true, so it does not emit
  // the browser's `online` event. A failed request therefore explicitly kicks
  // address recovery instead of waiting for the next wake event.
  const [compat, setCompat] = useState<Compat | null>(null);
  const [compatChecking, setCompatChecking] = useState(false);
  const [compatNonce, setCompatNonce] = useState(0);

  const sessionConn = openTarget?.conn ?? active;
  // Transport identity is the URL/token pair — the only fields `GatewayClient`
  // reads — so renaming a connection's label never tears down the live stream.
  const connUrl = sessionConn?.url ?? "";
  const connToken = sessionConn?.token;
  const client = useMemo(
    () =>
      connUrl ? new GatewayClient({ url: connUrl, token: connToken }) : null,
    [connUrl, connToken],
  );
  const subscriptions = useMemo(
    () => (client ? new SessionSubscriptionHub(client) : null),
    [client],
  );

  const refresh = useCallback(async () => {
    const [connections, primaryConnection] = await Promise.all([
      loadConnections(),
      getPrimaryConnection(),
    ]);
    setConns(connections);
    setPrimary(primaryConnection);
    setActive(primaryConnection);
  }, []);

  const [recoveryNonce, setRecoveryNonce] = useState(0);
  const lastRecoverySweepAt = useRef(0);
  const handleUnreachable = useCallback((message: string | null) => {
    setOffline(message);
    // Deliberately NOT reset when the app reports "reachable" again: screens
    // report both edges, so resetting here let a failing screen alternate
    // null/message and sweep on every single failed request.
    if (!message) return;
    const now = Date.now();
    if (now - lastRecoverySweepAt.current < RECOVERY_SWEEP_MIN_GAP_MS) return;
    lastRecoverySweepAt.current = now;
    setRecoveryNonce((nonce) => nonce + 1);
  }, []);

  const addConnection = useCallback(
    async (conn: GatewayConn, makeActive = true) => {
      const next = await upsertConnection(conn);
      if (next.length === 1) {
        await Promise.all([setPrimaryUrl(conn.url), setActiveUrl(conn.url)]);
        setPrimary(conn);
      }
      setConns(next);
      setOffline(null);
      setActive(makeActive ? conn : await getPrimaryConnection());
      if (makeActive) setTab("sessions");
    },
    [],
  );

  // The tap must paint the session on the frame it happens on. Remembering the
  // active gateway and the watch list are Capacitor Preferences writes — bridge
  // round trips — and awaiting them BEFORE the state flip is exactly the lag
  // between tapping a row and seeing the transcript. Nothing on this path reads
  // them back, so they are persisted underneath the navigation, not in front of
  // it.
  const openGatewaySession = useCallback(
    (conn: GatewayConn, sid: string, fresh = false) => {
      setActive(conn);
      setOpenTarget({ conn, sid, fresh });
      void rememberSubscribedSession(conn.url, sid).catch(() => undefined);
    },
    [],
  );

  // Overlays are screen-scoped, and a navigation can land while one is up: see
  // `isSessionEntered`. Dismissing here covers every way in — list tap, deep
  // link, notification tap, share intake, and the create that finishes after
  // the user has already opened Settings.
  const screen = screenKey(openTarget);
  const shownScreen = useRef(screen);
  useEffect(() => {
    const previous = shownScreen.current;
    shownScreen.current = screen;
    if (!isSessionEntered(previous, screen)) return;
    setSettingsTarget(null);
    setSettingsOpen(false);
  }, [screen]);

  // A share sheet drop, an Android `ACTION_SEND`, or a Shortcuts run carries a
  // payload but no destination. `lib/share-intake` parks it; this decides where
  // it lands: the most recently opened session of the active gateway, so
  // "Share → vis" is one tap from a prompt. With nothing to reopen, the list is
  // the honest destination — the payload stays parked until a composer takes it,
  // so landing short never loses it.
  const openSharedSession = useCallback(async () => {
    const conn = active ?? (await getPrimaryConnection());
    if (!conn) {
      setTab("connect");
      return;
    }
    const [sid] = await loadSubscribedSessions(conn.url);
    if (!sid) {
      setActive(conn);
      setTab("sessions");
      return;
    }
    openGatewaySession(conn, sid);
  }, [active, openGatewaySession]);

  // Bumped by every accepted share. A counter, not a flag: two links dropped
  // back to back must both re-route, including the one that arrives while the
  // first is still opening.
  const [shareNonce, setShareNonce] = useState(0);
  const handledShareNonce = useRef(0);

  // A share that outlived the webview it was dropped into (cold start, OS
  // reclaim, crash). Only a RECENT one steers navigation: opening the app days
  // later must not yank the user into a session, and the payload keeps waiting
  // for whichever composer mounts next either way.
  useEffect(() => {
    void hydratePendingShare().then((share) => {
      if (share && Date.now() - (share.at ?? 0) < RESUMABLE_SHARE_MS) {
        setShareNonce((nonce) => nonce + 1);
      }
    });
  }, []);

  // Hash routing: a session is a shareable URL (#/s/<sid>?gw=<gateway-url>).
  // Apply the current hash to view state, and follow browser back/forward and
  // pasted links via `hashchange`. Opening still needs the gateway paired
  // locally (its bearer token is never in the link).
  const applyRoute = useCallback(
    (hash: string) => {
      const route = parseRoute(hash);
      if (route.name === "session") {
        const conn =
          (route.gw && conns.find((c) => c.id === route.gw)) || active;
        if (conn) {
          void openGatewaySession(conn, route.sid);
          return;
        }
        setOpenTarget(null);
        setTab("connect");
        return;
      }
      setOpenTarget(null);
      setTab(route.name === "connect" ? "connect" : "sessions");
    },
    [conns, active, openGatewaySession],
  );

  // Apply the initial hash once connections are loaded, then track hashchange.
  const [routeApplied, setRouteApplied] = useState(false);
  // True only while a history entry WE pushed (the list → session step) is on
  // top of the stack. Nothing else may be popped: a session reached by deep link
  // or cold start has no entry of ours beneath it, and going back from there
  // would leave the app.
  const pushedSessionRef = useRef(false);
  useEffect(() => {
    if (!ready) return;
    if (!routeApplied) {
      // Adopting the address bar IS synchronising from an external system, and it
      // happens exactly once per load — not a cascading render. But a cold
      // relaunch after iOS killed the WebContent process (Capacitor #7810/#7905)
      // reboots from capacitor://localhost's blank hash, not the previous
      // address bar — so a route-less cold start here does NOT mean "go to the
      // list", it means "we lost the address bar". Resume the last subscribed
      // session for the active gateway instead, so an abandoned in-flight turn
      // never gets silently orphaned by a background/foreground cycle.
      const hash = window.location.hash;
      if (!hash && active) {
        void loadSubscribedSessions(active.url).then(([sid]) => {
          if (sid) {
            openGatewaySession(active, sid);
          } else {
            applyRoute(hash);
          }
        });
      } else {
        applyRoute(hash);
      }
      setRouteApplied(true);
    }
    const onHash = () => {
      // Any arrival at a non-session URL (our own pop, browser back, a pasted
      // link) means our pushed entry is gone from the top.
      if (parseRoute(window.location.hash).name !== "session")
        pushedSessionRef.current = false;
      applyRoute(window.location.hash);
    };
    window.addEventListener("hashchange", onHash);
    return () => window.removeEventListener("hashchange", onHash);
  }, [ready, routeApplied, applyRoute, active, openGatewaySession]);

  // Reflect view state back into the URL so the address bar is always shareable.
  useEffect(() => {
    if (!ready || !routeApplied) return;
    // Prefer the freshest captured gateway id (backfilled after open) over the
    // one snapshotted into openTarget, so the shareable URL cleans up in place.
    const gwId =
      conns.find((c) => c.url === openTarget?.conn.url)?.id ??
      openTarget?.conn.id;
    const desired = openTarget
      ? sessionHash(openTarget.sid, gwId)
      : tabHash(tab === "connect" ? "connect" : "sessions");
    const current = window.location.hash || "#/";
    if (current === desired) return;
    // Opening a session is this app's ONE forward navigation, so it is the one
    // that earns a history entry. Rewriting every route in place left the stack
    // empty, which is why Android's back gesture quit the app from inside a
    // session instead of returning to the list. Everything else — tab switches,
    // the gateway id backfilled into an already-open session — still rewrites
    // the current entry, so back never walks through cosmetic URL repairs.
    if (openTarget && parseRoute(current).name !== "session") {
      history.pushState(null, "", desired);
      pushedSessionRef.current = true;
      return;
    }
    history.replaceState(null, "", desired);
  }, [openTarget, tab, ready, routeApplied, conns]);

  // Leaving a session is a history STEP, not a state reset: entering pushed an
  // entry, so popping it is what keeps the address bar, the back gesture and the
  // in-app arrow telling the same story. With no entry of ours on top (deep
  // link, cold start) the state flip is the whole navigation.
  const leaveSession = useCallback(() => {
    if (pushedSessionRef.current) {
      pushedSessionRef.current = false;
      history.back();
      return;
    }
    setOpenTarget(null);
  }, []);

  // Android's hardware/gesture back. Registering a listener REPLACES Capacitor's
  // default, which is `history.back()` — the thing that did nothing here. One
  // handler for the whole shell: dismiss what is on top, then walk the shell
  // back, and only leave the app from the root. Kept in a ref so the native
  // listener is registered once instead of re-crossing the bridge per render.
  const backRef = useRef<() => void>(() => {});
  useEffect(() => {
    backRef.current = () => {
      if (settingsTarget) {
        setSettingsTarget(null);
        return;
      }
      if (openTarget) {
        leaveSession();
        return;
      }
      if (tab !== "sessions" && conns.length > 0 && active) {
        setTab("sessions");
        return;
      }
      try {
        void CapacitorApp.exitApp().catch(() => undefined);
      } catch {
        /* web build: nothing to exit */
      }
    };
  });
  useEffect(() => {
    let removed = false;
    let sub: { remove: () => void } | null = null;
    try {
      void CapacitorApp.addListener("backButton", () => backRef.current())
        .then((handle) => {
          if (removed) handle.remove();
          else sub = handle;
        })
        .catch(() => undefined);
    } catch {
      /* plugin unavailable */
    }
    return () => {
      removed = true;
      sub?.remove();
    };
  }, []);

  useEffect(() => {
    // Mount-time gateway load: the flag flips when the read settles, or when the
    // watchdog fires.
    let revealed = false;
    const reveal = () => {
      if (revealed) return;
      revealed = true;
      setReady(true);
    };
    const timer = window.setTimeout(reveal, BOOT_REVEAL_MS);
    void refresh().finally(reveal);
    return () => window.clearTimeout(timer);
  }, [refresh]);

  // The palette is STATIC CSS shipped with the app, so the first frame paints from
  // one stored id — no gateway is asked, and none can repaint this device.
  useEffect(() => {
    void getThemePref().then((pref) => {
      applyTheme(resolveTheme(pref));
    });
  }, []);

  // Deep-linked pairing: vis://gateway?url=…&token=…
  // Deep-linked session: vis://s/<sid>?gw=<id> — the shareable form on native,
  // where the WebView origin (capacitor://localhost) is not an openable URL.
  // Shared drop: vis://share?url=…&text=…&title=… — the single shape the iOS
  // share extension, the Android SEND filter and the Shortcuts action all
  // rewrite whatever they were handed into.
  useEffect(() => {
    let dispose = () => {};
    void onPairingLink((url) => {
      const parsed = parsePairing(url);
      if (parsed) {
        // Keep `alts`: they are the other addresses this gateway answers on,
        // and the app needs them to move off a LAN-only address later.
        void addConnection(parsed);
        return;
      }
      const shared = parseShareLink(url);
      if (shared) {
        // Park it BEFORE navigating: a link the user handed us must not depend
        // on this app managing to reach a composer in this launch.
        if (receiveSharedText(shared)) setShareNonce((nonce) => nonce + 1);
        return;
      }
      const hash = parseSessionDeepLink(url);
      // Route through the hash so a cold start and a warm resume take the same
      // path as a pasted web link (and browser back still works).
      if (hash) window.location.hash = hash;
    }).then((d) => (dispose = d));
    return () => dispose();
  }, [addConnection]);

  // Steer to a composer once a share is parked AND the initial route has been
  // applied: routing earlier would be overwritten by the hash we booted with.
  useEffect(() => {
    if (
      !shareNonce ||
      !routeApplied ||
      shareNonce === handledShareNonce.current
    )
      return;
    handledShareNonce.current = shareNonce;
    void openSharedSession();
  }, [shareNonce, routeApplied, openSharedSession]);

  // Precache the provider/model fleet. `/v1/router` costs the daemon a live
  // auth + limits probe per provider, so warming it at connect time (and once
  // per TTL after) is what makes the model picker open instantly instead of
  // spinning for seconds on first use.
  useEffect(() => {
    if (!client) return;
    client.prefetchRouter();
    const timer = window.setInterval(
      () => client.prefetchRouter(),
      ROUTER_TTL_MS,
    );
    return () => window.clearInterval(timer);
  }, [client]);

  // Version handshake, once per gateway. `/healthz` stays open even to a client
  // the gateway refuses to serve, so a protocol mismatch can explain itself
  // instead of surfacing as a stream of payloads neither side can read. An
  // unreachable gateway yields no verdict — that is a connection problem, and
  // the screens below already report it.
  useEffect(() => {
    if (!client) {
      setCompat(null);
      return;
    }
    let cancelled = false;
    const ctrl = new AbortController();
    setCompatChecking(true);
    void client
      .health(ctrl.signal)
      .then((h) => {
        if (!cancelled) setCompat(compatFromHealth(h));
      })
      .catch(() => {
        if (!cancelled) setCompat(null);
      })
      .finally(() => {
        if (!cancelled) setCompatChecking(false);
      });
    return () => {
      cancelled = true;
      ctrl.abort();
    };
  }, [client, compatNonce]);

  // ── Address preference ──────────────────────────────────────────
  // One gateway answers on several addresses at once (Tailscale, LAN, tunnel,
  // loopback) and they are NOT equal: pairing happens standing next to the
  // machine, where the LAN address replies first — and that address dies the
  // moment the phone leaves the house. So for the ACTIVE gateway the app
  //   1. refreshes the known address list FROM the gateway (`/v1/capabilities`
  //      advertises them), so an app paired on the LAN can still learn the
  //      tailnet address without re-scanning a QR, and
  //   2. moves itself onto a more durable address as soon as that one answers.
  // A hand-picked address is never changed while it works, and loopback is never
  // left behind — see `lib/endpoints.ts`.
  const activeUrl = active?.url ?? "";
  const activeToken = active?.token;
  const activePinned = active?.pinned ?? false;
  const activeLabel = active?.label;
  const knownAltsKey = (active?.alts ?? []).join(" ");
  useEffect(() => {
    if (!activeUrl) return;
    let cancelled = false;
    let ctrl = new AbortController();

    const run = async () => {
      ctrl.abort();
      ctrl = new AbortController();
      const signal = ctrl.signal;
      const creds = { url: activeUrl, token: activeToken };
      let advertised: string[] = [];
      let activeResponded = false;
      try {
        advertised =
          (await new GatewayClient(creds).capabilities(signal)).addresses ?? [];
        activeResponded = true;
      } catch {
        // An address can disappear while the device remains online (for example,
        // during Wi-Fi → mobile handoff). Probe the stored alternatives below.
      }
      if (cancelled) return;
      // What the gateway advertises is authoritative when it answers at all:
      // an address it no longer serves — a stale DHCP lease, a tailnet IP from
      // a machine that was re-imaged — has to disappear instead of lingering
      // forever as an unreachable row nobody can delete.
      const known = advertised.length
        ? mergeAddresses([activeUrl], advertised)
        : mergeAddresses([activeUrl], knownAltsKey.split(" "));
      if (known.join(" ") !== knownAltsKey) {
        await upsertConnection({ url: activeUrl, alts: known });
        if (cancelled) return;
        setConns(await loadConnections());
      }
      // A pinned address stays preferred while it works, but it must not turn a
      // temporary network change into a permanent dead connection.
      if (activePinned && activeResponded) return;
      const candidates = activeResponded
        ? known.filter((url) => isUpgrade(url, activeUrl))
        : known;
      if (!candidates.length) return;
      const reachable = (
        await Promise.all(
          candidates.map(async (url) => {
            try {
              return (await new GatewayClient({ ...creds, url }).ping(signal))
                ? url
                : null;
            } catch {
              return null;
            }
          }),
        )
      ).filter((url): url is string => url !== null);
      const chosen = bestAddress(reachable);
      if (cancelled || !chosen) return;
      // The capabilities request failed but its follow-up ping won: the original
      // address recovered before failover completed, so simply restore the UI.
      if (chosen === activeUrl) {
        setOffline(null);
        return;
      }
      // A machine the user renamed keeps its name across an address switch; an
      // auto-derived host label follows the address it describes.
      const named = Boolean(activeLabel) && activeLabel !== hostOf(activeUrl);
      // THE PIN DOES NOT TRAVEL. It records the address a human picked BY NAME,
      // and this move is the app's own: carrying it would leave the row saying
      // `Pinned` about an address nobody chose, and would freeze the app there —
      // a pin outranks the durability order, so no better address is taken
      // afterwards either. A pin the app could not honour is released here.
      await switchConnectionUrl(activeUrl, chosen, {
        pinned: false,
        ...(named ? {} : { label: hostOf(chosen) }),
      });
      if (cancelled) return;
      setOffline(null);
      await refresh();
    };

    void run();
    // A resumed app is often on a different network than when it was suspended,
    // which is exactly when the durable address becomes reachable (or the LAN
    // one stops being).
    const off = onWake(() => void run());
    return () => {
      cancelled = true;
      ctrl.abort();
      off();
    };
  }, [
    activeUrl,
    activeToken,
    activePinned,
    activeLabel,
    knownAltsKey,
    recoveryNonce,
    refresh,
  ]);

  // Native push is a PER-GATEWAY choice, so the sweep is per gateway: every
  // paired machine is brought in line with ITS OWN switch (that gateway's
  // settings ▸ Notifications), not just the one the app happens to have open.
  // Re-asserting both directions is what keeps the choice true over time — the
  // OS rotates this device's token, and a "stop" made while a machine was
  // unreachable has to land eventually. Registration only refreshes a permission
  // the user ALREADY granted — the app never prompts on launch, and the web
  // build no-ops.
  const pairedKey = conns
    .map((c) => `${c.url}\u0000${c.token ?? ""}`)
    .join("\n");
  const notifyTargets = useMemo<GatewayConn[]>(
    () =>
      pairedKey
        .split("\n")
        .filter(Boolean)
        .map((row) => {
          const [url, token] = row.split("\u0000");
          return { url, token: token || undefined };
        }),
    [pairedKey],
  );
  useEffect(() => {
    if (!isPushSupported()) return;
    // The channel must exist before the first alert can land: Android posts
    // nothing without one, and Firebase's own fallback is the unnamed
    // "Miscellaneous" row at default importance.
    void ensureAndroidChannel();
    let cancelled = false;
    // Which name this device is filed under is the gateway's answer, not ours:
    // a machine with no signing key of its own is handed a relay grant instead
    // of a token (see lib/relay.ts).
    const revoke = (conn: GatewayConn, tok: string) =>
      unregisterFromPush(tok, new GatewayClient(conn).pushTarget());
    const sweep = async () => {
      if ((await pushPermission()) !== "granted") return;
      let token = cachedPushToken() ?? "";
      try {
        token = await acquirePushToken();
      } catch {
        // An OS that withheld the token is not a reason to degrade the session
        // UI — and a machine this device was taken off is still named by the
        // relay grant it was registered under, which is stored on this device.
      }
      if (cancelled) return;
      // What is OWED comes first: a machine that was forgotten is no longer in
      // the swept set, so this is the only thing left that can stop it pushing.
      await drainPushRevocations(token, revoke, () => cancelled);
      if (cancelled || !token || notifyTargets.length === 0) return;
      await syncPushRegistrations(
        notifyTargets,
        token,
        {
          register: (conn, tok) =>
            registerForPush(
              deviceRegistration(tok),
              new GatewayClient(conn).pushTarget(),
            ),
          unregister: revoke,
        },
        () => cancelled,
      );
    };
    void sweep();
    // A switch flipped while its machine was unreachable is stored but not yet
    // asserted, and waking is exactly when that machine tends to come back.
    const off = onWake(() => void sweep());
    return () => {
      cancelled = true;
      off();
    };
  }, [notifyTargets]);

  useEffect(() => {
    if (!isWebNotificationsPlatform()) return;
    void registerWebServiceWorker().catch(() => undefined);
  }, []);

  // Existing permission and subscriptions are enough to restore background delivery;
  // never prompt on launch. The service worker owns the notification, not this tab.
  useEffect(() => {
    if (!isWebNotificationsPlatform()) return;
    let cancelled = false;
    const sweep = async () => {
      // A browser that was taken off a gateway has to be taken off it THERE:
      // the subscription lives on the machine, and a forgotten machine is not
      // swept any more.
      await drainWebPushRevocations(() => cancelled);
      if (cancelled || notifyTargets.length === 0) return;
      await syncWebPushRegistrations(notifyTargets, () => cancelled);
    };
    void sweep();
    const off = onWake(() => void sweep());
    return () => {
      cancelled = true;
      off();
    };
  }, [notifyTargets]);

  // A notification tap is a HANDOFF across a cold start, so it is PARKED, not
  // handled where it lands: Capacitor retains the tap until the first listener
  // consumes it, and that listener attaches on the first render — before the
  // saved machines are read back and before the launch route is applied. The
  // handler used to read the active gateway right there, find none, and return,
  // consuming the tap: the notification opened the app on the session list and
  // the session it was about never appeared. See `lib/push-intent.ts`.
  const [pushIntent, setPushIntent] = useState<PushIntent | null>(null);
  useEffect(() => {
    // Mount-once on purpose: re-subscribing whenever the active gateway changes
    // leaves a window with no listener attached, which is where the retained
    // launch tap arrives.
    return onPushTap((tap) => {
      // The OS may suspend us with the software keyboard up and never deliver its
      // hide event. Reclaim the full shell before routing; otherwise the old
      // keyboard-height pin survives into the notification's destination.
      reclaimViewportForExternalNavigation();
      const intent = pushIntentFrom(tap, Date.now());
      if (intent) setPushIntent(intent);
    });
  }, []);

  // Drain the parked tap as soon as the launch route is applied AND a gateway
  // exists to open it on — both of which arrive after the tap during a cold
  // start. Re-evaluated on every state change that could unblock it.
  useEffect(() => {
    if (!pushIntent) return;
    const outcome = resolvePushIntent(pushIntent, {
      isRouteApplied: routeApplied,
      conns,
      active,
      now: Date.now(),
    });
    if (outcome.action === "wait") return;
    setPushIntent(null);
    if (outcome.action === "open")
      openGatewaySession(outcome.conn, outcome.sid);
  }, [pushIntent, routeApplied, conns, active, openGatewaySession]);

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
        const id = await new GatewayClient(conn)
          .identify(ctrl.signal)
          .catch(() => null);
        if (id) {
          await upsertConnection({ ...conn, id });
          changed = true;
        }
      }
      if (changed && !cancelled) {
        const next = await loadConnections();
        if (cancelled) return;
        setConns(next);
        setActive(await getPrimaryConnection());
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
    // Nothing to restore without a stream to carry the watch list.
    if (!subscriptions || !sessionConn) return;
    void loadSubscribedSessions(sessionConn.url).then((ids) => {
      if (cancelled) return;
      const next =
        openTarget?.sid && !ids.includes(openTarget.sid)
          ? [openTarget.sid, ...ids]
          : ids;
      subscriptions.watchSessions(next);
    });
    return () => {
      cancelled = true;
    };
  }, [openTarget?.sid, sessionConn, subscriptions]);

  useEffect(() => () => subscriptions?.dispose(), [subscriptions]);

  // The visual-viewport pin is owned by <Shell> below. Owning it there (instead of in
  // this component) means a keyboard or rotation frame re-renders only <Shell>'s root,
  // not this whole tree and not the multi-thousand-line session screen. Rotation is
  // coordinated imperatively in lib/viewport.ts without stamping a class on <html>.

  if (!ready) return <Splash />;

  // A session already open keeps its own screen; the offline gate is about the
  // list, not about the transcript you are reading.
  const blocked = !!offline && !openTarget;
  const hasConn = conns.length > 0 && !!active && !blocked;
  const isIncompatible = !!sessionConn && !!compat && !compat.isCompatible;

  // One decision drives both halves of the shell: which screen fills it, and
  // therefore whether the shell still owns the header and the tab bar. Deciding
  // those apart is what let a session opened from a notification — a cold start
  // parks the shell on `connect` before the saved machines load — render the
  // Machines screen stripped of its chrome, riding under the status bar.
  const shellView = shellScreen({
    isSessionOpen: !!openTarget,
    isSessionReady: !!client && !!subscriptions,
    isIncompatible,
    hasConn,
    tab,
  });
  const isChromeVisible = isShellChromeVisible(shellView);
  // Keep the fleet list alive while Machines is open, not only while a session is open.
  // Changing tabs should change visibility, never the list's component identity: its cached
  // rows, scope, scroll position, and expanded projects are already the user's frame.
  const sessionsMounted = conns.length > 0 && !!active;
  const sessionsVisible = shellView === "sessions";

  return (
    <Shell>
      {isChromeVisible && (
        <Header
          query={query}
          onQuery={setQuery}
          isSearching={searching}
          onSearch={() => setSearching(true)}
          onCloseSearch={() => {
            setSearching(false);
            setQuery("");
          }}
          onAppSettings={() => openSettings(active ?? primary ?? conns[0] ?? null)}
        />
      )}

      <main
        className={`min-h-0 flex-1 overflow-x-hidden overscroll-contain ${shellView === "session" ? "overflow-hidden" : "overflow-y-auto"}`}
      >
        {sessionsMounted && (
          <div className={sessionsVisible ? "h-full" : "hidden"}>
            <SessionsScreen
              conns={conns}
              isVisible={sessionsVisible}
              query={query}
              onQuery={setQuery}
              subscriptions={subscriptions}
              onUnreachable={handleUnreachable}
              onOpen={openGatewaySession}
              // The list's own way into the search page: a pull at the top of it
              // opens the same door the app bar's glass is, and the page it opens is
              // this screen with the query over it — so while it is already open,
              // the gesture has nothing left to open and stands down.
              onSearch={searching ? null : () => setSearching(true)}
            />
          </div>
        )}
        {shellView === "connect" && (
          <ConnectScreen
            conns={conns}
            active={active}
            primary={primary}
            onAdd={addConnection}
            onSettings={openSettings}
            offlineError={blocked ? offline : null}
            onRetry={() => setOffline(null)}
            onClose={hasConn ? () => setTab("sessions") : undefined}
          />
        )}
        {shellView === "incompatible" && sessionConn && compat && (
          <IncompatibleScreen
            compat={compat}
            conn={sessionConn}
            isChecking={compatChecking}
            onRetry={() => setCompatNonce((n) => n + 1)}
            onBack={() => {
              setOpenTarget(null);
              setTab("connect");
            }}
          />
        )}
        {shellView === "session" && openTarget && client && subscriptions && (
          <SessionScreen
            key={`${openTarget.conn.url}:${openTarget.sid}`}
            client={client}
            subscriptions={subscriptions}
            sid={openTarget.sid}
            fresh={openTarget.fresh}
            onBack={leaveSession}
            onOpenSession={(sid, fresh) =>
              void openGatewaySession(openTarget.conn, sid, fresh)
            }
            onManageProviders={() => openSettings(openTarget.conn)}
          />
        )}
      </main>

      {settingsOpen && (
        <SettingsDialog
          gateways={conns}
          gateway={settingsTarget}
          primaryUrl={primary?.url}
          onAddMachine={addConnection}
          onMakePrimary={async (conn) => {
            await Promise.all([setPrimaryUrl(conn.url), setActiveUrl(conn.url)]);
            setPrimary(conn);
            setActive(conn);
            setOpenTarget(null);
            setOffline(null);
            setTab("sessions");
          }}
          onRename={async (conn, label) => {
            const updated = { ...conn, label };
            await upsertConnection(updated);
            // The row the dialog opened on keeps its identity under the new name.
            if (settingsTarget?.url === conn.url) setSettingsTarget(updated);
            await refresh();
          }}
          onRemove={async (conn) => {
            await removeConnection(conn.url);
            if (settingsTarget?.url === conn.url) setSettingsTarget(null);
            await refresh();
          }}
          onSelectAddress={async (conn, url, pinned) => {
            // The verb acts on the ROW it came out of, never on another machine: the
            // address line belongs to its own machine, and only the two pointers that
            // actually named this gateway move with it.
            const wasActive = conn.url === active?.url;
            const wasOpened = settingsTarget?.url === conn.url;
            if (url !== conn.url) {
              const named = Boolean(conn.label) && conn.label !== hostOf(conn.url);
              await switchConnectionUrl(
                conn.url,
                url,
                named ? {} : { label: hostOf(url) },
              );
            }
            const saved = await upsertConnection({ url, pinned });
            const next = saved.find((c) => c.url === url) ?? { ...conn, url, pinned };
            if (wasOpened) setSettingsTarget(next);
            if (wasActive) setActive(next);
            await refresh();
          }}
          onClose={() => setSettingsOpen(false)}
        />
      )}
    </Shell>
  );
}

export function Header({
  query,
  onQuery,
  isSearching,
  onSearch,
  onCloseSearch,
  onAppSettings,
}: {
  query: string;
  onQuery: (next: string) => void;
  /** Whether the search PAGE is the screen right now. */
  isSearching: boolean;
  onSearch: () => void;
  onCloseSearch: () => void;
  onAppSettings: () => void;
}) {
  // `/` opens the search from anywhere on the shell — unannounced on purpose, and
  // never stolen from someone already typing. Escape closes it again, because a page
  // that took the whole bar has to be leavable without aiming at a control.
  const searchRef = useRef<HTMLInputElement>(null);
  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        if (!isSearching) return;
        event.preventDefault();
        onCloseSearch();
        return;
      }
      if (event.key !== '/' || event.metaKey || event.ctrlKey || event.altKey) return;
      const at = document.activeElement as HTMLElement | null;
      if (
        at &&
        (at.isContentEditable ||
          at.tagName === 'INPUT' ||
          at.tagName === 'TEXTAREA' ||
          at.tagName === 'SELECT')
      ) {
        return;
      }
      event.preventDefault();
      onSearch();
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [isSearching, onCloseSearch, onSearch]);
  // The caret belongs to the page that just opened: a search screen a human still has
  // to tap into is a screen that asked for the tap twice.
  useEffect(() => {
    if (isSearching) searchRef.current?.focus();
  }, [isSearching]);
  return (
    <header className="relative z-30 shrink-0 border-b border-dialog-edge bg-panel-2 pt-[env(safe-area-inset-top)]">
      {/* SEARCH IS A PAGE, AND THE BAR IS ITS DOOR.

          The open field used to hold the bar's whole middle at every width — the
          widest box on a 390px phone, permanently, for a question that is asked in
          bursts. It is a MARK now: one magnifying glass beside the cog, and pressing
          it turns the whole screen into the search — the bar becomes the way back
          plus the field, and everything under it is the answer. That is why nothing
          else rides the bar while it is open: a fleet-wide query is the screen, not a
          filter parked in a corner of it.

          Leaving the page clears the query, so the list a human comes back to is the
          one they left rather than a silently filtered copy of it. */}
      {isSearching ? (
        <div className="mx-auto flex h-12 w-full max-w-[1400px] items-stretch pr-[max(0.75rem,env(safe-area-inset-right))] sm:pr-[max(1.5rem,env(safe-area-inset-right))]">
          <BackButton label="Close search" onClick={onCloseSearch} />
          <SearchField
            ref={searchRef}
            value={query}
            onValue={onQuery}
            placeholder="Search all machines…"
            label="Search sessions on every machine"
            className="ml-3 min-w-0 flex-1"
          />
        </div>
      ) : (
        <div className="mx-auto flex w-full max-w-[1400px] items-center pl-[max(0.75rem,env(safe-area-inset-left))] pr-[max(0.75rem,env(safe-area-inset-right))] sm:pl-[max(1.5rem,env(safe-area-inset-left))] sm:pr-[max(1.5rem,env(safe-area-inset-right))]">
          <div className="flex h-12 items-center gap-2.5" aria-label="Vis">
            <img
              src="/vis-logo.png"
              alt=""
              className="h-[18px] w-5 object-contain"
            />
            <span className="font-mono text-title font-black tracking-[0.18em] text-white">
              VIS
            </span>
          </div>
          {/* TWO MARKS, ONE MEANING EACH.
              The bar carries the app's own two verbs and nothing else: the glass that
              opens the search page, and the one cog on the whole app, which means
              PREFERENCES — this device's own settings, never a gateway's. A machine's
              settings hang off that machine, in the list's own `⋯`, so two gears can
              never sit 40px apart meaning different things.
              They are named where a name is read: `aria-label` for the screen reader,
              `title` for the pointer. An icon-only control without one is a bug. */}
          <div className="ml-auto flex h-12 items-center gap-2">
            <IconButton
              type="button"
              label="Search all machines"
              title="Search all machines"
              onClick={onSearch}
            >
              <SearchIcon className="size-4" />
            </IconButton>
            <IconButton
              type="button"
              label="Open preferences"
              title="Preferences"
              onClick={onAppSettings}
            >
              <SettingsIcon className="size-4" />
            </IconButton>
          </div>
        </div>
      )}
    </header>
  );
}

function Splash() {
  return (
    <div
      className="flex h-full items-center justify-center bg-ink"
      aria-label="Loading Vis"
    >
      <img
        src="/vis-logo.png"
        alt="Vis"
        className="h-16 w-auto animate-pulse object-contain"
      />
    </div>
  );
}

export function Shell({ children }: { children: ReactNode }) {
  // Keep the shell in the page's layout layer: absolute positioning avoids the
  // lagging fixed WebKit layer during rotation. The viewport hook mutates only
  // this root's geometry synchronously, without a React render between focus
  // and the composer moving above the keyboard.
  const shellRef = useRef<HTMLDivElement>(null);
  useVisualViewportShell(shellRef);
  return (
    <div
      ref={shellRef}
      data-viewport-shell
      className="isolate absolute inset-0 flex h-full min-h-0 flex-col overflow-hidden bg-ink text-body"
    >
      {children}
    </div>
  );
}

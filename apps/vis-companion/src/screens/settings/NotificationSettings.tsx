import { useCallback, useEffect, useMemo, useState } from "react";

import { GatewayClient, GatewayError } from "../../lib/gateway";
import type { GatewayConn, PushDevice, PushStatus } from "../../lib/types";
import {
  acquirePushToken,
  cachedPushToken,
  canOpenSystemNotificationSettings,
  deviceRegistration,
  isPushSupported,
  maskToken,
  openSystemNotificationSettings,
  pushPermission,
  pushPlatform,
  type PushPermission,
} from "../../lib/push";
import {
  ensureWebPushSubscription,
  getExistingWebPushSubscription,
  isWebNotificationsPlatform,
  isWebPushSupported,
  registerWebPushForGateway,
  requestWebPushPermission,
  unregisterWebPushForGateway,
  webPushApplicationServerKey,
  webPushPermission,
} from "../../lib/web-push";
import { applyGatewayNotify, applyWebGatewayNotify } from "../../lib/notify";
import {
  cachedNotifyVerdict,
  isHeldBy,
  notifyVerdict,
  rememberNotifyVerdict,
} from "../../lib/notify-verdict";
import {
  registerForPush,
  registeredIds,
  refusedRelayUrl,
  relayUrlFor,
  unregisterFromPush,
} from "../../lib/relay";
import { getGatewayNotify } from "../../lib/storage";
import { Banner, Button, NotifyConnectionSwitch } from "../../components/ui";
import { SettingsPanel } from "./SettingsLayout";

/**
 * Native push ON THIS GATEWAY: whether it can push at all, and whether THIS
 * device is registered.
 *
 * The token itself never round-trips through the UI — the gateway masks every
 * token it stores, and the app matches its own row by computing the same mask.
 */
export function NotificationsPanel({
  client,
  gateway,
}: {
  client: GatewayClient;
  gateway: GatewayConn;
}) {
  if (isWebNotificationsPlatform())
    return <WebNotificationsPanel gateway={gateway} />;
  return <NativeNotificationsPanel client={client} gateway={gateway} />;
}

function WebNotificationsPanel({ gateway }: { gateway: GatewayConn }) {
  const [perm, setPerm] = useState<PushPermission>(webPushPermission());
  const [subscription, setSubscription] = useState<PushSubscription | null>(
    null,
  );
  const [notify, setNotify] = useState(false);
  // Nothing may be reported until the browser has answered: "Not connected"
  // rendered before the first read is a verdict about a question not yet asked.
  const [loaded, setLoaded] = useState(false);
  const [busy, setBusy] = useState<"enable" | "disable" | null>(null);
  const [err, setErr] = useState<string | null>(null);
  const supported = isWebPushSupported();

  useEffect(() => {
    let cancelled = false;
    void Promise.all([
      getGatewayNotify(gateway.url),
      getExistingWebPushSubscription(gateway.url),
    ]).then(([wanted, current]) => {
      if (cancelled) return;
      setNotify(wanted);
      setSubscription(current);
      setPerm(webPushPermission());
      setLoaded(true);
    });
    return () => {
      cancelled = true;
    };
  }, [gateway.url]);

  const enable = useCallback(async () => {
    setBusy("enable");
    setErr(null);
    try {
      if (!supported)
        throw new Error("This browser does not support background Web Push.");
      const permission = await requestWebPushPermission();
      setPerm(permission);
      if (permission !== "granted")
        throw new Error(
          "Notifications are blocked in this browser. Allow them in browser settings first.",
        );
      const target = new GatewayClient(gateway).pushTarget();
      const status = await target.status();
      const next = await ensureWebPushSubscription(
        gateway.url,
        webPushApplicationServerKey(status),
      );
      await registerWebPushForGateway(gateway, next);
      await applyWebGatewayNotify(gateway.url, true);
      setSubscription(next);
      setNotify(true);
    } catch (cause) {
      setErr(cause instanceof Error ? cause.message : String(cause));
    } finally {
      setBusy(null);
    }
  }, [gateway, supported]);

  const disable = useCallback(async () => {
    setBusy("disable");
    setErr(null);
    try {
      const current =
        subscription ?? (await getExistingWebPushSubscription(gateway.url));
      if (current) await unregisterWebPushForGateway(gateway, current);
      await applyWebGatewayNotify(gateway.url, false);
      setNotify(false);
    } catch (cause) {
      setErr(cause instanceof Error ? cause.message : String(cause));
    } finally {
      setBusy(null);
    }
  }, [gateway, subscription]);

  const notifying =
    supported && notify && perm === "granted" && subscription !== null;
  const machine = gateway.label ?? gatewayHost(gateway.url);
  const blocked = supported && perm === "denied";
  // Same rule as the native panel: the verdict this browser settled on last time
  // is the honest first frame, so reopening Settings does not flash `Checking…`.
  const live = loaded ? notifying : null;
  const remembered = useMemo(
    () => cachedNotifyVerdict(gateway.url),
    [gateway.url],
  );
  useEffect(() => {
    if (live !== null) rememberNotifyVerdict(gateway.url, live);
  }, [live, gateway.url]);
  const shown = live ?? remembered;
  const hasBanner = Boolean(err) || !supported || blocked;

  // NO ADDRESS IN THE BAND. It printed the machine beside its own title — the same one
  // the row three lines above is named after — and that was reported (paraphrased: drop
  // that address, it is not useful; notifications are just on or off). The panel's one
  // fact is whether alerts arrive on this device, and the switch is that fact.

  return (
    <SettingsPanel
      title="Notifications"
      action={
        <NotifyConnectionSwitch
          machine={machine}
          isOn={shown ?? false}
          isBusy={busy !== null}
          isChecking={shown === null}
          disabled={!supported || blocked || shown === null || busy !== null}
          // The mark on the control is what the press must do, so a band painted
          // from the remembered verdict acts on THAT, not on a load still in flight.
          onClick={() => void (shown ? disable() : enable())}
        />
      }
    >
      {hasBanner && (
        <div className="space-y-2 p-3">
          {err && <Banner kind="err">{err}</Banner>}

          {!supported && (
            <Banner kind="warn">
              This browser does not support background Web Push.
            </Banner>
          )}

          {blocked && (
            <Banner kind="warn">
              Notifications are blocked in this browser — allow them in browser
              settings and this device can connect again.
            </Banner>
          )}
        </div>
      )}
    </SettingsPanel>
  );
}

export function NativeNotificationsPanel({
  client,
  gateway,
}: {
  client: GatewayClient;
  gateway: GatewayConn;
}) {
  // Reopening Settings must not re-ask a question this device already has the
  // answer to: the last device list this machine gave is painted first and the
  // fetch below revalidates it underneath.
  const seed = useMemo(() => client.cachedDevices(), [client]);
  const [push, setPush] = useState<PushStatus | null>(seed?.push ?? null);
  const [devices, setDevices] = useState<PushDevice[] | null>(
    seed?.devices ?? null,
  );
  const [perm, setPerm] = useState<PushPermission>("unsupported");
  const [err, setErr] = useState<string | null>(null);
  const [busy, setBusy] = useState<"enable" | "disable" | null>(null);
  // An OLDER gateway simply has no /v1/devices route. That is not an error the
  // user can act on — it is a missing capability upstream — so the whole panel
  // (and every button in it) disappears instead of offering calls that 404.
  // The refusal is remembered per machine, because a panel that paints itself
  // and then deletes itself takes everything below it up the screen with it.
  const [unsupported, setUnsupported] = useState(() =>
    client.isDevicesUnsupported(),
  );
  // This device's own answer, remembered per gateway: a machine you disconnected
  // from stays silent across relaunches, and a machine you connected to stays
  // registered even while another gateway is the one you have open.
  // Nothing is claimed before that machine's own answer is read back; a machine
  // this device never connected to answers no.
  const [notify, setNotify] = useState(false);

  const load = useCallback(
    async (signal?: AbortSignal) => {
      try {
        const [state, permission, wanted] = await Promise.all([
          client.devices(signal),
          pushPermission(),
          getGatewayNotify(gateway.url),
        ]);
        if (signal?.aborted) return;
        setPush(state.push);
        setDevices(state.devices);
        setPerm(permission);
        setNotify(wanted);
        setErr(null);
        // A machine that was upgraded since the last visit answers now: take
        // the remembered refusal back off rather than staying hidden until the
        // app is relaunched.
        setUnsupported(false);
      } catch (e) {
        if (signal?.aborted) return;
        if (
          e instanceof GatewayError &&
          (e.status === 404 || e.status === 501)
        ) {
          setUnsupported(true);
          setDevices([]);
          setErr(null);
          return;
        }
        setDevices([]);
        setErr(e instanceof GatewayError ? e.message : String(e));
      }
    },
    [client, gateway.url],
  );

  useEffect(() => {
    const ctrl = new AbortController();
    void load(ctrl.signal);
    return () => ctrl.abort();
  }, [load]);

  const token = cachedPushToken();
  // This device can appear in the list under either of its names: its push token,
  // or the relay grant a machine without a signing key was handed instead.
  const [masks, setMasks] = useState<string[]>([]);
  const [areMasksRead, setAreMasksRead] = useState(false);
  useEffect(() => {
    let stale = false;
    void (async () => {
      const ids = await registeredIds(token ?? "");
      if (stale) return;
      setMasks(ids.map(maskToken));
      setAreMasksRead(true);
    })();
    return () => {
      stale = true;
    };
  }, [token, devices]);
  const supported = isPushSupported();

  const enable = useCallback(async () => {
    setBusy("enable");
    setErr(null);
    try {
      const fresh = await acquirePushToken();
      await applyGatewayNotify(gateway.url, true, () =>
        registerForPush(deviceRegistration(fresh), client.pushTarget()),
      );
      setNotify(true);
      await load();
    } catch (e) {
      // This device's answer may already be stored even though the machine refused
      // the call, so show what this device WILL do once it can reach it again.
      setNotify(await getGatewayNotify(gateway.url));
      setErr(e instanceof GatewayError ? e.message : (e as Error).message);
    } finally {
      setBusy(null);
    }
  }, [client, gateway.url, load]);

  const disable = useCallback(async () => {
    // Never gated on holding the OS token: this machine may know this device by
    // the relay GRANT instead, and a token this run was not given is no reason
    // to drop the user's answer on the floor. `unregisterFromPush` names every
    // id the machine could have filed it under, and the answer is stored first
    // so an unreachable machine is still silenced by the next sweep.
    const current = cachedPushToken() ?? "";
    setBusy("disable");
    setErr(null);
    try {
      await applyGatewayNotify(gateway.url, false, () =>
        unregisterFromPush(current, client.pushTarget()),
      );
      setNotify(false);
      await load();
    } catch (e) {
      setNotify(await getGatewayNotify(gateway.url));
      setErr(e instanceof GatewayError ? e.message : (e as Error).message);
    } finally {
      setBusy(null);
    }
  }, [client, gateway.url, load]);

  // Push has two independent halves; this device only cares about its own. An
  // iOS-only gateway can sign for an iPhone and not for a Pixel, so the verdict
  // is per platform, never the summary flag.
  const provider = pushPlatform() === "android" ? push?.fcm : push?.apns;
  // A machine holding no signing key is not silent: it reaches this device
  // through a relay, which needs nothing configured on either side — the app
  // was built naming one, and so was the gateway.
  const relayUrl = relayUrlFor(push ?? undefined, pushPlatform());
  // The one way that breaks is an operator who named an address we refuse. That
  // is a MISCONFIGURED machine, not a machine without credentials — and the
  // address is the only part its operator can fix.
  const refusedRelay = refusedRelayUrl(push ?? undefined, pushPlatform());
  const available =
    Boolean(relayUrl) ||
    (provider ? provider.is_available : (push?.is_available ?? false));

  // The OS outranks everything else: a machine can hold this device's token and
  // still reach nobody, so a blocked permission is never reported as connected.
  const blocked = supported && perm === "denied";
  // THE ROW NEVER FLASHES, AND OPENING IT COSTS NOTHING. Its verdict is
  // assembled from asynchronous answers, so its honest first frame used to be
  // `Checking…` on every open — an amber `Connect` that turned into a quiet
  // `Disconnect` a moment later, on a question whose answer had not changed
  // since the last time this dialog was opened. Reported as: the settings
  // screen flickers, and every paired machine is asked the same thing four or
  // five times over. So the launch/wake sweep settles this verdict for the
  // WHOLE fleet at one request each (`lib/notify.ts`), the row paints from
  // there, and the revalidating read below is answered by that same request
  // (`gateway.ts`).
  const isSettled = devices !== null && areMasksRead;
  const live = isSettled
    ? notifyVerdict({
        isHeld: isHeldBy(devices ?? [], masks),
        isWanted: notify,
        isBlocked: blocked,
      })
    : null;
  const remembered = useMemo(
    () => cachedNotifyVerdict(gateway.url),
    [gateway.url],
  );
  useEffect(() => {
    if (live !== null) rememberNotifyVerdict(gateway.url, live);
  }, [live, gateway.url]);
  const shown = live ?? remembered;

  // Gateway too old to know about push at all: render nothing.
  if (unsupported) return null;

  const machine = gateway.label ?? gatewayHost(gateway.url);
  const checking = shown === null;
  const hasBanner =
    Boolean(err) || !supported || blocked || Boolean(push && !available);

  // Same band rule as the web panel: no address, one switch.

  return (
    <SettingsPanel
      title="Notifications"
      action={
        <NotifyConnectionSwitch
          machine={machine}
          isOn={shown ?? false}
          isBusy={busy !== null}
          isChecking={checking}
          disabled={
            !supported || !available || blocked || checking || busy !== null
          }
          // The mark on the control is what the press must do, so a band painted
          // from the remembered verdict acts on THAT, not on a load still in flight.
          onClick={() => void (shown ? disable() : enable())}
        />
      }
    >
      {hasBanner && (
        <div className="space-y-2 p-3">
          {err && <Banner kind="err">{err}</Banner>}

          {push && !available && refusedRelay && (
            <Banner kind="warn">
              This machine relays notifications through {refusedRelay}, which is
              not https — this device will not hand a push grant to an address
              on the wire. Unset VIS_PUSH_RELAY_URL there and it goes back to
              the relay this app was built with; point it at an https address to
              keep your own.
            </Banner>
          )}

          {push && !available && !refusedRelay && (
            <Banner kind="warn">
              This machine cannot send notifications — it holds no push
              credentials and no relay.
            </Banner>
          )}

          {!supported && (
            <Banner kind="warn">
              Native alerts need the iOS or Android app. The web build can stay
              open instead.
            </Banner>
          )}

          {blocked && (
            <Banner kind="warn">
              Notifications are turned off for Vis in system Settings — turn
              them on there and this device can connect again.
            </Banner>
          )}
        </div>
      )}

      {blocked && canOpenSystemNotificationSettings() && (
        <div className="px-3 pb-3">
          <Button
            variant="secondary"
            density="panel"
            className="w-full"
            onClick={() => openSystemNotificationSettings()}
          >
            Open system Settings
          </Button>
        </div>
      )}
    </SettingsPanel>
  );
}

function gatewayHost(url: string): string {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}

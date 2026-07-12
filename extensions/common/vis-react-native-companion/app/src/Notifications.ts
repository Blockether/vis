import * as Notifications from "expo-notifications";

/* ── local turn-completion notifications ─────────────────────────────────
   The companion streams a session's live events over SSE (see VisClient /
   LiveTurns). When a turn reaches a terminal event (turn.completed /
   turn.failed) and the app is NOT the foreground-active app, we fire a LOCAL
   notification so the operator knows their turn is ready without staring at
   the screen.

   Honest scope: this is best-effort *local* notification, not remote push.
   iOS keeps JS (and the SSE socket) alive for a short window after the app is
   backgrounded, so a turn that finishes in that window pings immediately;
   a turn that finishes after iOS suspends the app pings on the next resume
   when the buffered terminal event is delivered. True "while fully suspended"
   delivery would require APNs push from the gateway (device-token registration
   + a push sender) — a separate, larger feature. */

/* Show a banner (no sound) if a notification ever surfaces while the app is
   foreground-active — we normally suppress those, but keep a sane default. */
Notifications.setNotificationHandler({
  handleNotification: async () => ({
    shouldShowBanner: true,
    shouldShowList: true,
    shouldPlaySound: false,
    shouldSetBadge: false
  })
});

export type NotifyDecision = {
  /* the gateway event type, e.g. "turn.completed" */
  type?: string | undefined;
  /* the user's notifications-enabled preference */
  enabled: boolean;
  /* whether the app is the foreground-active app (AppState === "active") */
  appActive: boolean;
};

/* Pure + testable: notify only for a terminal turn event, only when the user
   enabled notifications, and only when the app is NOT foreground-active (if the
   operator is watching the live stream, the completion is already on screen —
   a banner would be redundant noise). */
export const shouldNotifyTurnDone = ({ type, enabled, appActive }: NotifyDecision): boolean =>
  enabled && !appActive && (type === "turn.completed" || type === "turn.failed");

export type TurnDoneNotice = {
  failed: boolean;
  sessionTitle?: string | undefined;
  request?: string | undefined;
};

/* Pure formatter for the notification title/body — tested directly. */
export const formatTurnDoneNotice = ({
  failed,
  sessionTitle,
  request
}: TurnDoneNotice): { title: string; body: string } => {
  const where = sessionTitle?.trim() || "vis";
  const title = failed ? `${where} — turn failed` : `${where} — turn finished`;
  const ask = request?.trim();
  const body = ask
    ? ask.length > 120
      ? `${ask.slice(0, 117)}\u2026`
      : ask
    : failed
      ? "The turn ended with an error."
      : "Your turn is ready.";
  return { title, body };
};

/* Cached grant flag so notifyTurnDone can no-op cheaply without a native call
   on every event. */
let granted = false;

/* Ask for (or confirm) notification permission. Idempotent — safe to call on
   mount and whenever the user flips the toggle on. Returns the grant. */
export const ensureNotificationPermissions = async (): Promise<boolean> => {
  try {
    const cur = await Notifications.getPermissionsAsync();
    if (cur.granted) {
      granted = true;
      return true;
    }
    if (cur.canAskAgain === false) {
      granted = false;
      return false;
    }
    const req = await Notifications.requestPermissionsAsync();
    granted = req.granted;
    return req.granted;
  } catch {
    granted = false;
    return false;
  }
};

/* Fire the local notification (best-effort; no-op if permission was denied). */
export const notifyTurnDone = async (notice: TurnDoneNotice): Promise<void> => {
  if (!granted) return;
  const { title, body } = formatTurnDoneNotice(notice);
  try {
    await Notifications.scheduleNotificationAsync({
      content: { title, body, sound: false },
      trigger: null
    });
  } catch {
    /* best-effort — a failed schedule must never break the stream */
  }
};

/**
 * System alerts for the desktop app, which is this same bundle inside a Pake (Tauri) window.
 *
 * That window has no Web Push: WKWebView exposes no `PushManager` and no service worker for the
 * app scheme, so nothing can reach it while it is closed. What Pake does grant is the notification
 * plugin's command channel, so alerts are raised HERE, from the fleet the sessions list already
 * polls, for as long as the window is open. A phone or a browser tab still covers the closed app.
 *
 * A banner saying only that a session answered is not worth opening, so WHAT one says is asked of
 * the machine that raised it. `gateway/push.clj` words every alert there, and this window shows
 * the same two lines a phone gets: the answer's own words, or the question that parked the run.
 */
import { desktopInvoke } from './desktop';
import { hostOf } from './endpoints';
import type { FleetMachine } from './fleet';
import { GatewayClient } from './gateway';
import type { PushPermission } from './push';
import { getGatewayNotify } from './storage';
import type { GatewayConn, Session, SessionAlert } from './types';

/** Whether this window raises its own alerts: true only inside the desktop app. */
export function isDesktopNotificationsPlatform(): boolean {
  return desktopInvoke() !== undefined;
}

/**
 * What the desktop host says about alerts. macOS decides delivery in System Settings and tells
 * the app nothing about a refusal, so a granted answer here means the channel is open, not that
 * an alert will appear on screen.
 */
export async function desktopNotificationPermission(): Promise<PushPermission> {
  const invoke = desktopInvoke();
  if (!invoke) return 'unsupported';
  try {
    const granted = await invoke('plugin:notification|is_permission_granted');
    if (granted === true) return 'granted';
    if (granted === false) return 'denied';
    return 'prompt';
  } catch {
    return 'unsupported';
  }
}

/** Ask the host for the alert channel. The first call is what triggers the system prompt. */
export async function requestDesktopNotificationPermission(): Promise<PushPermission> {
  const invoke = desktopInvoke();
  if (!invoke) return 'unsupported';
  try {
    const state = await invoke('plugin:notification|request_permission');
    return state === 'granted' ? 'granted' : 'denied';
  } catch {
    return 'unsupported';
  }
}

/** Raise one system alert. A refused or absent channel leaves the window exactly as it was. */
export async function showDesktopAlert(alert: SessionAlert): Promise<void> {
  const invoke = desktopInvoke();
  if (!invoke) return;
  try {
    await invoke('plugin:notification|notify', {
      options: { title: alert.title, body: alert.body },
    });
  } catch {
    /* The alert is a courtesy; the sessions list stays the record of what happened. */
  }
}

/** What a session had last time this window looked at it. */
interface SessionMark {
  answers: number;
  awaiting: boolean;
}

/** Marks per gateway URL, then per session id, rebuilt each pass so closed sessions drop out. */
let marks = new Map<string, Map<string, SessionMark>>();
let running = false;

const markOf = (session: Session): SessionMark => ({
  answers: session.answer_count ?? 0,
  awaiting: session.is_awaiting_input === true,
});

/** Forget every mark, so the next pass seeds again instead of replaying what is already read. */
export function resetDesktopAlerts(): void {
  marks = new Map();
}

/** How much of the title a machine's own name may take, so the session's name survives it. */
const MACHINE_LIMIT = 24;

/** Cut on a word boundary when one is near enough, and say that the text goes on. */
function clip(text: string, limit: number): string {
  if (text.length <= limit) return text;
  const cut = text.slice(0, limit);
  const space = cut.lastIndexOf(' ');
  return `${(space > limit / 2 ? cut.slice(0, space) : cut).trimEnd()}…`;
}

/** Why a session is worth a banner; what it SAYS is asked of the machine afterwards. */
type AlertReason = 'question' | 'answer';

/** One change this pass found, held until its words have been read off the machine. */
interface PendingAlert {
  conn: GatewayConn;
  /** That machine's own name, as the sessions list shows it. */
  machine: string;
  session: Session;
  reason: AlertReason;
}

/**
 * A question outranks an answer: it is the one that has stopped the session and needs you.
 * A session seen for the first time only alerts when it is already waiting, because its
 * accumulated answers are history, not news.
 */
function reasonFor(before: SessionMark | undefined, now: SessionMark): AlertReason | null {
  if (now.awaiting && !before?.awaiting) return 'question';
  if (before && now.answers > before.answers) return 'answer';
  return null;
}

/**
 * WHICH machine the session runs on, appended while this window watches more than one, because
 * nothing else in a system banner says where it ran. The gateway has already clipped the title to
 * the phone's own budget; a long machine name is cut here so the suffix cannot crowd out the rest.
 */
function withMachine(title: string, machine: string, isFleet: boolean): string {
  return isFleet ? `${title} — ${clip(machine, MACHINE_LIMIT)}` : title;
}

/**
 * All this window knows by itself. The gateway words every alert, so this is reached only when the
 * machine cannot answer — and it then says what `gateway/push.clj` says when a turn leaves nothing
 * readable behind, rather than inventing a second wording.
 */
function unheardAlert({ session, reason }: PendingAlert): SessionAlert {
  return reason === 'question'
    ? { title: 'Action needed', body: 'Vis is waiting on your answer.' }
    : { title: session.title?.trim() || 'Vis', body: 'Turn finished.' };
}

/** What the banner says, asked of the machine that raised it. */
async function alertFor(pending: PendingAlert, isFleet: boolean): Promise<SessionAlert> {
  const { conn, machine, session, reason } = pending;
  const alert = await new GatewayClient(conn)
    .sessionAlert(session.id, reason)
    .catch(() => unheardAlert(pending));
  return { title: withMachine(alert.title, machine, isFleet), body: alert.body };
}

/**
 * Compare the fleet against the last pass and alert on what changed. The first pass for a gateway
 * seeds silently — opening the app is not news — and marks are recorded even while its switch is
 * off, so turning notifications on reports what happens NEXT rather than replaying the backlog.
 * An unreachable machine keeps its marks: a blip must not reseed it into a second first pass.
 */
export async function notifyDesktopFleet(machines: FleetMachine[]): Promise<void> {
  if (!isDesktopNotificationsPlatform() || running) return;
  running = true;
  try {
    const next = new Map<string, Map<string, SessionMark>>();
    const pending: PendingAlert[] = [];
    const watched = new Set<string>();
    for (const machine of machines) {
      const url = machine.conn.url;
      const before = marks.get(url);
      const sessions = machine.error ? null : machine.sessions;
      if (!sessions) {
        if (before) next.set(url, before);
        continue;
      }
      const current = new Map<string, SessionMark>();
      next.set(url, current);
      const label = machine.conn.label?.trim() || hostOf(url);
      const wanted = before !== undefined && (await getGatewayNotify(url));
      if (wanted) watched.add(url);
      for (const session of sessions) {
        const now = markOf(session);
        current.set(session.id, now);
        if (!wanted) continue;
        const reason = reasonFor(before?.get(session.id), now);
        if (reason) pending.push({ conn: machine.conn, machine: label, session, reason });
      }
    }
    marks = next;
    // Every machine is asked at once; the banners themselves stay in the order they were found.
    const alerts = await Promise.all(pending.map((alert) => alertFor(alert, watched.size > 1)));
    for (const alert of alerts) await showDesktopAlert(alert);
  } finally {
    running = false;
  }
}

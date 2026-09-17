/**
 * System alerts for the desktop app, which is this same bundle inside a Pake (Tauri) window.
 *
 * That window has no Web Push: WKWebView exposes no `PushManager` and no service worker for the
 * app scheme, so nothing can reach it while it is closed. What Pake does grant is the notification
 * plugin's command channel, so alerts are raised HERE, from the fleet the sessions list already
 * polls, for as long as the window is open. A phone or a browser tab still covers the closed app.
 */
import { desktopInvoke } from './desktop';
import { hostOf } from './endpoints';
import type { FleetMachine } from './fleet';
import type { PushPermission } from './push';
import { getGatewayNotify } from './storage';
import type { Session } from './types';

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
export async function showDesktopAlert(alert: { title: string; body: string }): Promise<void> {
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

/**
 * A question outranks an answer: it is the one that has stopped the session and needs you.
 * A session seen for the first time only alerts when it is already waiting, because its
 * accumulated answers are history, not news.
 */
function alertFor(
  machine: string,
  session: Session,
  before: SessionMark | undefined,
  now: SessionMark,
): { title: string; body: string } | null {
  const title = session.title?.trim() || 'Vis session';
  if (now.awaiting && !before?.awaiting) {
    return { title: `Action needed — ${title}`, body: `Vis is waiting on your answer on ${machine}.` };
  }
  if (before && now.answers > before.answers) {
    return { title, body: `Vis answered on ${machine}.` };
  }
  return null;
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
    const alerts: { title: string; body: string }[] = [];
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
      for (const session of sessions) {
        const now = markOf(session);
        current.set(session.id, now);
        if (!wanted) continue;
        const alert = alertFor(label, session, before?.get(session.id), now);
        if (alert) alerts.push(alert);
      }
    }
    marks = next;
    for (const alert of alerts) await showDesktopAlert(alert);
  } finally {
    running = false;
  }
}

/**
 * System alerts for the desktop app, which is this same bundle inside a Pake (Tauri) window.
 *
 * That window has no Web Push: WKWebView exposes no `PushManager` and no service worker for the
 * app scheme, so nothing can reach it while it is closed. What Pake does grant is the notification
 * plugin's command channel, so alerts are raised HERE, from the fleet the sessions list already
 * polls, for as long as the window is open. A phone or a browser tab still covers the closed app.
 *
 * A banner saying only that a session answered is not worth opening, so WHAT one says is read off
 * the machine that raised it: the answer's own words, or the question that parked the run — the
 * same two banners `gateway/push.clj` sends a phone.
 */
import { desktopInvoke } from './desktop';
import { hostOf } from './endpoints';
import type { FleetMachine } from './fleet';
import { GatewayClient } from './gateway';
import type { HumanInputRequest } from './human-input';
import type { PushPermission } from './push';
import { getGatewayNotify } from './storage';
import type { ContentBlock, GatewayConn, Session } from './types';

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
 * The banner's own budget, and the two limits `gateway/push.clj` clips a phone alert to: one line
 * of title, about two lines of body. Past them the tail is never read.
 */
const TITLE_LIMIT = 64;

const BODY_LIMIT = 180;

/** How much of the title a machine's own name may take, so the session's name survives it. */
const MACHINE_LIMIT = 24;

/** Cut on a word boundary when one is near enough, and say that the text goes on. */
function clip(text: string, limit: number): string {
  if (text.length <= limit) return text;
  const cut = text.slice(0, limit);
  const space = cut.lastIndexOf(' ');
  return `${(space > limit / 2 ? cut.slice(0, space) : cut).trimEnd()}…`;
}

/** One description, trimmed to the banner's budget, or nothing when there is none. */
function shortened(text: string | undefined): string | null {
  const trimmed = text?.trim();
  return trimmed ? clip(trimmed, BODY_LIMIT) : null;
}

/**
 * A turn's content as plain text — the engine's own `content/text-projection`. Tool and
 * attachment blocks carry no words of their own and never reach a settled answer, so they are
 * the two the banner leaves out.
 */
function turnText(blocks: readonly ContentBlock[] | undefined): string {
  return (blocks ?? [])
    .map((block) => {
      switch (block.type) {
        case 'prose':
          return block.markdown ?? '';
        case 'speech':
        case 'code':
        case 'reasoning':
          return block.text ?? '';
        case 'error':
        case 'notice':
          return block.message ?? '';
        default:
          return '';
      }
    })
    .filter((text) => text.trim().length > 0)
    .join('\n\n');
}

/**
 * The answer, flattened to one banner-safe line, by the rules the phone's alert is built with.
 * Markdown is written for a renderer, not for a banner: fenced code becomes a marker, links keep
 * their label, and emphasis, heading and bullet markers go. nil when nothing readable survives.
 */
function alertBody(blocks: readonly ContentBlock[] | undefined): string | null {
  const text = turnText(blocks)
    .replace(/```[\s\S]*?```/g, ' [code] ')
    .replace(/`([^`]*)`/g, '$1')
    .replace(/!?\[([^\]]*)\]\([^)]*\)/g, '$1')
    .replace(/^\s{0,3}#{1,6}\s*/gm, '')
    .replace(/^\s{0,3}>\s?/gm, '')
    .replace(/^\s{0,3}[-*+]\s+/gm, '• ')
    .replace(/\*\*([^*]+)\*\*/g, '$1')
    .replace(/(^|[^\w*_])[*_]([^*_\n]+)[*_](?!\w)/g, '$1$2')
    .replace(/\s+/g, ' ');
  return shortened(text);
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

/** The newest settled answer in that session's own words, or nothing readable. */
async function answerLine(client: GatewayClient, sid: string): Promise<string | null> {
  try {
    const turn = await client.newestTurn(sid);
    // Council never advances `answer_count`, so a Council turn on top of the transcript is not
    // the answer this banner is about.
    return turn?.request_kind === 'council' ? null : alertBody(turn?.content);
  } catch {
    return null;
  }
}

/** The request the run is parked on, or nothing when the machine cannot say. */
async function parkedRequest(
  client: GatewayClient,
  sid: string,
): Promise<HumanInputRequest | null> {
  try {
    return (await client.inputViews(sid))[0] ?? null;
  } catch {
    return null;
  }
}

/**
 * The session's own name, as the phone shows it — followed by WHICH machine when this window is
 * watching more than one, because nothing else in a system banner says where a session runs. A
 * long machine name is cut first, so the suffix can never crowd out the session's own name.
 */
function alertTitle(title: string, machine: string, isFleet: boolean): string {
  const from = isFleet ? ` — ${clip(machine, MACHINE_LIMIT)}` : '';
  return `${clip(title, TITLE_LIMIT - from.length)}${from}`;
}

/**
 * What the banner says, read off the machine that raised it. A machine that cannot answer keeps
 * the plain line it used to send, so a slow or unreachable gateway still alerts.
 */
async function alertFor(
  pending: PendingAlert,
  isFleet: boolean,
): Promise<{ title: string; body: string }> {
  const { conn, machine, session, reason } = pending;
  const client = new GatewayClient(conn);
  const named = session.title?.trim() || 'Vis session';
  if (reason === 'question') {
    const asked = await parkedRequest(client, session.id);
    return {
      title: alertTitle(`Action needed — ${asked?.title?.trim() || named}`, machine, isFleet),
      body: shortened(asked?.description) ?? `Vis is waiting on your answer on ${machine}.`,
    };
  }
  return {
    title: alertTitle(named, machine, isFleet),
    body: (await answerLine(client, session.id)) ?? `Vis answered on ${machine}.`,
  };
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

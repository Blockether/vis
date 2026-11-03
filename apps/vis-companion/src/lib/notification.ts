import type { SseEvent } from './types';

/** The most toasts that show at the same time. */
export const MAX_TOASTS = 3;

/** One toast that an extension sent with `vis.notify`. */
export interface SessionNotice {
  /** The toast identity. A later notice with the same id replaces the toast. */
  id: string;
  text: string;
  kind: 'neutral' | 'ok' | 'warn' | 'err';
  title?: string;
  /** The extension that sent it. */
  source?: string;
  /** How long it stays, when the extension chose it. */
  durationMs?: number;
}

const KINDS: Record<string, SessionNotice['kind']> = {
  info: 'neutral',
  success: 'ok',
  warn: 'warn',
  error: 'err',
};

function text(value: unknown): string | undefined {
  const trimmed = typeof value === 'string' ? value.trim() : '';
  return trimmed || undefined;
}

/**
 * The notice of a `notification` event, or null for any other event.
 *
 * An extension sends the event with `vis.notify`. The gateway sends it live only,
 * so a screen shows it once and never finds it in a replay.
 */
export function noticeOf(event: SseEvent): SessionNotice | null {
  if (event.type !== 'notification') return null;
  const body = text(event.text);
  if (!body) return null;
  const notice: SessionNotice = {
    id: text(event.notice_id) ?? `${String(event.session_id)}:${String(event.seq)}`,
    text: body,
    kind: KINDS[typeof event.level === 'string' ? event.level : ''] ?? 'neutral',
  };
  const title = text(event.title);
  const source = text(event.extension);
  if (title) notice.title = title;
  if (source) notice.source = source;
  if (typeof event.duration_ms === 'number' && event.duration_ms > 0) notice.durationMs = event.duration_ms;
  return notice;
}

/** How long `notice` stays: its own duration, else longer for a warning or an error. */
export function noticeDuration(notice: SessionNotice): number {
  return notice.durationMs ?? (notice.kind === 'warn' || notice.kind === 'err' ? 8000 : 4000);
}

/**
 * `stack` (newest first) with `notice` on top. A toast with the same id goes away,
 * so one notice that reaches several sessions shows once. At most `MAX_TOASTS` stay.
 */
export function pushNotice(stack: readonly SessionNotice[], notice: SessionNotice): SessionNotice[] {
  return [notice, ...stack.filter((shown) => shown.id !== notice.id)].slice(0, MAX_TOASTS);
}

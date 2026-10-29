import type { SseEvent } from './types';

/** How long a session notice stays on screen, in milliseconds. */
export const NOTICE_MS = 4000;

/** One short notice that an extension sent to the people of a session. */
export interface SessionNotice {
  text: string;
  kind: 'neutral' | 'ok' | 'warn' | 'err';
}

const KINDS: Record<string, SessionNotice['kind']> = {
  info: 'neutral',
  success: 'ok',
  warn: 'warn',
  error: 'err',
};

/**
 * The notice of a `notification` event, or null for any other event.
 *
 * An extension sends the event with `vis.notify`. The gateway sends it live only,
 * so a screen shows it once and never finds it in a replay.
 */
export function noticeOf(event: SseEvent): SessionNotice | null {
  if (event.type !== 'notification') return null;
  const text = typeof event.text === 'string' ? event.text.trim() : '';
  if (!text) return null;
  return { text, kind: KINDS[typeof event.level === 'string' ? event.level : ''] ?? 'neutral' };
}

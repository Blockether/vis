import type { SseEvent } from './types';

/** The most notices that one screen keeps. One shows at a time; the others wait as pages. */
export const MAX_NOTICES = 20;

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

/** How long `notice` shows on screen: its own duration, else longer for a warning or an error. */
export function noticeDuration(notice: SessionNotice): number {
  return notice.durationMs ?? (notice.kind === 'warn' || notice.kind === 'err' ? 8000 : 4000);
}

/** One kept notice with its clock. The clock runs from `since` only while the notice shows. */
export interface HeldNotice extends SessionNotice {
  /** The time left when the clock last stopped. */
  leftMs: number;
  /** When the clock started, or null while the notice waits behind another page. */
  since: number | null;
}

/** The notices of one screen, newest first, and the id of the shown page (null: the newest). */
export interface NoticePages {
  notices: HeldNotice[];
  page: string | null;
}

/** The page that shows: the notice, its 0-based index, the count and the time left. */
export interface NoticePage {
  notice: HeldNotice;
  index: number;
  total: number;
  remainingMs: number;
  durationMs: number;
}

export const NO_NOTICES: NoticePages = { notices: [], page: null };

/** The milliseconds that `notice` still shows at `now`. */
export function remainingMs(notice: HeldNotice, now: number): number {
  return Math.max(0, notice.leftMs - (notice.since === null ? 0 : now - notice.since));
}

function pageIndex(pages: NoticePages): number {
  const index = pages.notices.findIndex((notice) => notice.id === pages.page);
  return index < 0 ? 0 : index;
}

/** The page that shows at `now`, or null without notices. */
export function shownPage(pages: NoticePages, now: number): NoticePage | null {
  const index = pageIndex(pages);
  const notice = pages.notices[index];
  if (!notice) return null;
  return { notice, index, total: pages.notices.length, remainingMs: remainingMs(notice, now), durationMs: noticeDuration(notice) };
}

/**
 * `pages` without the notices whose time ran out, with only the clock of the shown page
 * running. A notice that waits behind another page keeps all its time.
 */
export function settleNotices(pages: NoticePages, now: number): NoticePages {
  const kept = pages.notices.filter((notice) => remainingMs(notice, now) > 0);
  const shown = shownPage({ ...pages, notices: kept }, now)?.notice.id;
  const notices = kept.map((notice) => {
    if (notice.id === shown) return notice.since === null ? { ...notice, since: now } : notice;
    return notice.since === null ? notice : { ...notice, leftMs: remainingMs(notice, now), since: null };
  });
  return { notices, page: pages.page };
}

/**
 * `pages` with `notice` as the newest page, and that page shown. A notice with the same id
 * goes away first, so one notice that reaches several sessions shows once, and its clock
 * starts again. At most `MAX_NOTICES` stay.
 */
export function pushNotice(pages: NoticePages, notice: SessionNotice, now: number): NoticePages {
  const held: HeldNotice = { ...notice, leftMs: noticeDuration(notice), since: null };
  const notices = [held, ...pages.notices.filter((shown) => shown.id !== notice.id)].slice(0, MAX_NOTICES);
  return settleNotices({ notices, page: notice.id }, now);
}

/** `pages` with the next (`delta` 1) or the previous (-1) page shown. The pages wrap. */
export function turnNoticePage(pages: NoticePages, delta: number, now: number): NoticePages {
  const total = pages.notices.length;
  if (total < 2) return pages;
  const index = (((pageIndex(pages) + delta) % total) + total) % total;
  return settleNotices({ ...pages, page: pages.notices[index].id }, now);
}

/** `pages` without notice `id`. When it was the shown page, the notice at the same place shows. */
export function dismissNotice(pages: NoticePages, id: string, now: number): NoticePages {
  const index = pages.notices.findIndex((notice) => notice.id === id);
  if (index < 0) return pages;
  const notices = pages.notices.filter((notice) => notice.id !== id);
  const wasShown = index === pageIndex(pages);
  const page = wasShown ? (notices[Math.min(index, notices.length - 1)]?.id ?? null) : pages.page;
  return settleNotices({ notices, page }, now);
}

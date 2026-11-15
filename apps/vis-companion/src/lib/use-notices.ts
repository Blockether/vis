import { useCallback, useEffect, useState } from 'react';
import {
  NO_NOTICES,
  dismissNotice,
  pushNotice,
  settleNotices,
  shownPage,
  turnNoticePage,
  type NoticePage,
  type NoticePages,
  type SessionNotice,
} from './notification';

/** The toasts of one screen: the page that shows, and the changes that a screen can make. */
export type Notices = {
  page: NoticePage | null;
  /** Show `notice` as the newest page. A notice with the same id goes away first. */
  push: (notice: SessionNotice) => void;
  dismiss: (id: string) => void;
  /** Show the next (1) or the previous (-1) page. */
  turn: (delta: number) => void;
  clear: () => void;
};

/**
 * The toasts of one screen as pages, newest first. One page shows, and only its clock runs;
 * `at` is the time of the last change, so a render reads one clock.
 */
export function useNotices(): Notices {
  const [notices, setNotices] = useState<{ pages: NoticePages; at: number }>({ pages: NO_NOTICES, at: 0 });
  const change = useCallback((next: (pages: NoticePages, now: number) => NoticePages) => {
    setNotices(({ pages }) => {
      const now = Date.now();
      return { pages: next(pages, now), at: now };
    });
  }, []);
  const push = useCallback((notice: SessionNotice) => change((pages, now) => pushNotice(pages, notice, now)), [change]);
  const dismiss = useCallback((id: string) => change((pages, now) => dismissNotice(pages, id, now)), [change]);
  const turn = useCallback((delta: number) => change((pages, now) => turnNoticePage(pages, delta, now)), [change]);
  const clear = useCallback(() => setNotices({ pages: NO_NOTICES, at: 0 }), []);
  const page = shownPage(notices.pages, notices.at);
  const leftMs = page?.remainingMs;
  useEffect(() => {
    if (leftMs === undefined) return;
    const timer = window.setTimeout(() => change(settleNotices), leftMs + 20);
    return () => window.clearTimeout(timer);
  }, [leftMs, notices.at, change]);
  return { page, push, dismiss, turn, clear };
}

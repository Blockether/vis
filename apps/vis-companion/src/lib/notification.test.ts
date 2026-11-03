import { describe, expect, it } from 'vitest';

import {
  MAX_NOTICES,
  NO_NOTICES,
  dismissNotice,
  noticeOf,
  pushNotice,
  remainingMs,
  settleNotices,
  shownPage,
  turnNoticePage,
  type NoticePages,
  type SessionNotice,
} from './notification';
import type { SseEvent } from './types';

function event(value: Record<string, unknown>): SseEvent {
  return { type: 'notification', session_id: 's', seq: 1, ...value } as SseEvent;
}

describe('noticeOf', () => {
  it('gives each vis.notify level its banner kind', () => {
    expect(noticeOf(event({ text: 'Deployed', level: 'success' }))).toEqual({ id: 's:1', text: 'Deployed', kind: 'ok' });
    expect(noticeOf(event({ text: 'Slow', level: 'warn' }))?.kind).toBe('warn');
    expect(noticeOf(event({ text: 'Failed', level: 'error' }))?.kind).toBe('err');
    expect(noticeOf(event({ text: 'Note', level: 'info' }))?.kind).toBe('neutral');
  });

  it('shows an unknown level as information', () => {
    expect(noticeOf(event({ text: 'Note', level: 'loud' }))?.kind).toBe('neutral');
  });

  it('keeps the title, source, duration and notice id of a toast', () => {
    expect(
      noticeOf(
        event({ text: 'Deployed', level: 'success', title: 'Release', extension: 'forge', duration_ms: 5000, notice_id: 'deploy' }),
      ),
    ).toEqual({ id: 'deploy', text: 'Deployed', kind: 'ok', title: 'Release', source: 'forge', durationMs: 5000 });
  });

  it('shows nothing for a blank text or another event', () => {
    expect(noticeOf(event({ text: '  ', level: 'info' }))).toBeNull();
    expect(noticeOf({ type: 'turn.started', session_id: 's', text: 'Deployed' })).toBeNull();
  });
});

function notice(id: string, text = id, durationMs = 4000): SessionNotice {
  return { id, text, kind: 'neutral', durationMs };
}

function pages(...ids: string[]): NoticePages {
  return ids.reduce((shown, id) => pushNotice(shown, notice(id), 0), NO_NOTICES);
}

describe('pushNotice', () => {
  // A notice without a session reaches every open session with one notice id. A phone
  // that follows two of them must show one toast, and "Deploying" must become "Deployed".
  it('replaces the notice with the same id and shows it as the newest page', () => {
    const shown = pushNotice(pages('deploy', 'copy'), notice('deploy', 'Deployed'), 0);
    expect(shown.notices.map((held) => held.text)).toEqual(['Deployed', 'copy']);
    expect(shownPage(shown, 0)?.notice.text).toBe('Deployed');
  });

  it('keeps only the newest notices', () => {
    const ids = Array.from({ length: MAX_NOTICES + 2 }, (_, index) => String(index));
    expect(pages(...ids).notices).toHaveLength(MAX_NOTICES);
  });
});

describe('notice pages', () => {
  // User request: several notices show one at a time with "1/N". A page that waits must
  // keep its time, or a burst of notices leaves before anyone reads it.
  it('counts down only the shown page', () => {
    const later = turnNoticePage(pages('a', 'b'), 1, 600);
    const [b, a] = later.notices;
    expect(shownPage(later, 600)?.notice.id).toBe('a');
    expect(remainingMs(a, 1500)).toBe(3100);
    expect(remainingMs(b, 1500)).toBe(3400);
  });

  it('drops the shown page when its time runs out and shows the next one', () => {
    const settled = settleNotices(pages('a', 'b'), 4000);
    expect(settled.notices.map((held) => held.id)).toEqual(['a']);
    expect(shownPage(settled, 4000)).toMatchObject({ index: 0, total: 1, remainingMs: 4000 });
  });

  it('wraps around, and a dismissed page gives its place to the next notice', () => {
    const last = turnNoticePage(pages('a', 'b', 'c'), -1, 0);
    expect(shownPage(last, 0)).toMatchObject({ index: 2, total: 3 });
    const middle = turnNoticePage(last, -1, 0);
    expect(shownPage(dismissNotice(middle, 'b', 0), 0)).toMatchObject({ index: 1, total: 2, notice: { id: 'a' } });
  });
});

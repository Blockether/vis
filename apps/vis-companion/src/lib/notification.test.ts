import { describe, expect, it } from 'vitest';

import { MAX_TOASTS, noticeOf, pushNotice, type SessionNotice } from './notification';
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

function notice(id: string, text = id): SessionNotice {
  return { id, text, kind: 'neutral' };
}

describe('pushNotice', () => {
  // A notice without a session reaches every open session with one notice id. A phone
  // that follows two of them must show one toast, and "Deploying" must become "Deployed".
  it('replaces the toast with the same id and puts it on top', () => {
    const stack = pushNotice(pushNotice([], notice('deploy', 'Deploying')), notice('copy'));
    expect(pushNotice(stack, notice('deploy', 'Deployed')).map((shown) => shown.text)).toEqual(['Deployed', 'copy']);
  });

  it('keeps only the newest toasts', () => {
    const stack = ['a', 'b', 'c', 'd'].reduce((shown, id) => pushNotice(shown, notice(id)), [] as SessionNotice[]);
    expect(stack.map((shown) => shown.id)).toEqual(['d', 'c', 'b'].slice(0, MAX_TOASTS));
  });
});

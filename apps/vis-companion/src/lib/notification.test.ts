import { describe, expect, it } from 'vitest';

import { noticeOf } from './notification';
import type { SseEvent } from './types';

function event(value: Record<string, unknown>): SseEvent {
  return { type: 'notification', session_id: 's', seq: 1, ...value } as SseEvent;
}

describe('noticeOf', () => {
  it('gives each vis.notify level its banner kind', () => {
    expect(noticeOf(event({ text: 'Deployed', level: 'success' }))).toEqual({ text: 'Deployed', kind: 'ok' });
    expect(noticeOf(event({ text: 'Slow', level: 'warn' }))?.kind).toBe('warn');
    expect(noticeOf(event({ text: 'Failed', level: 'error' }))?.kind).toBe('err');
    expect(noticeOf(event({ text: 'Note', level: 'info' }))?.kind).toBe('neutral');
  });

  it('shows an unknown level as information', () => {
    expect(noticeOf(event({ text: 'Note', level: 'loud' }))?.kind).toBe('neutral');
  });

  it('shows nothing for a blank text or another event', () => {
    expect(noticeOf(event({ text: '  ', level: 'info' }))).toBeNull();
    expect(noticeOf({ type: 'turn.started', session_id: 's', text: 'Deployed' })).toBeNull();
  });
});

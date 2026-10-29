import { describe, expect, it } from 'vitest';

import { failAppAction, runAppAction, SendKeys, settleAppAction } from './app-actions';
import { GatewayError } from './gateway';
import type { ViewActionOutcome } from './view';

const accepted = (extra: Partial<ViewActionOutcome> = {}): ViewActionOutcome => ({
  action: 'draft',
  view_id: 'v1',
  is_accepted: true,
  seq: 7,
  ...extra,
});

describe('settleAppAction', () => {
  it('shows saved only with the acknowledged state version', () => {
    expect(settleAppAction(accepted({ thread_id: 't1' }))).toEqual({
      status: 'saved',
      seq: 7,
      threadId: 't1',
    });
  });

  it('reads the receipt of an accepted Send', () => {
    const result = settleAppAction(
      accepted({
        action: 'submit',
        submission: { id: 's1', idempotency_key: 'k1', status: 'accepted', created_at: 1 },
      }),
    );
    expect(result.submission).toMatchObject({ id: 's1', status: 'accepted' });
  });

  it('reads an invalid answer with its errors', () => {
    const result = settleAppAction({
      action: 'submit',
      view_id: 'v1',
      is_accepted: false,
      reason: 'invalid',
      errors: { name: 'required' },
    });
    expect(result).toEqual({ status: 'invalid', errors: { name: 'required' } });
  });
});

describe('failAppAction', () => {
  it('reads a network failure as offline', () => {
    expect(failAppAction(new GatewayError(0, 'network error: down')).status).toBe('offline');
  });

  it('reads the 409 bodies as conflict, stale revision and closed', () => {
    expect(
      failAppAction(new GatewayError(409, 'conflict', { is_accepted: false, reason: 'conflict', seq: 9 })),
    ).toEqual({ status: 'conflict', seq: 9 });
    expect(
      failAppAction(
        new GatewayError(409, 'stale', { reason: 'stale_revision', seq: 9, revision: 'abc' }),
      ),
    ).toEqual({ status: 'stale', seq: 9, revision: 'abc' });
    expect(failAppAction(new GatewayError(409, 'closed', { reason: 'closed' }))).toEqual({
      status: 'closed',
    });
  });

  it('reads a 409 for an app without its owner as disconnected', () => {
    expect(
      failAppAction(new GatewayError(409, 'disconnected', { is_accepted: false, reason: 'disconnected' })),
    ).toEqual({ status: 'disconnected' });
  });
});

describe('SendKeys', () => {
  it('reuses one key until the host accepts or refuses the Send', async () => {
    let made = 0;
    const keys = new SendKeys(() => `key-${++made}`);
    const sent: string[] = [];
    const attempt = (outcome: () => Promise<ViewActionOutcome>) => {
      sent.push(keys.next());
      return runAppAction(outcome).then((result) => {
        keys.settle(result);
        return result;
      });
    };
    expect(
      (await attempt(() => Promise.reject(new GatewayError(0, 'network error')))).status,
    ).toBe('offline');
    expect((await attempt(() => Promise.reject(new GatewayError(502, 'bad gateway')))).status).toBe(
      'failed',
    );
    expect((await attempt(() => Promise.resolve(accepted({ action: 'submit' })))).status).toBe(
      'saved',
    );
    expect(sent).toEqual(['key-1', 'key-1', 'key-1']);
    expect(keys.current).toBeUndefined();
    expect(keys.next()).toBe('key-2');
  });

  it('frees the key after a conflict, so the next Send is a new one', () => {
    const keys = new SendKeys(() => 'k');
    keys.next();
    keys.settle({ status: 'conflict' });
    expect(keys.current).toBeUndefined();
  });
});

import { describe, expect, it } from 'vitest';
import {
  APP_MIN_GATEWAY_PROTOCOL,
  APP_PROTOCOL,
  compatFromHealth,
  compatOf,
} from './compat';

// Regression: a matching pair showed the full-screen "Update the gateway" wall.
// `client.health()` RESOLVES `undefined` when the gateway answers 304 or a 200
// with an empty body — a daemon restarting behind a tunnel does exactly that —
// and `compatOf(undefined)` read that silence as reason 'unknown', i.e. "the
// gateway is too old". Dismissing the wall only parked the user on Machines,
// so every return to Sessions demanded the update again. Silence is a
// CONNECTION problem, and the connect screens already report it.
describe('compatFromHealth', () => {
  it('returns no verdict when the gateway answered with no body at all', () => {
    expect(compatFromHealth(undefined)).toBeNull();
    expect(compatFromHealth(null)).toBeNull();
  });

  it('still refuses a gateway whose health carries no protocol block', () => {
    const verdict = compatFromHealth({ status: 'ok', id: 'g1' });
    expect(verdict?.reason).toBe('unknown');
    expect(verdict?.isCompatible).toBe(false);
    expect(verdict?.title).toBe('Update the gateway');
  });

  it('accepts a gateway that speaks this app\u2019s protocol', () => {
    const verdict = compatFromHealth({
      status: 'ok',
      protocol: {
        protocol: APP_PROTOCOL,
        min_client: APP_PROTOCOL,
        min_gateway: APP_MIN_GATEWAY_PROTOCOL,
        version: '0.1.28',
      },
    });
    expect(verdict?.isCompatible).toBe(true);
    expect(verdict?.reason).toBe('ok');
    expect(verdict?.upgrade).toBeNull();
  });

  it('names the half that is stale', () => {
    const oldGateway = compatFromHealth({
      protocol: { protocol: APP_MIN_GATEWAY_PROTOCOL - 1 },
    });
    expect(oldGateway?.reason).toBe('gateway-too-old');
    expect(oldGateway?.upgrade).toBe('gateway');

    const oldApp = compatFromHealth({
      protocol: { protocol: APP_PROTOCOL + 1, min_client: APP_PROTOCOL + 1 },
    });
    expect(oldApp?.reason).toBe('client-too-old');
    expect(oldApp?.upgrade).toBe('client');
    expect(oldApp?.title).toBe('Update this app');
  });
});

describe('compatOf', () => {
  it('judges a protocol block on its own', () => {
    expect(compatOf({ protocol: APP_PROTOCOL }).isCompatible).toBe(true);
    expect(compatOf(undefined).reason).toBe('unknown');
  });
});

import { describe, expect, it } from 'vitest';
import appSource from '../App.tsx?raw';
import {
  pushIntentFrom,
  resolvePushIntent,
  RESUMABLE_PUSH_MS,
  type PushIntentState,
} from './push-intent';
import type { GatewayConn } from './types';

const laptop: GatewayConn = { url: 'http://10.0.0.5:7890', id: 'gw-laptop' };
const desktop: GatewayConn = { url: 'http://10.0.0.6:7890', id: 'gw-desktop' };

const TAP = { sessionId: 'sess-42', turnId: 'turn-7', status: 'completed', type: 'turn.end' };

function state(over: Partial<PushIntentState> = {}): PushIntentState {
  return { isRouteApplied: true, conns: [], active: null, now: 1_000, ...over };
}

/**
 * The handler that shipped before this module: read the active gateway AT TAP
 * TIME, return when there is none. Capacitor has already consumed the retained
 * tap by then, so returning loses it for good.
 */
function shippedHandler(tap: typeof TAP, s: PushIntentState): { url: string; sid: string } | null {
  const conn = s.active;
  if (!conn || !tap.sessionId) return null;
  return { url: conn.url, sid: tap.sessionId };
}

describe('pushIntentFrom', () => {
  it('carries the session the alert was about', () => {
    expect(pushIntentFrom(TAP, 1_000)).toEqual({ sessionId: 'sess-42', at: 1_000 });
  });

  it('ignores a notification that names no session', () => {
    expect(pushIntentFrom({ ...TAP, sessionId: undefined }, 1_000)).toBeNull();
    expect(pushIntentFrom({ ...TAP, sessionId: '   ' }, 1_000)).toBeNull();
  });
});

describe('resolvePushIntent', () => {
  it('opens the session on the active gateway', () => {
    const intent = pushIntentFrom(TAP, 1_000);
    expect(resolvePushIntent(intent, state({ conns: [laptop, desktop], active: desktop }))).toEqual({
      action: 'open',
      conn: desktop,
      sid: 'sess-42',
    });
  });

  it('falls back to the only paired machine before one is made active', () => {
    const intent = pushIntentFrom(TAP, 1_000);
    expect(resolvePushIntent(intent, state({ conns: [laptop] }))).toEqual({
      action: 'open',
      conn: laptop,
      sid: 'sess-42',
    });
  });

  it('waits instead of dropping the tap that launched the app', () => {
    const intent = pushIntentFrom(TAP, 1_000);
    // Cold start: the retained tap is replayed into the mount-time listener,
    // before storage is read back and before the launch route is applied.
    expect(resolvePushIntent(intent, state({ isRouteApplied: false }))).toEqual({ action: 'wait' });
    // Route applied, machines still coming off the native bridge.
    expect(resolvePushIntent(intent, state())).toEqual({ action: 'wait' });
  });

  it('forgets a tap nothing could open before it went stale', () => {
    const intent = pushIntentFrom(TAP, 1_000);
    const late = 1_000 + RESUMABLE_PUSH_MS + 1;
    expect(resolvePushIntent(intent, state({ now: late, conns: [laptop], active: laptop }))).toEqual(
      { action: 'drop' },
    );
  });

  it('has nothing to do without an intent', () => {
    expect(resolvePushIntent(null, state({ conns: [laptop], active: laptop }))).toEqual({
      action: 'wait',
    });
  });

  it('opens the session across a full cold start — the shipped handler never did', () => {
    // The states a launch-from-notification walks through, in order.
    const boot: PushIntentState[] = [
      state({ isRouteApplied: false }),
      state({ isRouteApplied: false, conns: [laptop] }),
      state({ conns: [laptop] }),
      state({ conns: [laptop], active: laptop }),
    ];

    const intent = pushIntentFrom(TAP, 1_000);
    const outcomes = boot.map((s) => resolvePushIntent(intent, s));
    // Parked until there is somewhere to land, then opened exactly once.
    expect(outcomes.map((o) => o.action)).toEqual(['wait', 'wait', 'open', 'open']);
    expect(outcomes[2]).toEqual({ action: 'open', conn: laptop, sid: 'sess-42' });

    // The regression this pins: the tap is delivered at boot[0], and the old
    // handler answered it there and only there.
    expect(shippedHandler(TAP, boot[0])).toBeNull();
  });
});

describe('App wiring', () => {
  it('subscribes to taps once, at mount, so the retained cold-start tap is caught', () => {
    const at = appSource.indexOf('onPushTap(');
    expect(at).toBeGreaterThan(-1);
    // Re-subscribing when the active gateway changes would leave a window with
    // no listener attached — precisely when the launch tap is replayed.
    const deps = appSource.slice(at).indexOf('}, [');
    expect(appSource.slice(at).slice(deps, deps + 6)).toBe('}, [])');
  });

  it('parks the tap and drains it through this module', () => {
    expect(appSource).toContain('pushIntentFrom');
    expect(appSource).toContain('resolvePushIntent');
  });
});

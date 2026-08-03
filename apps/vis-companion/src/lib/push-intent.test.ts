import { describe, expect, it } from 'vitest';
import appSource from '../App.tsx?raw';
import {
  pushIntentFrom,
  resolvePushIntent,
  RESUMABLE_PUSH_MS,
  type PushIntentOutcome,
  type PushIntentState,
} from './push-intent';
import { isShellChromeVisible, shellScreen, type ShellScreen } from './shell';
import type { GatewayConn } from './types';

const laptop: GatewayConn = { url: 'http://10.0.0.5:7890', id: 'gw-laptop' };
const desktop: GatewayConn = { url: 'http://10.0.0.6:7890', id: 'gw-desktop' };

const TAP = {
  sessionId: 'sess-42',
  gatewayId: 'gw-laptop',
  turnId: 'turn-7',
  status: 'completed',
  type: 'turn.end',
};

/** An alert whose sender installed no gateway id. */
const ANONYMOUS_TAP = { ...TAP, gatewayId: undefined };

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
    expect(pushIntentFrom(TAP, 1_000)).toEqual({
      sessionId: 'sess-42',
      gatewayId: 'gw-laptop',
      at: 1_000,
    });
    expect(pushIntentFrom(ANONYMOUS_TAP, 1_000)?.gatewayId).toBeNull();
  });

  it('ignores a notification that names no session', () => {
    expect(pushIntentFrom({ ...TAP, sessionId: undefined }, 1_000)).toBeNull();
    expect(pushIntentFrom({ ...TAP, sessionId: '   ' }, 1_000)).toBeNull();
  });
});

describe('resolvePushIntent', () => {
  it('opens the session on the gateway that SENT the alert, not the active one', () => {
    // Session ids are minted per gateway: `sess-42` on the desktop is a 404 at
    // best, and somebody else's session at worst.
    const intent = pushIntentFrom(TAP, 1_000);
    expect(resolvePushIntent(intent, state({ conns: [laptop, desktop], active: desktop }))).toEqual({
      action: 'open',
      conn: laptop,
      sid: 'sess-42',
    });
  });

  it('opens on the sending machine before any of them is made active', () => {
    const intent = pushIntentFrom(TAP, 1_000);
    expect(resolvePushIntent(intent, state({ conns: [laptop] }))).toEqual({
      action: 'open',
      conn: laptop,
      sid: 'sess-42',
    });
  });

  it('falls back to the active machine when the alert names no gateway', () => {
    const intent = pushIntentFrom(ANONYMOUS_TAP, 1_000);
    expect(resolvePushIntent(intent, state({ conns: [laptop, desktop], active: desktop }))).toEqual({
      action: 'open',
      conn: desktop,
      sid: 'sess-42',
    });
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

  it('drops an alert from a machine this phone is not paired with', () => {
    // Every paired machine has reported its id and none of them is the sender,
    // so nothing here can serve that session.
    const intent = pushIntentFrom(TAP, 1_000);
    expect(resolvePushIntent(intent, state({ conns: [desktop], active: desktop }))).toEqual({
      action: 'drop',
    });
  });

  it('waits while a paired machine has not reported its id yet', () => {
    // Hydration, not a verdict: the id lands with the next /healthz.
    const unidentified: GatewayConn = { url: 'http://10.0.0.7:7890' };
    const intent = pushIntentFrom(TAP, 1_000);
    expect(resolvePushIntent(intent, state({ conns: [unidentified, desktop] }))).toEqual({
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

describe('a tapped notification ends on the session screen', () => {
  /**
   * What `App.tsx` renders once the drain has answered: `openGatewaySession`
   * sets `openTarget`, `sessionConn` is `openTarget?.conn ?? active`, and the
   * client + subscription hub are memos over its url/token — so the transport
   * exists on the SAME render the session opens, and `shellScreen` reaches
   * `'session'` without another round trip.
   */
  function screenAfter(outcome: PushIntentOutcome, s: PushIntentState): ShellScreen {
    const openTarget = outcome.action === 'open' ? { conn: outcome.conn, sid: outcome.sid } : null;
    const sessionConn = openTarget?.conn ?? s.active;
    return shellScreen({
      isSessionOpen: !!openTarget,
      isSessionReady: !!sessionConn,
      isIncompatible: false,
      hasConn: s.conns.length > 0 && !!s.active,
      // The cold start's own tab, i.e. the worst case: the Machines tab is what
      // used to win the screen switch.
      tab: 'connect',
    });
  }

  it('opens the session the alert was about, chrome yielded to it', () => {
    const boot: PushIntentState[] = [
      state({ isRouteApplied: false }),
      state({ isRouteApplied: false, conns: [laptop] }),
      state({ conns: [laptop] }),
      state({ conns: [laptop], active: laptop }),
    ];
    const intent = pushIntentFrom(TAP, 1_000);
    const screens = boot.map((s) => screenAfter(resolvePushIntent(intent, s), s));

    // The launch paints the machine list for a beat, then the tapped session
    // takes the shell and keeps it.
    expect(screens).toEqual(['connect', 'connect', 'session', 'session']);
    // And it is the only screen allowed to take the chrome, because it brings
    // its own header and status-bar padding.
    expect(screens.map(isShellChromeVisible)).toEqual([true, true, false, false]);
  });

  it('never reached the session on the shipped handler', () => {
    const cold = state({ isRouteApplied: false });
    // The tap was answered at boot, with no gateway resolved: nothing opened,
    // and the retained tap was gone.
    expect(shippedHandler(TAP, cold)).toBeNull();
    expect(screenAfter({ action: 'drop' }, cold)).toBe('connect');
  });

  it('keeps the transport pinned to the tapped gateway, not the active one', () => {
    // `sessionConn` prefers `openTarget.conn`, so opening a session on a machine
    // that is not the active one still has a client the instant it renders.
    expect(appSource).toContain('const sessionConn = openTarget?.conn ?? active;');
    const s = state({ conns: [laptop, desktop], active: desktop });
    const outcome = resolvePushIntent(pushIntentFrom(TAP, 1_000), s);
    expect(outcome).toEqual({ action: 'open', conn: laptop, sid: 'sess-42' });
    expect(screenAfter(outcome, s)).toBe('session');
  });
});

import { describe, expect, it } from 'vitest';

import { isSessionEntered, screenKey } from './router';

const gw = (url: string) => ({ conn: { url }, sid: 's1' });

describe('screenKey', () => {
  it('is empty for the navigator', () => {
    expect(screenKey(null)).toBe('');
    expect(screenKey(undefined)).toBe('');
  });

  it('separates gateway from session so neither can forge the other', () => {
    // A naive `url + sid` join would make these two collide.
    expect(screenKey({ conn: { url: 'https://a/b' }, sid: 'c' })).not.toBe(
      screenKey({ conn: { url: 'https://a' }, sid: '/bc' }),
    );
  });

  it('is stable for the same session and distinct across gateways', () => {
    expect(screenKey(gw('https://a'))).toBe(screenKey(gw('https://a')));
    expect(screenKey(gw('https://a'))).not.toBe(screenKey(gw('https://b')));
  });
});

describe('isSessionEntered', () => {
  const enter = (target: Parameters<typeof screenKey>[0], from: Parameters<typeof screenKey>[0]) =>
    isSessionEntered(screenKey(from), screenKey(target));

  it('is true when a session opens over the navigator', () => {
    expect(enter(gw('https://a'), null)).toBe(true);
  });

  it('is true when the shell swaps to a different session or gateway', () => {
    expect(isSessionEntered(screenKey(gw('https://a')), screenKey(gw('https://b')))).toBe(true);
    expect(
      isSessionEntered(
        screenKey({ conn: { url: 'https://a' }, sid: 's1' }),
        screenKey({ conn: { url: 'https://a' }, sid: 's2' }),
      ),
    ).toBe(true);
  });

  it('is false for a re-render of the same session', () => {
    expect(enter(gw('https://a'), gw('https://a'))).toBe(false);
  });

  it('is false on the navigator, including the first render', () => {
    expect(isSessionEntered('', '')).toBe(false);
  });

  // The reported bug: tap "new session", open Settings while the POST is in
  // flight, and the create resolves into a screen change under the open modal.
  // The overlay must be dismissed on that landing, not left floating over a
  // transcript whose shell it was never measured against.
  it('dismisses overlays when an in-flight create lands under an open dialog', () => {
    let overlayOpen = false;
    let shown = screenKey(null);

    const navigate = (target: Parameters<typeof screenKey>[0]) => {
      const next = screenKey(target);
      const entered = isSessionEntered(shown, next);
      shown = next;
      if (entered) overlayOpen = false;
    };

    overlayOpen = true; // user opens Settings from the sessions list
    navigate(gw('https://a')); // POST /sessions resolves, App sets openTarget
    expect(overlayOpen).toBe(false);
  });

  // Its mirror: the gateway settings dialog closes the open session itself when
  // it promotes a gateway to primary, and has to outlive that.
  it('leaves an overlay alone when a session closes underneath it', () => {
    expect(isSessionEntered(screenKey(gw('https://a')), screenKey(null))).toBe(false);
  });
});

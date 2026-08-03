import { describe, expect, it } from 'vitest';

import appSource from '../App.tsx?raw';
import sessionScreenSource from '../screens/SessionScreen.tsx?raw';
import { isShellChromeVisible, shellScreen, type ShellState } from './shell';

const state = (over: Partial<ShellState> = {}): ShellState => ({
  isSessionOpen: false,
  isSessionReady: true,
  isIncompatible: false,
  hasConn: true,
  tab: 'sessions',
  ...over,
});

/** Every state the shell can be in, so the invariant below is not spot-checked. */
function everyState(): ShellState[] {
  const bools = [false, true];
  const out: ShellState[] = [];
  for (const isSessionOpen of bools)
    for (const isSessionReady of bools)
      for (const isIncompatible of bools)
        for (const hasConn of bools)
          for (const tab of ['sessions', 'connect'] as const)
            out.push({ isSessionOpen, isSessionReady, isIncompatible, hasConn, tab });
  return out;
}

describe('shell screen', () => {
  // The reported failure: the app was killed, then relaunched by tapping a
  // notification. The cold start routed to `connect` before the saved machines
  // were loaded, the notification then opened its session, and the shell drew
  // the Machines screen with the header and tab bar gone — "Saved machines"
  // under the status bar clock, no way back.
  it('opens the notification session even while the Machines tab is selected', () => {
    expect(shellScreen(state({ isSessionOpen: true, tab: 'connect' }))).toBe('session');
  });

  it('opens the notification session before the machine list has loaded', () => {
    expect(shellScreen(state({ isSessionOpen: true, hasConn: false }))).toBe('session');
  });

  // The chrome is the only thing that pads the top safe area on those screens,
  // so hiding it anywhere but a session puts content under the status bar.
  it('never hides the chrome on a screen that has none of its own', () => {
    for (const s of everyState()) {
      const screen = shellScreen(s);
      if (!isShellChromeVisible(screen)) expect(screen).toBe('session');
    }
  });

  it('hides the chrome exactly when a session is on screen', () => {
    for (const s of everyState()) {
      expect(isShellChromeVisible(shellScreen(s))).toBe(shellScreen(s) !== 'session');
    }
  });

  it('keeps the tabs in charge while nothing is open', () => {
    expect(shellScreen(state({ tab: 'connect' }))).toBe('connect');
    expect(shellScreen(state({ tab: 'sessions' }))).toBe('sessions');
    expect(shellScreen(state({ hasConn: false }))).toBe('connect');
  });

  it('reports an incompatible gateway with the chrome intact', () => {
    expect(shellScreen(state({ isIncompatible: true }))).toBe('incompatible');
    expect(shellScreen(state({ isSessionOpen: true, isIncompatible: true }))).toBe('incompatible');
    expect(isShellChromeVisible('incompatible')).toBe(true);
    // The Machines tab still wins over a version mismatch: it is where the user
    // fixes the pairing.
    expect(shellScreen(state({ isIncompatible: true, tab: 'connect' }))).toBe('connect');
  });

  it('falls back to the list when an open session has no transport yet', () => {
    expect(shellScreen(state({ isSessionOpen: true, isSessionReady: false }))).toBe('sessions');
  });
});

// The chrome is allowed to disappear on exactly one screen, so that screen has
// to bring the status-bar padding itself: the session's own header is the first
// child of its section, and everything else there (the scroller and the loading
// veil, which is scoped to the wrapper BELOW the header) sits under it. Vitest
// runs with no DOM, so the padding is asserted where it is written: in source.
const screenHeaderClass = (source: string, component: string): string => {
  const declared = source.indexOf(component);
  if (declared < 0) throw new Error(`${component} is gone`);
  const header = /<header[^>]*className="([^"]*)"/.exec(source.slice(declared));
  if (!header) throw new Error(`${component} no longer renders a <header>`);
  return header[1];
};

describe('status bar padding', () => {
  it('pads the status bar from the shell chrome', () => {
    expect(screenHeaderClass(appSource, 'function Header({')).toContain(
      'pt-[env(safe-area-inset-top)]',
    );
  });

  it('pads the status bar from the session, the one screen the chrome leaves', () => {
    expect(isShellChromeVisible('session')).toBe(false);
    expect(screenHeaderClass(sessionScreenSource, 'export function SessionScreen({')).toContain(
      'pt-[env(safe-area-inset-top)]',
    );
  });
});

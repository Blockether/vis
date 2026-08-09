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

// Regression, issue #6830218b-c00d-497a-86b9-1a8966cd92ca: returning from a
// session used to unmount the cached fleet, replay its entrance animation, and
// visibly reflow every machine's sessions instead of restoring the previous frame.
describe('retained session list', () => {
  it('keeps the sessions screen mounted while visiting Machines and hides it', () => {
    expect(appSource).toContain('const sessionsMounted = conns.length > 0 && !!active;');
    expect(appSource).toContain('const sessionsVisible = shellView === "sessions";');
    expect(appSource).toContain('className={sessionsVisible ? "h-full" : "hidden"}');
    expect(appSource).toContain('sessionsMounted && (');

    const main = appSource.indexOf('<main');
    const list = appSource.indexOf('{sessionsMounted && (');
    const connect = appSource.indexOf('{shellView === "connect" && (');
    expect(main).toBeGreaterThanOrEqual(0);
    expect(list).toBeGreaterThan(main);
    expect(connect).toBeGreaterThan(list);
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

// Regression, user report (paraphrased: make it ONE screen, with pairing behind a
// single icon, so there are not two tabs): pairing owned a whole tab of a two-tab bar
// for a verb used twice a year, and a second cog sat 40px from the app's own, so two
// different gears meant two different things.
describe('one screen, pairing is a chip', () => {
  it('has no tab bar and no primary navigation at all', () => {
    expect(appSource).not.toContain('export function TabBar');
    expect(appSource).not.toContain('Primary navigation');
    expect(appSource).not.toContain("label: 'Machines'");
    expect(appSource).not.toContain('label: "Machines"');
  });

  it('names the remaining cog Preferences', () => {
    expect(appSource).toContain('label="Open preferences"');
    expect(appSource).toContain('title="Preferences"');
  });

  // Regression, user report ("cog icon", drawn over the word `Preferences`): the bar
  // spelled its verbs as word-buttons, so two nouns held the trailing corner of every
  // screen for orders given twice a session. They are marks now — and a mark is only
  // legible if it is NAMED where a name is read: `aria-label` for the screen reader,
  // `title` for the pointer. An icon-only control without both is a bug.
  it('marks the bar’s two verbs and names them twice', () => {
    expect(appSource).toContain('<SettingsIcon className="size-4" />');
    expect(appSource).toContain('<SearchIcon className="size-4" />');
    expect(appSource).not.toContain('>Preferences</Button>');
    expect(appSource).toContain('label="Search all machines"');
    expect(appSource).toContain('title="Search all machines"');
  });

  // The bar's controls are the app's own, never a hand-rolled slab: two controls that
  // mean the same thing must never look like two different things.
  it('wears the app\'s own controls, not hand-rolled slabs', () => {
    expect(appSource).toContain(
      'import { BackButton, IconButton, SearchField } from "./components/ui";',
    );
    expect(appSource).not.toMatch(/<button\s+type="button"/);
  });

  // Regression, user report ("just search icon that triggers full page search"): the
  // open field was the bar's whole middle at every width — the widest object on a
  // 390px phone, permanently, for a question that is asked in bursts. Pressing the
  // mark turns the screen into the search: the bar becomes a way back plus the field,
  // and the list under it is the answer.
  it('opens search as a page, not a box parked on the bar', () => {
    expect(appSource).toContain('label="Search sessions on every machine"');
    expect(appSource).toContain('placeholder="Search all machines…"');
    expect(appSource).toContain('<BackButton label="Close search"');
    // Nothing scopes it: the list still takes the fleet-wide query as a prop.
    expect(appSource).toContain('query={query}');
    expect(appSource).toContain('onQuery={setQuery}');
    // The rarest verb no longer holds prime real estate beside it.
    expect(appSource).not.toContain('>Pair machine</Button>');
  });

  // Regression, user report ("it should be this search more subtle and looking more
  // connected to our designs"): the field was a hand-rolled white-filled rounded box,
  // taller and louder than every 32px flat control beside it. It is `SearchField` now —
  // the app's own control, on `Button`'s exact metrics, paper at rest.
  it('wears the app\'s own search control, on the button rhythm', () => {
    expect(appSource).toContain('<SearchField');
    expect(appSource).not.toMatch(/<label className="mx-3 flex h-8/);
  });

  it('pairs from Preferences and gives Machines a way back', () => {
    expect(appSource).toContain('onPair={() => setTab("connect")}');
    expect(appSource).not.toContain('onPairMachine');
    expect(appSource).toContain('onClose={hasConn ? () => setTab("sessions") : undefined}');
  });
});

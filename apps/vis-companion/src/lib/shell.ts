// Which screen fills the shell, and whether the shell draws its own chrome.
//
// The two halves used to be decided in separate places in `App.tsx`: the header
// and the tab bar were hidden whenever a session was open, while the screen
// switch only reached the session AFTER the Machines screen had declined it
// (`!hasConn || tab === 'connect'` came first). Every state with a session open
// AND the Machines tab selected — or with no active connection resolved yet —
// therefore rendered the Machines screen with no header and no tab bar: its
// content rode under the status bar (nothing else supplies the top safe-area
// inset) and there was no navigation left on screen at all.
//
// Opening a notification after the app had been killed is a way in: the cold
// start applies the launch route before the saved machines are loaded, which
// parks the shell on `connect`, and the session then opens underneath that tab.
//
// So the decision is made once, here, and the chrome follows from the screen
// instead of from the routing state that chose it.

export type ShellScreen = 'connect' | 'incompatible' | 'session' | 'sessions';

export interface ShellState {
  /** A session is open — `openTarget` is set. */
  isSessionOpen: boolean;
  /** Its transport exists (gateway client + subscription hub). */
  isSessionReady: boolean;
  /** The gateway for this screen speaks a protocol this build cannot use. */
  isIncompatible: boolean;
  /** At least one saved machine, one of them active, and no offline gate. */
  hasConn: boolean;
  /** The tab the shell falls back to when no session is open. */
  tab: 'sessions' | 'connect';
}

/**
 * An open session outranks the tabs: a tab is where the shell FALLS BACK when
 * nothing is open, and letting it win is what stole the chrome from Machines.
 */
export function shellScreen(state: ShellState): ShellScreen {
  if (state.isSessionOpen && state.isIncompatible) return 'incompatible';
  if (state.isSessionOpen && state.isSessionReady) return 'session';
  if (!state.hasConn || state.tab === 'connect') return 'connect';
  if (state.isIncompatible) return 'incompatible';
  return 'sessions';
}

/**
 * THE DESK IS A SPLIT: the list stands in a sidebar beside the transcript, the
 * ChatGPT shape, so both of those screens are up at once and neither owns the
 * whole shell. Every other screen fills it alone, on a desk as on a phone.
 */
export function isShellSplit(screen: ShellScreen, isDesk: boolean): boolean {
  return isDesk && (screen === 'session' || screen === 'sessions');
}

/**
 * The shell owns the header and the tab bar for every screen except a session:
 * that one brings its own header, back control and safe-area padding. In the
 * desk's split the shell keeps its bar over both columns — the session's own
 * header is then the transcript's, not the app's.
 */
export function isShellChromeVisible(screen: ShellScreen, isSplit = false): boolean {
  return isSplit || screen !== 'session';
}

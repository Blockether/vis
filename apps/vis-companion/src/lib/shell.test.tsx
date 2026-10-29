// @vitest-environment jsdom
import { screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { describe, expect, it } from 'vitest';

import { renderApp } from '../app-harness';
import { isShellChromeVisible, isShellSplit, shellScreen, type ShellState } from './shell';

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

// The desk is a split: the list and the transcript are up together, so the shell's
// bar stays over both and the session's own header is the transcript's.
describe('the desk split', () => {
  it('splits exactly the two screens that stand side by side', () => {
    const screens = ['connect', 'incompatible', 'session', 'sessions'] as const;
    expect(screens.filter((s) => isShellSplit(s, true))).toEqual(['session', 'sessions']);
    expect(screens.filter((s) => isShellSplit(s, false))).toEqual([]);
  });

  // Regression, user report (paraphrased: there is no button to hide the sidebar):
  // the split had no way out, so a desk could never read one transcript wide.
  it('hands the whole shell to whichever screen is open once the sidebar is put away', () => {
    expect(isShellSplit('session', true, false)).toBe(false);
    expect(isShellSplit('sessions', true, false)).toBe(false);
    expect(isShellSplit('session', true, true)).toBe(true);
  });

  it('keeps the chrome over every desk screen, and drops it over a phone session', () => {
    expect(isShellChromeVisible('session', true)).toBe(true);
    expect(isShellChromeVisible('session', false)).toBe(false);
    expect(isShellChromeVisible('sessions', true)).toBe(true);
  });

  // User request: Zen mode works on the web too. An open session then takes the whole
  // window, with no app bar and no list beside it; every other screen keeps its chrome.
  it('gives a Zen session the whole window and leaves the other screens alone', () => {
    expect(isShellSplit('session', true, true, true)).toBe(false);
    expect(isShellChromeVisible('session', true, true)).toBe(false);
    expect(isShellSplit('sessions', true, true, true)).toBe(true);
    expect(isShellChromeVisible('sessions', true, true)).toBe(true);
    expect(isShellChromeVisible('connect', false, true)).toBe(true);
  });
});

// The shell's own chrome, read off the mounted app. Every claim below used to be
// a string looked up in `App.tsx`, which passes just as happily when the control
// it describes never reaches the screen.
describe('the app bar', () => {
  const mount = async () => {
    const view = renderApp({ machines: [{ label: 'laptop' }] });
    await screen.findByRole('button', { name: 'New project on laptop' });
    return view;
  };

  // Regression, user report ("just search icon that triggers full page search"): the
  // open field was the bar's whole middle at every width — the widest object on a
  // 390px phone, permanently, for a question that is asked in bursts. A later report
  // (paraphrased: search filled the list on the left; give it a dialog of its own)
  // moved the field and its answers out of the list and into a dialog.
  it('opens search as a focused dialog and clears its query on close', async () => {
    const view = await mount();
    await userEvent.click(screen.getByRole('button', { name: 'Search sessions' }));
    const dialog = await screen.findByRole('dialog', { name: 'Search sessions' });
    const field = within(dialog).getByRole('searchbox', {
      name: 'Search session titles and messages',
    });
    expect(field).toHaveFocus();
    expect(within(dialog).getByRole('button', { name: 'Close search' })).toBeVisible();
    // The list behind the dialog stays whole: nothing in it is filtered away.
    expect(screen.getByRole('button', { name: 'New project on laptop' })).toBeVisible();
    await userEvent.type(field, 'needle');
    expect(field).toHaveValue('needle');
    await userEvent.keyboard('{Escape}');
    await waitFor(() =>
      expect(screen.queryByRole('dialog', { name: 'Search sessions' })).not.toBeInTheDocument(),
    );
    // Opening by keyboard is the same dialog, with a fresh query and the caret ready.
    await userEvent.keyboard('/');
    const reopened = await screen.findByRole('searchbox', {
      name: 'Search session titles and messages',
    });
    expect(reopened).toHaveFocus();
    expect(reopened).toHaveValue('');
    await userEvent.click(screen.getByRole('button', { name: 'Close search' }));
    await waitFor(() =>
      expect(screen.queryByRole('dialog', { name: 'Search sessions' })).not.toBeInTheDocument(),
    );
    // `Ctrl+/` is the same door: the chord a desktop or browser reader presses anywhere.
    await userEvent.keyboard('{Control>}/{/Control}');
    expect(
      await screen.findByRole('searchbox', { name: 'Search session titles and messages' }),
    ).toHaveFocus();
    await userEvent.click(screen.getByRole('button', { name: 'Close search' }));
    view.unmount();
    view.restore();
  });

  // Regression, issue #6830218b-c00d-497a-86b9-1a8966cd92ca: returning from a
  // session used to unmount the cached fleet, replay its entrance animation, and
  // visibly reflow every machine's sessions instead of restoring the previous frame.
  // Pairing no longer even leaves the list — it happens inside the cog's dialog —
  // so the same node is not merely kept, it is never hidden.
  it('keeps the sessions screen mounted while pairing, and standing', async () => {
    const view = await mount();
    const main = view.baseElement.querySelector('main') as HTMLElement;
    const list = Array.from(main.children).find(
      (child) => child.className === 'isolate h-full',
    ) as HTMLElement;
    expect(list).toBeInTheDocument();

    await userEvent.click(screen.getByRole('button', { name: 'Open preferences' }));
    // The way in is the band's own + now, so the dialog is what to wait for.
    await screen.findByRole('button', { name: 'Add a machine' });

    // The very same node, still carrying the fleet — never rebuilt, never hidden.
    expect(main.contains(list)).toBe(true);
    expect(within(list).getByRole('button', { name: 'New project on laptop' })).toBeVisible();
    view.unmount();
    view.restore();
  });

  // Regression, user report (a sketch over the cog's dialog retitling it `Settings`,
  // with `Application` on one half and `Gateways` on the other): the shell mounted TWO
  // settings dialogs — one for this device behind the cog, one for a machine behind a
  // list `⋯` — and dismissing either was a different piece of state.
  it('mounts ONE settings dialog, holding both halves', async () => {
    const view = await mount();
    await userEvent.click(screen.getByRole('button', { name: 'Open preferences' }));
    const close = await screen.findByRole('button', { name: 'Close Settings' });
    // One box: this application's appearance AND the machines it talks to — a
    // machine's own verbs wait under its own row, and Escape closes what THEY
    // opened before it closes the dialog under it.
    await userEvent.click(
      within(screen.getByRole('group', { name: 'laptop actions' })).getByRole('button', {
        name: 'Forget',
      }),
    );
    expect(await screen.findByRole('group', { name: 'Forget laptop?' })).toBeVisible();
    await userEvent.keyboard('{Escape}');
    await waitFor(() => expect(screen.queryByRole('group', { name: 'Forget laptop?' })).toBeNull());
    expect(screen.getAllByRole('button', { name: 'Close Settings' })).toHaveLength(1);
    await userEvent.click(close);
    await waitFor(() =>
      expect(screen.queryByRole('button', { name: 'Close Settings' })).toBeNull(),
    );
    view.unmount();
    view.restore();
  });
});

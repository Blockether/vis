// @vitest-environment jsdom
import { screen, waitFor, within } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';

import { renderApp } from '../app-harness';
import { renderSessionScreen } from '../screens/session-screen-harness';
import { IconButton } from '../components/ui';
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
      const where = shellScreen(s);
      if (!isShellChromeVisible(where)) expect(where).toBe('session');
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

// The shell's own chrome, read off the mounted app. Every claim below used to be
// a string looked up in `App.tsx`, which passes just as happily when the control
// it describes never reaches the screen.
describe('the app bar', () => {
  const mount = async () => {
    const view = renderApp({ machines: [{ label: 'laptop' }] });
    await screen.findByRole('button', { name: 'Projects on laptop' });
    return view;
  };

  // The chrome is the only thing that pads the top safe area on these screens,
  // so if it stops doing it the app bar sits under the status bar clock.
  it('pads the status bar from the shell chrome', async () => {
    const view = await mount();
    const header = view.baseElement.querySelector('header');
    expect(header?.className).toContain('pt-[env(safe-area-inset-top)]');
    view.unmount();
    view.restore();
  });

  // Regression, user report (paraphrased: make it ONE screen, with pairing behind a
  // single icon, so there are not two tabs): pairing owned a whole tab of a two-tab
  // bar for a verb used twice a year.
  it('has no tab bar and no primary navigation at all', async () => {
    const view = await mount();
    expect(screen.queryAllByRole('navigation')).toHaveLength(0);
    expect(screen.queryByRole('button', { name: /Machines/ })).toBeNull();
    view.unmount();
    view.restore();
  });

  // Regression, user report ("cog icon", drawn over the word `Preferences`): the bar
  // spelled its verbs as word-buttons, so two nouns held the trailing corner of every
  // screen for orders given twice a session. They are marks now — and a mark is only
  // legible if it is NAMED where a name is read: `aria-label` for the screen reader,
  // `title` for the pointer. An icon-only control without both is a bug.
  it('marks the bar’s two verbs and names them twice', async () => {
    const view = await mount();
    for (const [label, title] of [
      ['Search all machines', 'Search all machines'],
      ['Open preferences', 'Preferences'],
    ]) {
      const mark = screen.getByRole('button', { name: label });
      expect(mark.getAttribute('title')).toBe(title);
      expect(mark.querySelector('svg')).not.toBeNull();
      expect(mark.textContent).toBe('');
    }
    view.unmount();
    view.restore();
  });

  // The bar's controls are the app's own, never a hand-rolled slab: two controls that
  // mean the same thing must never look like two different things.
  it('wears the app’s own controls, not hand-rolled slabs', async () => {
    const view = await mount();
    const own = (
      /class="([^"]*)"/.exec(
        renderToStaticMarkup(
          <IconButton label="x" title="x">
            <span />
          </IconButton>,
        ),
      )?.[1] ?? ''
    )
      .replace(/&quot;/g, '"')
      .split(' ')
      .filter(Boolean);
    expect(own.length).toBeGreaterThan(5);
    const header = view.baseElement.querySelector('header') as HTMLElement;
    const bar = within(header).getAllByRole('button');
    expect(bar).toHaveLength(2);
    for (const control of bar) {
      const worn = new Set(control.className.split(' ').filter(Boolean));
      expect(own.filter((one) => !worn.has(one))).toEqual([]);
    }
    view.unmount();
    view.restore();
  });

  // Regression, user report ("just search icon that triggers full page search"): the
  // open field was the bar's whole middle at every width — the widest object on a
  // 390px phone, permanently, for a question that is asked in bursts.
  it('opens search as a page, not a box parked on the bar', async () => {
    const view = await mount();
    await userEvent.click(screen.getByRole('button', { name: 'Search all machines' }));
    const field = await screen.findByRole('searchbox', {
      name: 'Search sessions on every machine',
    });
    expect(field.getAttribute('placeholder')).toBe('Search all machines…');
    // The page IS the search: the mark that opened it has given the bar up.
    expect(screen.queryByRole('button', { name: 'Search all machines' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Close search' })).toBeTruthy();
    // Nothing scopes it: the fleet list is still the answer underneath.
    expect(screen.getByRole('button', { name: 'Projects on laptop' })).toBeTruthy();
    await userEvent.click(screen.getByRole('button', { name: 'Close search' }));
    await waitFor(() =>
      expect(screen.getByRole('button', { name: 'Search all machines' })).toBeTruthy(),
    );
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
      (child) => child.className === 'h-full',
    ) as HTMLElement;
    expect(list).toBeTruthy();

    await userEvent.click(screen.getByRole('button', { name: 'Open preferences' }));
    // The way in is the band's own + now, so the dialog is what to wait for.
    await screen.findByRole('button', { name: 'Add a machine' });

    // The very same node, still carrying the fleet — never rebuilt, never hidden.
    expect(main.contains(list)).toBe(true);
    expect(list.className).toBe('h-full');
    expect(within(list).getByRole('button', { name: 'Projects on laptop' })).toBeTruthy();
    view.unmount();
    view.restore();
  });

  // Regression, user report (a sketch over the machines screen: "this should open when
  // I click the cog"): the cog opened a dialog whose machine half was a strip of bare
  // NAMES plus a `Pair machine` button whose only act was to CLOSE the dialog and
  // navigate to a screen the app bar has no door to — so the machines this device is
  // paired with, and both ways to add one, were behind nothing the cog could reach.
  it('opens the machines, and both ways to pair, straight from the cog', async () => {
    const view = await mount();
    await userEvent.click(screen.getByRole('button', { name: 'Open preferences' }));
    const dialog = await screen.findByRole('dialog', { name: 'Settings' });

    // The fleet LEADS the dialog: it is what the cog is opened for.
    const headings = within(dialog)
      .getAllByRole('heading')
      .map((heading) => heading.textContent ?? '');
    expect(headings).toContain('Machines');
    expect(headings.indexOf('Machines')).toBeLessThan(headings.indexOf('Application'));

    // A machine is a ROW — its name, its address, its verdict — not a bare tab,
    // and EVERY row keeps the machine's own verbs under its own trailing edge.
    const [row] = within(dialog).getAllByRole('button', { name: /laptop/ });
    expect(row.textContent).toContain('app-gateway');
    expect(
      within(within(dialog).getByRole('group', { name: 'laptop actions' })).getByRole(
        'button',
        { name: 'Forget laptop' },
      ),
    ).toBeTruthy();

    // Pairing is one word in the band, and what it opens stands OVER this dialog
    // rather than inside it: nothing navigates away to reach either way in.
    expect(within(dialog).queryByPlaceholderText(/vis:\/\/gateway/)).toBeNull();
    await userEvent.click(within(dialog).getByRole('button', { name: 'Add a machine' }));
    const sheet = await screen.findByRole('dialog', { name: 'Add a machine' });
    expect(within(sheet).getByPlaceholderText(/vis:\/\/gateway/)).toBeTruthy();
    expect(within(sheet).getByRole('button', { name: 'Scan QR' })).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Pair a machine' })).toBeNull();
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
        name: 'Forget laptop',
      }),
    );
    expect(await screen.findByRole('group', { name: 'Forget laptop?' })).toBeTruthy();
    await userEvent.keyboard('{Escape}');
    await waitFor(() =>
      expect(screen.queryByRole('group', { name: 'Forget laptop?' })).toBeNull(),
    );
    expect(screen.getAllByRole('button', { name: 'Close Settings' })).toHaveLength(1);
    await userEvent.click(close);
    await waitFor(() =>
      expect(screen.queryByRole('button', { name: 'Close Settings' })).toBeNull(),
    );
    view.unmount();
    view.restore();
  });
});

// The chrome is allowed to disappear on exactly one screen, so that screen has to
// bring the status-bar padding itself — otherwise its header sits under the clock.
describe('the session screen, the one screen the chrome leaves', () => {
  it('pads the status bar itself', async () => {
    expect(isShellChromeVisible('session')).toBe(false);
    const view = renderSessionScreen();
    const header = view.baseElement.querySelector('header');
    expect(header?.className).toContain('pt-[env(safe-area-inset-top)]');
    view.unmount();
  });
});

// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { renderApp } from './app-harness';
import { listSession } from './screens/sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
});

// `DESK_RAIL` as `fit-rows.ts` spells it — a wide window under a fine pointer.
const onADesk = () => {
  const previous = window.matchMedia;
  window.matchMedia = ((query: string) => ({
    matches: query.includes('pointer: fine'),
    media: query,
    onchange: null,
    addListener: () => {},
    removeListener: () => {},
    addEventListener: () => {},
    removeEventListener: () => {},
    dispatchEvent: () => false,
  })) as never;
  return () => {
    window.matchMedia = previous;
  };
};

const fleet = () => [
  {
    label: 'laptop',
    sessions: [
      listSession({
        id: 'one',
        title: 'Alpha one',
        workspace: { root: '/Users/dev/alpha' },
        modified_at: '2024-05-01T11:00:00Z',
      }),
    ],
  },
];

// Regression, user report (desktop review: the list was a 1400px table with a second
// fleet index beside it, and opening a session replaced the whole window with the
// transcript): a desk is a SPLIT — the list stands in a sidebar, the conversation
// fills the rest, and nothing navigates away from the list to read one.
describe('a desk keeps the list beside the conversation', () => {
  it('opens a session beside the list, with no way back because nothing was left', async () => {
    const restoreDensity = onADesk();
    const view = renderApp({ machines: fleet() });
    restore = () => {
      view.restore();
      restoreDensity();
    };
    await screen.findByText('Alpha one');
    fireEvent.click(screen.getByText('Alpha one'));
    await screen.findByLabelText('Message Vis');

    // The list did not leave; the empty pane did.
    expect(screen.getByRole('region', { name: 'Sessions' })).toBeVisible();
    expect(screen.queryByRole('region', { name: 'No session open' })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Back to sessions' })).toBeNull();
    // The shell's bar is still over both columns.
    expect(screen.getByRole('button', { name: 'Open preferences' })).toBeVisible();
    view.unmount();
  });

  // Regression, user report (paraphrased: there is no button to hide the sidebar):
  // the split had no way out, so a wide transcript was never readable wide.
  it('puts the sidebar away from the bar, hands the screen to what is open, and brings it back', async () => {
    const restoreDensity = onADesk();
    // The test above left its session in the route; this one starts with none open.
    window.location.hash = '';
    const view = renderApp({ machines: fleet() });
    restore = () => {
      view.restore();
      restoreDensity();
    };
    await screen.findByText('Alpha one');
    fireEvent.click(screen.getByRole('button', { name: 'Hide the session list' }));
    // With nothing open, putting the list away leaves the empty pane and the
    // bar's explicit way back; it never relabels a list that is still visible.
    expect(screen.getByRole('region', { name: 'No session open' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'Show the session list' })).toBeVisible();
    expect(localStorage.getItem('vis.sidebar')).toBe('hidden');

    fireEvent.click(screen.getByRole('button', { name: 'Show the session list' }));
    fireEvent.click(screen.getByText('Alpha one'));
    await screen.findByLabelText('Message Vis');

    fireEvent.click(screen.getByRole('button', { name: 'Hide the session list' }));
    expect(screen.queryByRole('button', { name: 'Back to sessions' })).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Show the session list' }));
    expect(screen.getByRole('region', { name: 'Sessions' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'Hide the session list' })).toBeVisible();
    view.unmount();
  });

  it('still hands a phone the whole screen and a way back', async () => {
    window.location.hash = '';
    const view = renderApp({ machines: fleet() });
    restore = view.restore;
    await screen.findByText('Alpha one');
    expect(screen.queryByRole('region', { name: 'No session open' })).toBeNull();
    fireEvent.click(screen.getByText('Alpha one'));
    await screen.findByLabelText('Message Vis');
    expect(screen.getByRole('button', { name: 'Back to sessions' })).toBeVisible();
    await waitFor(() =>
      expect(screen.queryByRole('button', { name: 'Open preferences' })).toBeNull(),
    );
    view.unmount();
  });

  // Regression, user report (paraphrased: the NEW mark sits on a row whose session is
  // already open, and only writing or sending something retires it): the transcript
  // reports the read mark through its OWN gateway client, so the list beside it went on
  // painting the badge until its next poll answered.
  it('retires the NEW mark on the row whose session the pane is showing', async () => {
    const restoreDensity = onADesk();
    // The tests above leave a session in the route; this one starts with none open.
    window.location.hash = '';
    const view = renderApp({
      machines: [
        {
          label: 'laptop',
          sessions: [
            listSession({
              id: 'one',
              title: 'Alpha one',
              workspace: { root: '/Users/dev/alpha' },
              modified_at: '2024-05-01T11:00:00Z',
              turn_count: 6,
              answer_count: 3,
              is_unread: true,
              unread_answers: 2,
            }),
          ],
        },
      ],
    });
    restore = () => {
      view.restore();
      restoreDensity();
    };
    const mark = await screen.findByText('NEW ×2');
    // In the row's ONE status mark, where the same row otherwise reads IDLE.
    expect(mark.closest('[data-session-status]')).toBeVisible();

    fireEvent.click(screen.getByText('Alpha one'));
    await screen.findByLabelText('Message Vis');
    // Nothing was written and nothing was sent: the conversation on screen is read.
    await waitFor(() => expect(screen.queryByText('NEW ×2')).toBeNull());
    expect(within(screen.getByRole('region', { name: 'Sessions' })).getByText('IDLE')).toBeVisible();
    view.unmount();
  });
});

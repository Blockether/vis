// @vitest-environment jsdom
import { fireEvent, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

// One machine, two projects: `alpha` moved last, so the list's own order puts it on
// top; `beta` is the history under it.
const at = (day: number) => new Date(`2024-05-0${day}T10:00:00Z`).toISOString();

const rows = [
  ...Array.from({ length: 3 }, (_, index) =>
    listSession({
      id: `a${index}`,
      title: `alpha ${index}`,
      workspace: { root: '/Users/dev/alpha' },
      modified_at: at(4 - index),
    }),
  ),
  ...Array.from({ length: 2 }, (_, index) =>
    listSession({
      id: `b${index}`,
      title: `beta ${index}`,
      workspace: { root: '/Users/dev/beta' },
      modified_at: at(2 - index),
    }),
  ),
];

afterEach(() => {
  globalThis.localStorage?.clear();
});

// Regression, user report: a project folded on the sessions list came back OPEN
// after a session was read and left, and every project was open to begin with — so
// a machine with several checkouts painted all of their history at once.
describe('folding a project', () => {
  it('opens the top project and no other', async () => {
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    try {
      await waitFor(() => expect(view.getByText('alpha 0')).toBeVisible());
      // The one project that opens by itself is the one the order put on top.
      expect(view.getByLabelText('Collapse alpha')).toBeVisible();
      expect(view.getByLabelText('Expand beta')).toBeVisible();
      expect(view.queryAllByText(/beta \d/)).toHaveLength(0);
    } finally {
      view.restore();
    }
  });

  it('remembers a fold across a relaunch of the screen', async () => {
    const first = renderSessionsScreen({ machines: [{ sessions: rows }] });
    let conns;
    try {
      await waitFor(() => expect(first.getByText('alpha 0')).toBeVisible());
      conns = first.conns;
      // The reader disagrees with both defaults: the top project is one they are done
      // with, the one under it is the one they are in.
      fireEvent.click(first.getByLabelText('Collapse alpha'));
      fireEvent.click(first.getByLabelText('Expand beta'));
      expect(first.queryAllByText(/alpha \d/)).toHaveLength(0);
      expect(first.queryAllByText(/beta \d/)).toHaveLength(2);
      first.unmount();
    } finally {
      first.restore();
    }

    // The app comes back: the same machine, a screen built from nothing.
    const again = renderSessionsScreen({ machines: [{ sessions: rows }], at: conns });
    try {
      await waitFor(() => expect(again.getByText('beta 0')).toBeVisible());
      expect(again.getByLabelText('Expand alpha')).toBeVisible();
      expect(again.getByLabelText('Collapse beta')).toBeVisible();
      expect(again.queryAllByText(/alpha \d/)).toHaveLength(0);
    } finally {
      again.restore();
    }
  });

  it('shows what a query matched, fold or no fold', async () => {
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    try {
      await waitFor(() => expect(view.getByText('alpha 0')).toBeVisible());
      expect(view.queryAllByText(/beta \d/)).toHaveLength(0);
      // A filter is a fleet-wide question. Answering it with a folded project would
      // be the screen saying it found nothing while holding the row.
      view.setQuery('beta 1');
      await waitFor(() => expect(view.getByText('beta 1')).toBeVisible());
    } finally {
      view.restore();
    }
  });

  // Regression, user report, Vis session 78b0c0b5-f5ba-453f-97ee-af0a85f72d25:
  // a persisted empty project needs a meaningful disclosure: its Sessions and Groups
  // sets expose creation and archived items even when there are no session rows.
  it('discloses an empty project so its sets remain accessible', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [],
          projects: [
            {
              root: '/Users/dev/vis',
              project_id: 'p-vis',
              name: 'vis',
              session_count: 0,
              live_count: 0,
              awaiting_count: 0,
              last_activity_ms: 0,
            },
          ],
        },
      ],
    });
    try {
      await waitFor(() => expect(view.getByText('0 sessions')).toBeVisible());
      expect(view.getByLabelText('Collapse vis')).toBeEnabled();
      expect(
        view.getByRole('button', { name: 'Actions for sessions in /Users/dev/vis' }),
      ).toBeVisible();
      fireEvent.click(view.getByLabelText('Collapse vis'));
      expect(
        view.queryByRole('button', { name: 'Actions for sessions in /Users/dev/vis' }),
      ).toBeNull();
      fireEvent.click(view.getByLabelText('Expand vis'));
      expect(
        view.getByRole('button', { name: 'Actions for sessions in /Users/dev/vis' }),
      ).toBeVisible();
    } finally {
      view.restore();
    }
  });
});

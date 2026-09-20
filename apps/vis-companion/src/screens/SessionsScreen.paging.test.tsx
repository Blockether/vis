// @vitest-environment jsdom
import { fireEvent, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

const at = (rank: number) => new Date(Date.UTC(2024, 4, 1, 10, 0, rank)).toISOString();

// One project deeper than any screen: forty sessions, and an 844px phone holds fifteen
// of them, so the project is three pages long.
const rows = Array.from({ length: 40 }, (_, index) =>
  listSession({
    id: `s${index}`,
    title: `alpha ${String(index).padStart(2, '0')}`,
    workspace: { root: '/Users/dev/alpha' },
    modified_at: at(40 - index),
  }),
);

type View = ReturnType<typeof renderSessionsScreen>;
const shown = (view: View) => view.queryAllByText(/^alpha \d\d$/).map((node) => node.textContent);
const pageReads = (view: View) =>
  view.requests
    // Reads of a project's PAGE: a project's groups are asked for with `?root=` too.
    .filter(({ path }) => path.startsWith('/v1/sessions?') && path.includes('root='))
    .map(({ path }) => decodeURIComponent(path));
const settle = () => new Promise((resolve) => setTimeout(resolve, 60));

afterEach(() => {
  globalThis.localStorage?.clear();
  window.innerHeight = 768;
});

// A pager over a list the client re-filters and re-orders is arithmetic on a lie: the
// gateway counted 1034 sessions in a project this list painted 763 of, so its last page
// sat 27 pages beyond the pager's and the reader watched three rows swap for ten. Every
// page is the gateway's own window now (`GatewayClient.listProjectPage`), asked for at
// the size this screen measured, so the header's count and the pager's arithmetic are
// one number and no page needs the fleet downloaded first.
describe('gateway-backed project pages', () => {
  // Regression: only the machine's head window survived leaving the screen, so
  // one busy project painted first and every other open project filled in later.
  it('loads open projects concurrently and restores their rows before revalidation', async () => {
    window.innerHeight = 844;
    const sessions = [
      ...Array.from({ length: 120 }, (_, index) =>
        listSession({
          id: `alpha-${index}`,
          title: `Alpha ${index}`,
          workspace: { root: '/Users/dev/alpha' },
          modified_at: at(200 - index),
        }),
      ),
      listSession({ id: 'beta', title: 'Beta session', workspace: { root: '/Users/dev/beta' } }),
    ];
    const first = renderSessionsScreen({ machines: [{ sessions, holdsPages: true }] });
    const conns = first.conns;
    try {
      fireEvent.click(await first.findByLabelText('Expand beta'));
      // Both requests are in flight while neither project's page has answered.
      await waitFor(() => expect(pageReads(first)).toHaveLength(2));
      expect(
        pageReads(first).map((read) => new URL(read, conns[0].url).searchParams.get('root')),
      ).toEqual(['/Users/dev/alpha', '/Users/dev/beta']);
      expect(first.queryByText('Beta session')).not.toBeInTheDocument();
      first.releasePages();
      await first.findByText('Beta session');
      await settle();
    } finally {
      first.unmount();
      first.releasePages();
      await settle();
      first.restore();
    }

    const again = renderSessionsScreen({ machines: [{ sessions, holdsList: true }], at: conns });
    try {
      // The same frame contains both projects, without waiting for any response.
      expect(again.getByText('Alpha 0')).toBeVisible();
      expect(again.getByText('Beta session')).toBeVisible();
      expect(again.getByLabelText('Collapse alpha')).toBeVisible();
      expect(again.getByLabelText('Collapse beta')).toBeVisible();
    } finally {
      again.unmount();
      again.releasePages();
      await settle();
      again.restore();
    }
  });

  it('keeps the current page visible and inactive while the project is folded', async () => {
    window.innerHeight = 844;
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    try {
      await waitFor(() => expect(shown(view)).toHaveLength(15));
      await settle();
      fireEvent.click(view.getByLabelText('Next page'));
      await waitFor(() => expect(shown(view)[0]).toBe('alpha 15'));
      await settle();
      const pager = view.getByRole('navigation', { name: 'Pages of alpha sessions' });
      const current = view.getByRole('textbox', { name: 'Current page' });
      const previous = view.getByRole('button', { name: 'Previous page' });
      const next = view.getByRole('button', { name: 'Next page' });
      const reads = pageReads(view).length;

      fireEvent.click(view.getByLabelText('Collapse alpha'));
      expect(view.getByRole('navigation', { name: 'Pages of alpha sessions' })).toBe(pager);
      expect(pager).toBeVisible();
      expect(pager).toHaveAttribute('aria-disabled', 'true');
      expect(current).toHaveValue('2');
      for (const control of [previous, current, next]) expect(control).toBeDisabled();
      fireEvent.click(previous);
      fireEvent.click(next);
      current.focus();
      expect(current).not.toHaveFocus();
      await settle();
      expect(shown(view)).toHaveLength(0);
      expect(pageReads(view)).toHaveLength(reads);
      expect(current).toHaveValue('2');

      fireEvent.click(view.getByLabelText('Expand alpha'));
      await waitFor(() => expect(shown(view)[0]).toBe('alpha 15'));
      expect(view.getByRole('navigation', { name: 'Pages of alpha sessions' })).toBe(pager);
      for (const control of [previous, current, next]) expect(control).toBeEnabled();
      fireEvent.click(next);
      await waitFor(() => expect(current).toHaveValue('3'));
    } finally {
      view.unmount();
      view.restore();
    }
  });

  it('shows an inactive pager for a project restored in its folded state', async () => {
    window.innerHeight = 844;
    const first = renderSessionsScreen({ machines: [{ sessions: rows }] });
    let conns;
    try {
      await waitFor(() => expect(shown(first)).toHaveLength(15));
      fireEvent.click(first.getByLabelText('Collapse alpha'));
      conns = first.conns;
      first.unmount();
    } finally {
      first.restore();
    }
    const again = renderSessionsScreen({ machines: [{ sessions: rows }], at: conns });
    try {
      const pager = await again.findByRole('navigation', { name: 'Pages of alpha sessions' });
      expect(pager).toBeVisible();
      expect(pager).toHaveAttribute('aria-disabled', 'true');
      expect(again.getByRole('textbox', { name: 'Current page' })).toHaveValue('1');
      expect(again.getByRole('textbox', { name: 'Current page' })).toBeDisabled();
      expect(shown(again)).toHaveLength(0);
      expect(pageReads(again)).toHaveLength(0);
    } finally {
      again.unmount();
      again.restore();
    }
  });

  it('asks once for the page on screen, and reads the pages after it ahead', async () => {
    window.innerHeight = 844;
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    try {
      await waitFor(() => expect(shown(view)).toHaveLength(15));
      await settle();

      // The page on screen is ONE read, cut where the screen was measured — not a
      // slice of a fleet this device had to download first. Behind it, and only
      // behind it, the next two pages are warmed: what that buys is their cursors
      // and a validator each, so a page turn is a paint instead of a wait.
      const first = pageReads(view);
      expect(first).toHaveLength(3);
      expect(first[0]).toBe('/v1/sessions?root=/Users/dev/alpha&limit=15');
      expect(first.slice(1).every((read) => read.includes('&after='))).toBe(true);
      expect(first.every((read) => read.includes('limit=15'))).toBe(true);
      expect(shown(view)[0]).toBe('alpha 00');
      expect(view.getAllByText('40 sessions').length).toBeGreaterThan(0);
      expect(view.getByRole('textbox', { name: 'Current page' })).toHaveValue('1');

      // THE TURN COSTS NO ROUND TRIP: the page the reader steps onto is already
      // held, so it paints in the frame of the tap. It used to stand on the page
      // before it until the gateway answered.
      fireEvent.click(view.getByLabelText('Next page'));
      expect(shown(view)[0]).toBe('alpha 15');
      expect(view.getByRole('textbox', { name: 'Current page' })).toHaveValue('2');
      await settle();

      // The last page, tapped from page two, paints the ten rows the header's
      // forty leaves, with no second paint under the thumb.
      fireEvent.click(view.getByLabelText('Next page'));
      await waitFor(() => expect(shown(view)).toHaveLength(10));
      await settle();
      expect(shown(view)).toEqual([
        'alpha 30',
        'alpha 31',
        'alpha 32',
        'alpha 33',
        'alpha 34',
        'alpha 35',
        'alpha 36',
        'alpha 37',
        'alpha 38',
        'alpha 39',
      ]);
      expect(view.getByText('Page 3 of 3')).toHaveAttribute('aria-live', 'polite');
      expect(view.getByLabelText('Next page')).toBeDisabled();
      // Every read is still one page of one project: no walk of the machine.
      expect(pageReads(view).every((read) => read.includes('limit=15'))).toBe(true);
    } finally {
      view.unmount();
      view.restore();
    }
  });

  // Regression, user report: on a slow connection the pager moved to the next
  // number while the rows still belonged to the previous page, making page turns
  // look ignored and sessions appear under the wrong page.
  it.each(['step', 'jump'])(
    'keeps a slow page %s attached to the rows the pager names',
    async (action) => {
      window.innerHeight = 844;
      const view = renderSessionsScreen({
        machines: [{ sessions: rows, holdsDeeperPages: true }],
      });
      try {
        await waitFor(() => expect(shown(view)).toHaveLength(15));
        expect(shown(view)[0]).toBe('alpha 00');
        await waitFor(() => expect(pageReads(view)).toHaveLength(2));

        if (action === 'step') fireEvent.click(view.getByLabelText('Next page'));
        else {
          const current = view.getByRole('textbox', { name: 'Current page' });
          current.focus();
          fireEvent.change(current, { target: { value: '2' } });
          fireEvent.keyDown(current, { key: 'Enter' });
        }

        // Until the slow answer lands, both halves keep saying page one. It used to
        // announce page two over page one's rows for the whole network round trip.
        expect(shown(view)[0]).toBe('alpha 00');
        expect(view.getByRole('textbox', { name: 'Current page' })).toHaveValue('1');
        expect(view.getByText('Page 1 of 3')).toHaveAttribute('aria-live', 'polite');
        expect(view.queryByText('Page 2 of 3')).not.toBeInTheDocument();

        view.releasePages();
        await waitFor(() => expect(shown(view)[0]).toBe('alpha 15'));
        expect(view.getByRole('textbox', { name: 'Current page' })).toHaveValue('2');
        expect(view.getByText('Page 2 of 3')).toHaveAttribute('aria-live', 'polite');
      } finally {
        view.releasePages();
        view.unmount();
        view.restore();
      }
    },
  );

  it('jumps directly to a typed page without visiting the pages between', async () => {
    window.innerHeight = 844;
    const view = renderSessionsScreen({ machines: [{ sessions: rows }] });
    try {
      await waitFor(() => expect(shown(view)).toHaveLength(15));
      await settle();
      const current = view.getByRole('textbox', { name: 'Current page' });
      current.focus();
      fireEvent.change(current, { target: { value: '3' } });
      expect(shown(view)[0]).toBe('alpha 00');
      fireEvent.keyDown(current, { key: 'Enter' });
      expect(shown(view)[0]).toBe('alpha 30');
      expect(shown(view)).toHaveLength(10);
      expect(current).toHaveValue('3');
      expect(view.getByText('Page 3 of 3')).toHaveAttribute('aria-live', 'polite');
      expect(view.getByLabelText('Next page')).toBeDisabled();
    } finally {
      view.unmount();
      view.restore();
    }
  });
});

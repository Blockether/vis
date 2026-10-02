// @vitest-environment jsdom
import { fireEvent, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

let restore = () => {};
afterEach(() => restore());

const rowOrder = () =>
  [...document.querySelectorAll('[data-session-id]')].map((row) =>
    row.getAttribute('data-session-id'),
  );

// Regression, user report: the star must be visible as soon as it changes.
// Updating the mark must not reorder sessions or change the current page.
describe('starring a session', () => {
  const machines = [
    {
      sessions: [
        listSession({
          id: 'older',
          title: 'Older session',
          modified_at: '2024-05-01T09:00:00Z',
        }),
        listSession({
          id: 'newer',
          title: 'Newer session',
          modified_at: '2024-05-01T11:00:00Z',
        }),
      ],
    },
  ];

  it("paints the star's own cell in the brand amber, never the neutral verb ink", async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText('Older session');

    const cell = () => screen.getByRole('group', { name: 'Older session actions' });
    const slab = (label: string) =>
      cell().querySelector(`button[aria-label="${label}"]`)!.className;
    expect(slab('Rename')).not.toContain('bg-accent/15');

    await userEvent.click(cell().querySelector('button[aria-label="Star"]')!);
  });

  it('keeps recency order and brings the starred row back into view', async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText('Older session');
    const before = rowOrder();
    expect(before).toHaveLength(2);
    const last = before[1]!;
    const title = last === 'older' ? 'Older session' : 'Newer session';

    const seen: Element[] = [];
    const scrollIntoView = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = function record(this: Element) {
      seen.push(this);
    };
    try {
      const star = screen
        .getByRole('group', { name: `${title} actions` })
        .querySelector('button[aria-label="Star"]')!;
      await userEvent.click(star);

      // The star changes its mark, not its position in the gateway's recency order.
      await waitFor(() =>
        expect(
          screen.getByRole('group', { name: `${title} actions` })
            .querySelector('button[aria-label="Unstar"]'),
        ).toBeInTheDocument(),
      );
      expect(rowOrder()).toEqual(before);
    } finally {
      Element.prototype.scrollIntoView = scrollIntoView;
    }

    // The row itself is what scrolls back, so the user keeps looking at the session
    // they just starred.
    expect(seen).not.toHaveLength(0);
    expect(
      seen.some((element) =>
        element.contains(document.querySelector(`[data-session-id="${last}"]`)),
      ),
    ).toBe(true);
  });

  // Regression, user report ("the star is not showing on the session row as long
  // as I don't drag to open the session or come back"): the row's own mark was in
  // the DOM the moment the strip was tapped — this is what proves it — and it was
  // painted #ffc420 on #faf3eb paper at 1.45:1, so it could not be SEEN until the
  // list was left and re-entered and the eye went looking for it. The state was
  // never the bug; see `icons.tsx` for the outline that gives the mark a shape.
  it('wears its star on the row the moment the mark is tapped', async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText('Older session');
    const row = () =>
      (document.querySelector('[data-session-id="older"]') as HTMLElement).parentElement!;
    expect(row().querySelector("svg[fill='currentColor']")).toBeNull();

    await userEvent.click(
      screen
        .getByRole('group', { name: 'Older session actions' })
        .querySelector('button[aria-label="Star"]')!,
    );

    // No remount, no reopened list: the same row, in the same commit. One star,
    // not a mark and a control — the row's state IS the way to take it back.
    expect(row().querySelector("svg[fill='currentColor']")).toBeInTheDocument();
    expect(row().querySelectorAll("svg[fill='currentColor']")).toHaveLength(1);
    expect(
      screen
        .getByRole('group', { name: 'Older session actions' })
        .querySelector('button[aria-label="Unstar"]'),
    ).toBeVisible();
  });

  // Regression, user report (paraphrased: the star is in two states at once): the
  // mark was kept in THIS DEVICE's storage, so the machine never heard about it —
  // one screen showed a session starred, another showed it plain, and no answer the
  // gateway could give would settle which was true. The star is its fact now.
  it('tells the gateway, and wears the rank the gateway answers with', async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText('Older session');
    const cell = () => screen.getByRole('group', { name: 'Older session actions' });
    const patched = () => view.requests.filter((request) => request.method === 'PATCH');

    await userEvent.click(cell().querySelector('button[aria-label="Star"]')!);

    expect(patched().map((request) => [request.path, request.body])).toEqual([
      ['/v1/sessions/older', { is_favorite: true }],
    ]);
    // The mark the row wears is the rank that came BACK — there is no local copy of
    // the tap left over to disagree with it.
    expect(await screen.findByRole('button', { name: 'Unstar' })).toBeVisible();

    await userEvent.click(cell().querySelector('button[aria-label="Unstar"]')!);

    expect(patched().map((request) => request.body)).toEqual([
      { is_favorite: true },
      { is_favorite: false },
    ]);
    expect(cell().querySelector('button[aria-label="Star"]')).toBeInTheDocument();
  });
  // Regression, user report on iOS ("when I click the star on some other row, first I
  // don't see the star automatically, only after I do slide once again ... there is
  // some mismatch with the state", with the cell painted over its own old caption):
  // the row starred at the TOP of the list showed its mark at once and every other row
  // did not. A verb closed the drawer by ASKING for an animated slide home while
  // `open` flipped on the spot, and the pin then fired a second animated scroll at
  // that same scroller in the same commit; when the platform ran neither, the strip
  // stood open over a row whose state said shut — and the mark the tap had just left
  // sits at the row's LEADING edge, which is the half a slid-open row hides.
  it('sends the row it pins home in the same tap, not on an animation', async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText('Older session');
    // The row the pin MOVES: the one not already at the top of its project.
    const moved = rowOrder()[1]!;
    const title = moved === 'older' ? 'Older session' : 'Newer session';
    const row = document.querySelector(`[data-session-id="${moved}"]`) as HTMLElement;
    const track = row.closest('[data-swipe-track]') as HTMLElement;

    // A thumb slid it open: the platform scrolls the track, the component reads it.
    Object.defineProperty(track, 'scrollLeft', { value: 216, configurable: true });
    fireEvent.scroll(track);

    const home: ScrollToOptions[] = [];
    const scrollTo = Element.prototype.scrollTo;
    Element.prototype.scrollTo = function record(this: Element, options?: ScrollToOptions) {
      if (this === track && options) home.push(options);
    } as typeof Element.prototype.scrollTo;
    try {
      await userEvent.click(
        screen
          .getByRole('group', { name: `${title} actions` })
          .querySelector('button[aria-label="Star"]')!,
      );
    } finally {
      Element.prototype.scrollTo = scrollTo;
    }

    // Home in the same frame the star was tapped, with no animation that could
    // cover the changed mark.
    expect(home).toEqual([{ left: 0, behavior: 'auto' }]);
    expect(row.parentElement!.querySelector("svg[fill='currentColor']")).toBeInTheDocument();
  });

  // Regression: a row starred on a later page must keep its mark visible there.
  it('keeps a starred row on its current page', async () => {
    const many = Array.from({ length: 17 }, (_, index) =>
      listSession({
        id: `s${String(index + 1).padStart(2, '0')}`,
        title: `Session ${index + 1}`,
        // Descending, so the list order is s01 … s17 before anything is starred.
        modified_at: new Date(Date.UTC(2024, 4, 1, 23 - index, 0, 0)).toISOString(),
      }),
    );
    const view = renderSessionsScreen({ machines: [{ sessions: many }] });
    restore = view.restore;
    await screen.findByText('Session 1');
    expect(rowOrder()).toHaveLength(15);

    await userEvent.click(screen.getByRole('button', { name: 'Next page' }));
    expect(rowOrder()).toEqual(['s16', 's17']);

    await userEvent.click(
      screen
        .getByRole('group', { name: 'Session 17 actions' })
        .querySelector('button[aria-label="Star"]')!,
    );

    // The row stays on page two wearing its mark; starring does not change recency.
    await waitFor(() =>
      expect(
        screen.getByRole('group', { name: 'Session 17 actions' })
          .querySelector('button[aria-label="Unstar"]'),
      ).toBeInTheDocument(),
    );
    expect(rowOrder()).toEqual(['s16', 's17']);
    expect(screen.getByRole('textbox', { name: 'Current page' })).toHaveValue('2');
    const row = document.querySelector('[data-session-id="s17"]')?.parentElement ?? null;
    expect(row).toBeVisible();
    expect(row!.querySelector("svg[fill='currentColor']")).toBeInTheDocument();
  });
  // Regression on iOS: animated scrolling can cover a newly changed star.
  // Place the row and close its swipe strip in the same frame.
  it('places the starred row in the same frame, never on an animation', async () => {
    const view = renderSessionsScreen({ machines });
    restore = view.restore;
    await screen.findByText('Older session');
    // Mark the row not already at the top of its project.
    const marked = rowOrder()[1]!;
    const title = marked === 'older' ? 'Older session' : 'Newer session';

    const asked: (ScrollIntoViewOptions | undefined)[] = [];
    const scrollIntoView = Element.prototype.scrollIntoView;
    Element.prototype.scrollIntoView = function record(
      this: Element,
      options?: boolean | ScrollIntoViewOptions,
    ) {
      asked.push(typeof options === 'object' ? options : undefined);
    } as typeof Element.prototype.scrollIntoView;
    try {
      await userEvent.click(
        screen
          .getByRole('group', { name: `${title} actions` })
          .querySelector('button[aria-label="Star"]')!,
      );
    } finally {
      Element.prototype.scrollIntoView = scrollIntoView;
    }

    expect(asked).not.toHaveLength(0);
    expect(asked[0]).toEqual({
      block: 'nearest',
      inline: 'nearest',
      behavior: 'auto',
    });
    // Nothing in this tap may hand that track an animation the platform can drop.
    expect(asked.some((options) => options?.behavior === 'smooth')).toBe(false);
  });

  // Regression: the leading favorite column indented every title. Keep a stable
  // slot immediately before the status instead, including on unstarred rows.
  it('reserves the favorite slot immediately before the status', async () => {
    const view = renderSessionsScreen({
      machines: [
        {
          sessions: [
            listSession({ id: 'starred', title: 'Starred session', favorite_rank: 1 }),
            listSession({ id: 'plain', title: 'Plain session', favorite_rank: null }),
          ],
        },
      ],
    });
    restore = view.restore;
    await screen.findByText('Starred session');

    const row = (id: string) => document.querySelector(`[data-session-id="${id}"]`) as HTMLElement;
    const favorite = (id: string) =>
      row(id).querySelector('[data-session-favorite-slot]') as HTMLElement;
    const status = (id: string) => row(id).querySelector('[data-session-status]') as HTMLElement;
    const starredFavorite = favorite('starred');
    const plainFavorite = favorite('plain');

    expect(starredFavorite.nextElementSibling).toBe(status('starred'));
    expect(plainFavorite.nextElementSibling).toBe(status('plain'));
    expect(row('starred').firstElementChild).not.toBe(starredFavorite);
    expect(row('plain').firstElementChild).not.toBe(plainFavorite);
    expect(starredFavorite.querySelector("svg[fill='currentColor']")).toBeInTheDocument();
    expect(plainFavorite.querySelector('svg')).toBeNull();
    expect(starredFavorite.className).toBe(plainFavorite.className);
    expect(status('starred').children[0]?.hasAttribute('data-session-status-dot')).toBe(true);
    expect(status('starred').children[1]?.textContent).toBe('IDLE');
    expect(status('plain').children[0]?.hasAttribute('data-session-status-dot')).toBe(true);
    expect(status('plain').children[1]?.textContent).toBe('IDLE');
  });
});

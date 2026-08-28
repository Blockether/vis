// @vitest-environment jsdom
import { fireEvent, screen, waitFor, within } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { listSession, renderSessionsScreen } from './sessions-screen-harness';

/**
 * A desk, as `DESK_RAIL` in `lib/fit-rows.ts` spells it: a window wide enough to
 * stand a rail beside the list, under a pointer that can hit a 28px row. jsdom has
 * no `matchMedia` at all, which is why every other test here stays on the touch
 * paint without asking.
 */
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

/** jsdom lays nothing out, so a test that measures says where the boxes are. */
const boxAt = (element: Element, top: number) => {
  element.getBoundingClientRect = (() => ({
    top,
    bottom: top,
    left: 0,
    right: 0,
    width: 0,
    height: 0,
    x: 0,
    y: top,
    toJSON: () => ({}),
  })) as never;
};

const fleet = () => ({
  machines: [
    {
      label: 'visgw',
      sessions: [
        listSession({ id: 'a1', title: 'First', workspace: { root: '/Users/dev/alpha' } }),
        listSession({ id: 'b1', title: 'Second', workspace: { root: '/Users/dev/beta' } }),
      ],
    },
    {
      label: 'builder',
      sessions: [listSession({ id: 'c1', title: 'Third', workspace: { root: '/Users/dev/gamma' } })],
    },
  ],
});

let restore = () => {};
let restoreDensity = () => {};
afterEach(() => {
  restore();
  restoreDensity();
});

describe("the desk's fleet rail", () => {
  it('names every machine and every project the list is holding', async () => {
    restoreDensity = onADesk();
    const view = renderSessionsScreen(fleet());
    restore = () => {
      view.unmount();
      view.restore();
    };

    await waitFor(() => expect(screen.getByText('First')).toBeTruthy());
    const rail = screen.getByRole('navigation', { name: 'Fleet' });
    expect(within(rail).getByRole('button', { name: /^visgw/ })).toBeTruthy();
    expect(within(rail).getByRole('button', { name: /^builder/ })).toBeTruthy();
    expect(within(rail).getByRole('button', { name: /alpha/ })).toBeTruthy();
    expect(within(rail).getByRole('button', { name: /beta/ })).toBeTruthy();
    // The machine the list is scoped to is the current one, and it is the only one.
    expect(
      within(rail)
        .getAllByRole('button')
        .filter((button) => button.getAttribute('aria-current') === 'true')
        .map((button) => button.textContent),
    ).toHaveLength(1);
  });

  it('leaves no second machine switcher above the list', async () => {
    restoreDensity = onADesk();
    const view = renderSessionsScreen(fleet());
    restore = () => {
      view.unmount();
      view.restore();
    };

    await waitFor(() => expect(screen.getByText('First')).toBeTruthy());
    // The tabs are the phone's way of choosing a machine; on a desk the rail is.
    expect(screen.queryByRole('group', { name: 'Machines' })).toBeNull();

    const rail = screen.getByRole('navigation', { name: 'Fleet' });
    fireEvent.click(within(rail).getByRole('button', { name: /^builder/ }));
    await waitFor(() => expect(screen.getByText('Third')).toBeTruthy());
    expect(screen.queryByText('First')).toBeNull();
  });

  it('takes the list to the band a project names, moving only the list', async () => {
    restoreDensity = onADesk();
    const view = renderSessionsScreen(fleet());
    restore = () => {
      view.unmount();
      view.restore();
    };

    // A band paints from the machine's overview; the rows inside it arrive with
    // that project's own page read, which is not what this is about.
    await waitFor(() =>
      expect(document.querySelector('[data-project-root="/Users/dev/beta"]')).toBeTruthy(),
    );
    const band = document.querySelector('[data-project-root="/Users/dev/beta"]');
    const list = band?.closest("[class*='overflow-y-auto']");
    expect(list).toBeTruthy();
    Object.defineProperty(list, 'scrollTop', { writable: true, value: 0 });
    boxAt(list as Element, 120);
    boxAt(band as Element, 460);

    const rail = screen.getByRole('navigation', { name: 'Fleet' });
    fireEvent.click(within(rail).getByRole('button', { name: /beta/ }));

    // The band's own offset inside the scroller, and nothing above it moved.
    expect((list as HTMLElement).scrollTop).toBe(340);
    expect(window.scrollY).toBe(0);
  });
});

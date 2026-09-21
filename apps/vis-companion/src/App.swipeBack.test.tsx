// @vitest-environment jsdom
import { act, fireEvent, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { renderApp } from './app-harness';
import { listSession } from './screens/sessions-screen-harness';
import { drag, fireTouch } from './lib/pull-to-search.fixture';
import { EDGE_BACK_PX } from './lib/edge-back';

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
});

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

/** Carry a finger in from `x` by `distance`, and lift. */
const swipe = (element: Element, x: number, distance: number) =>
  act(() =>
    drag(element, { x, y: 300 }, [
      { x: x + distance / 2, y: 300 },
      { x: x + distance, y: 300 },
    ]),
  );

// On a phone the transcript is the whole screen, so the stroke that means "back"
// everywhere else on that phone — a finger in from the left edge — has to mean it
// here too, without reaching for the arrow at the top of the glass.
describe('swiping in from the edge of a transcript', () => {
  it('goes back to the session list', async () => {
    window.location.hash = '';
    const view = renderApp({ machines: fleet() });
    restore = view.restore;
    await screen.findByText('Alpha one', {}, { timeout: 5_000 });
    fireEvent.click(screen.getByText('Alpha one'));
    await screen.findByLabelText('Message Vis');
    const pane = (view.baseElement.querySelector('main') as HTMLElement)
      .lastElementChild as HTMLElement;

    swipe(pane, 6, EDGE_BACK_PX + 20);

    await waitFor(() => expect(screen.queryByLabelText('Message Vis')).toBeNull());
    expect(screen.getByRole('region', { name: 'Sessions' })).toBeVisible();
    view.unmount();
  });

  it('keeps the session open for a drag that began inside the transcript', async () => {
    window.location.hash = '';
    const view = renderApp({ machines: fleet() });
    restore = view.restore;
    await screen.findByText('Alpha one', {}, { timeout: 5_000 });
    fireEvent.click(screen.getByText('Alpha one'));
    await screen.findByLabelText('Message Vis');
    const pane = (view.baseElement.querySelector('main') as HTMLElement)
      .lastElementChild as HTMLElement;

    swipe(pane, 200, EDGE_BACK_PX + 120);

    expect(screen.getByLabelText('Message Vis')).toBeVisible();
    view.unmount();
  });

  // Mid-stroke the screen holds both pages at once: the list and the app bar the
  // gesture is uncovering stand behind the transcript, and the transcript is the
  // one that moves.
  it('uncovers the list and the app bar under the moving transcript', async () => {
    window.location.hash = '';
    const view = renderApp({ machines: fleet() });
    restore = view.restore;
    await screen.findByText('Alpha one', {}, { timeout: 5_000 });
    fireEvent.click(screen.getByText('Alpha one'));
    await screen.findByLabelText('Message Vis');
    const pane = (view.baseElement.querySelector('main') as HTMLElement)
      .lastElementChild as HTMLElement;
    // A phone's transcript is the whole screen, so the app bar is away until the
    // stroke starts bringing it back.
    expect(screen.queryByLabelText('Vis')).toBeNull();

    act(() => {
      fireTouch(pane, 'touchstart', [{ x: 6, y: 300 }]);
      fireTouch(pane, 'touchmove', [{ x: 6 + EDGE_BACK_PX, y: 300 }]);
    });

    expect(screen.getByRole('region', { name: 'Sessions' })).toBeVisible();
    expect(screen.getByLabelText('Vis')).toBeVisible();
    expect(screen.getByLabelText('Message Vis')).toBeVisible();
    expect(pane.style.transform).toContain('translate');

    act(() => fireTouch(pane, 'touchcancel', []));
    view.unmount();
  });
});

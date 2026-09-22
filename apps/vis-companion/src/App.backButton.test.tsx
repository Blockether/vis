// @vitest-environment jsdom
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

// The system back crosses the Capacitor bridge, so the plugin is the seam: this is where
// the button is pressed, and what it reaches is the shell's own handler.
const native = vi.hoisted(() => ({
  back: null as (() => void) | null,
  exits: 0,
}));
vi.mock('@capacitor/app', () => ({
  App: {
    addListener: vi.fn(async (event: string, handler: () => void) => {
      if (event === 'backButton') native.back = handler;
      return { remove: vi.fn() };
    }),
    getLaunchUrl: vi.fn(async () => ({ url: '' })),
    exitApp: vi.fn(async () => {
      native.exits += 1;
    }),
  },
}));

import { renderApp } from './app-harness';
import { Modal } from './components/ui';
import { listSession } from './screens/sessions-screen-harness';

let restore = () => {};
afterEach(() => {
  restore();
  restore = () => {};
  native.back = null;
  native.exits = 0;
  vi.clearAllMocks();
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

/** Open the one session in the fixture, and wait until its transcript is on screen. */
async function openSession() {
  window.location.hash = '';
  const view = renderApp({ machines: fleet() });
  restore = view.restore;
  await screen.findByText('Alpha one', {}, { timeout: 5_000 });
  fireEvent.click(screen.getByText('Alpha one'));
  await screen.findByLabelText('Message Vis');
  return view;
}

// Reported: inside an opened LIVE run on Android, back left the SESSION and took the run
// with it. The shell's handler knew the settings sheet, the open session and the tabs, and
// nothing at all about what was standing over them.
describe("the phone's back button", () => {
  it('takes down the dialog standing over the session, and keeps the session', async () => {
    const view = await openSession();
    const onDismiss = vi.fn();
    const dialog = render(
      <Modal onDismiss={onDismiss} within="session">
        <p>an opened run</p>
      </Modal>,
    );

    act(() => native.back?.());

    expect(onDismiss).toHaveBeenCalledTimes(1);
    expect(screen.getByLabelText('Message Vis')).toBeVisible();
    expect(native.exits).toBe(0);
    dialog.unmount();
    view.unmount();
  });

  it('leaves the session when nothing stands over it', async () => {
    const view = await openSession();

    act(() => native.back?.());

    await waitFor(() => expect(screen.queryByLabelText('Message Vis')).toBeNull());
    expect(screen.getByRole('region', { name: 'Sessions' })).toBeVisible();
    expect(native.exits).toBe(0);
    view.unmount();
  });
});

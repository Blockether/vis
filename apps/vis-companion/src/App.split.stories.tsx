import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { useState } from 'react';

import { sidebarRailClass } from './App';
import { STORY_FLEET_CONNS, storyFleetFetch } from './dev/story-data';
import { EmptyPane } from './screens/EmptyPane';
import { SessionsScreen } from './screens/SessionsScreen';

/**
 * THE DESK SPLIT'S ONE MOVING PART: the list's rail going away and coming back.
 *
 * jsdom can read the class the rail wears but not the box it becomes, and the
 * regression this story holds was pure geometry (user report: "the sidebar comes
 * out wrong and now I cannot see it at all"). A rail that dropped its width when it
 * left kept an invisible column of the list's own content standing in the row, so
 * the pane that was supposed to take the whole shell was squeezed beside it —
 * sometimes to nothing. Real CSS, real rows, real boxes.
 */
const meta = {
  title: 'Navigation/Desk split',
  // The rail is a DESK shape: 1280x800, where a third of the shell is wider than
  // the 20rem floor, so both halves of the mirror are exercised.
  globals: { viewport: { value: 'desktop', isRotated: false } },
  beforeEach: () => {
    const previous = globalThis.fetch;
    globalThis.fetch = storyFleetFetch();
    return () => {
      globalThis.fetch = previous;
    };
  },
} satisfies Meta;

export default meta;

type Story = StoryObj<typeof meta>;

/** The shell the desk splits: the list's rail, then the pane that waits for a session. */
function DeskSplit() {
  const [isShown, setShown] = useState(true);
  return (
    <main className="flex h-dvh min-h-0 w-full min-w-0 overflow-x-hidden overflow-y-auto bg-ink">
      <div className={sidebarRailClass(isShown)}>
        <SessionsScreen
          conns={STORY_FLEET_CONNS}
          primary={STORY_FLEET_CONNS[0]}
          query=""
          onQuery={fn()}
          subscriptions={null}
          onOpen={fn()}
          onSearch={fn()}
          isVisible={isShown}
        />
      </div>
      <EmptyPane sidebar={{ isShown, onToggle: () => setShown((shown) => !shown) }} />
    </main>
  );
}

export const RailRidesOffTheSeam: Story = {
  render: () => <DeskSplit />,
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const shell = canvasElement.querySelector('main')!;
    const rail = shell.firstElementChild as HTMLElement;
    const pane = () => page.getByRole('region', { name: 'No session open' }).getBoundingClientRect();
    await page.findByText('uberworkspace');

    const frame = shell.getBoundingClientRect();
    // A third of the shell, never under 20rem, standing on the seam.
    const width = Math.max(shell.clientWidth * 0.33, 320);
    const up = rail.getBoundingClientRect();
    await expect(up.left).toBeCloseTo(frame.left, 1);
    await expect(up.width).toBeCloseTo(width, 1);
    await expect(pane().left).toBeCloseTo(up.right, 1);

    await userEvent.click(page.getByRole('button', { name: 'Hide the session list' }));
    const away = rail.getBoundingClientRect();
    // THE WHOLE POINT: the same box, exactly one width off the seam — so nothing of
    // it is left in the row and the pane behind it takes the shell entire.
    await expect(away.width).toBeCloseTo(up.width, 1);
    await expect(away.right).toBeCloseTo(frame.left, 1);
    await expect(getComputedStyle(rail).visibility).toBe('hidden');
    await expect(pane().left).toBeCloseTo(frame.left, 1);
    await expect(pane().width).toBeCloseTo(frame.width, 1);

    await userEvent.click(page.getByRole('button', { name: 'Show the session list' }));
    const back = rail.getBoundingClientRect();
    await expect(back.left).toBeCloseTo(frame.left, 1);
    await expect(back.width).toBeCloseTo(width, 1);
    await expect(getComputedStyle(rail).visibility).toBe('visible');
    await expect(pane().left).toBeCloseTo(back.right, 1);
    await expect(await page.findByText('uberworkspace')).toBeVisible();
  },
};

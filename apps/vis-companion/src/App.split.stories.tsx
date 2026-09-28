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
    await page.findByText('uberworkspace');

    await userEvent.click(page.getByRole('button', { name: 'Hide the session list' }));

    await userEvent.click(page.getByRole('button', { name: 'Show the session list' }));
    await expect(await page.findByText('uberworkspace')).toBeVisible();
  },
};

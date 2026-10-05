import type { Meta, StoryObj } from '@storybook/react-vite';
import { useRef, type ComponentProps } from 'react';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

import { STORY_OUTLINE_ENTRIES, STORY_OUTLINE_LONG_ENTRIES } from '../dev/story-data';
import { TranscriptOutline } from './TranscriptOutline';

/**
 * THE TRANSCRIPT OUTLINE, over a transcript frame of its own.
 *
 * The rail reads the turn on screen from a real scroller, so each story draws a
 * column of turns that carry `data-turn-id`, the same as the session screen. A jump
 * here scrolls the row into view. The session screen also pages history in first.
 */
type FrameProps = Pick<
  ComponentProps<typeof TranscriptOutline>,
  'entries' | 'total' | 'readAll' | 'onJump'
>;

function OutlineFrame({ entries, total, readAll, onJump }: FrameProps) {
  const scroller = useRef<HTMLDivElement>(null);
  const column = useRef<HTMLDivElement>(null);
  return (
    <div className="relative flex h-dvh w-full flex-col bg-ink">
      <div ref={scroller} className="min-h-0 flex-1 overflow-y-auto" data-testid="transcript">
        <div
          ref={column}
          className="transcript-column mx-auto w-full max-w-3xl pb-10 pt-4 mouse:max-w-6xl"
        >
          {entries.map((entry, index) => (
            <div key={entry.id} data-turn-id={entry.id} className={index === 0 ? '' : 'mt-10'}>
              <p className="font-mono text-ui font-bold text-dialog-foreground">{entry.label}</p>
              <p className="mt-3 min-h-80 text-body text-dialog-hint">The answer to this turn.</p>
            </div>
          ))}
        </div>
      </div>
      <div className="pointer-events-none absolute inset-0 mx-auto flex max-w-3xl items-center justify-end mouse:max-w-6xl">
        <TranscriptOutline
          className="pointer-events-auto"
          entries={entries}
          total={total}
          readAll={readAll}
          scroller={scroller}
          column={column}
          onJump={(id, index) => {
            onJump(id, index);
            const row = Array.from(column.current?.children ?? []).find(
              (child) => child instanceof HTMLElement && child.dataset.turnId === id,
            );
            row?.scrollIntoView({ block: 'start' });
          }}
        />
      </div>
    </div>
  );
}

const meta = {
  title: 'Components/TranscriptOutline',
  component: OutlineFrame,
  parameters: { layout: 'fullscreen' },
  args: {
    entries: STORY_OUTLINE_ENTRIES,
    total: STORY_OUTLINE_ENTRIES.length,
    onJump: fn(),
  },
} satisfies Meta<typeof OutlineFrame>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Every turn is held: one line for each turn, and the panel lists them all. */
export const EveryTurnHeld: Story = {
  play: async ({ args, canvasElement }) => {
    const rail = await within(canvasElement).findByRole('button', { name: 'Jump to a message' });
    await expect(rail.querySelectorAll('span')).toHaveLength(STORY_OUTLINE_ENTRIES.length);
    await waitFor(() => expect(rail.querySelector('[data-active]')).not.toBeNull());

    await userEvent.click(rail);
    const panel = await within(document.body).findByRole('dialog', { name: 'Jump to a message' });
    const rows = within(panel).getAllByRole('button');
    await expect(rows).toHaveLength(STORY_OUTLINE_ENTRIES.length);
    // The turn on screen depends on the layout. Only one row can be the current one.
    await expect(within(panel).getAllByRole('button', { current: true })).toHaveLength(1);
    await expect(within(panel).getByText('council')).toBeVisible();

    await userEvent.click(rows[2]);
    await expect(args.onJump).toHaveBeenCalledWith(STORY_OUTLINE_ENTRIES[2].id, 2);
    await waitFor(() => expect(within(document.body).queryByRole('dialog')).toBeNull());
    await waitFor(() => expect(rail.querySelectorAll('span')[2]).toHaveAttribute('data-active'));
  },
};

/** A long session holds only its newest turns. The panel reads the rest on open. */
export const LongSession: Story = {
  args: {
    entries: STORY_OUTLINE_LONG_ENTRIES.slice(-6),
    total: STORY_OUTLINE_LONG_ENTRIES.length,
    readAll: async () => STORY_OUTLINE_LONG_ENTRIES,
  },
  play: async ({ args, canvasElement }) => {
    const rail = await within(canvasElement).findByRole('button', { name: 'Jump to a message' });
    await expect(rail.querySelectorAll('span')).toHaveLength(12);

    await userEvent.click(rail);
    const panel = await within(document.body).findByRole('dialog', { name: 'Jump to a message' });
    await waitFor(() =>
      expect(within(panel).getAllByRole('button')).toHaveLength(STORY_OUTLINE_LONG_ENTRIES.length),
    );
    const current = within(panel).getByRole('button', { current: true });
    // The turn on screen depends on the layout, but it is always a turn that the screen holds.
    await expect(within(panel).getAllByRole('button').indexOf(current)).toBeGreaterThanOrEqual(
      STORY_OUTLINE_LONG_ENTRIES.length - 6,
    );

    await userEvent.click(within(panel).getAllByRole('button')[0]);
    await expect(args.onJump).toHaveBeenCalledWith(STORY_OUTLINE_LONG_ENTRIES[0].id, 0);
  },
};

/** The gateway does not answer: the panel lists the held turns and says why. */
export const EarlierTurnsUnread: Story = {
  args: {
    entries: STORY_OUTLINE_LONG_ENTRIES.slice(-6),
    total: STORY_OUTLINE_LONG_ENTRIES.length,
    readAll: async () => {
      throw new Error('gateway unavailable');
    },
  },
  play: async ({ canvasElement }) => {
    const rail = await within(canvasElement).findByRole('button', { name: 'Jump to a message' });
    await userEvent.click(rail);
    const panel = await within(document.body).findByRole('dialog', { name: 'Jump to a message' });
    await expect(
      await within(panel).findByText('Earlier turns could not be read. Only loaded turns are listed.'),
    ).toBeVisible();
    await expect(within(panel).getAllByRole('button')).toHaveLength(6);
  },
};

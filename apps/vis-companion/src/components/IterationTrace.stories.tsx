import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import {
  STORY_LISTING,
  STORY_EXCHANGE_TURN,
  STORY_TURN_ITERATIONS,
  STORY_TURN_ITERATIONS_LONG,
  STORY_TURN_ITERATIONS_SETTLED,
} from '../dev/story-data';
import { AssistantMessage, IterationTrace, UserMessage } from './ChatContent';
import { openStepDigests } from '../dev/story-steps';

/**
 * A TURN, DRAWN AS JOINED EXECUTION BANDS.
 *
 * Code, Result and Activity are ordered bands without a left rail.
 * Activity starts shut; its count and status remain visible until expanded.
 * Adjacent bands touch; a new step keeps its own spacing, facts and disclosure state.
 *
 * The data is `STORY_TURN_ITERATIONS` — the same iteration objects the gateway
 * ships — and the activity inside each step is an engine payload parsed by the
 * app's own reader, so a wire change fails this sheet before it reaches a phone.
 */
const meta = {
  title: 'Session/Transcript/Steps',
  component: IterationTrace,
  parameters: { layout: 'fullscreen' },
  // The transcript is a COLUMN, capped and centred (SessionScreen.tsx, the
  // `max-w-3xl` scroller). A sheet that renders the thread across a 900px
  // viewport is reviewing a width the app never paints — the time margin ends
  // up half a screen from the row it belongs to, and the fix goes into the
  // component instead of into the sheet that lied.
  decorators: [
    (Story) => (
      <div className="mx-auto w-full max-w-3xl px-3.5 pt-4 sm:px-6 sm:pt-6">
        <Story />
      </div>
    ),
  ],
  args: { iterations: STORY_TURN_ITERATIONS, whole: true },
} satisfies Meta<typeof IterationTrace>;

export default meta;

type Story = StoryObj<typeof meta>;

/** The turn while it is being written: the last step is open, so the line runs past it. */
export const Running: Story = {
  args: { live: true },
};

/** The same turn once it has landed: every marker closed, the line stopping at the last. */
export const Settled: Story = {
  args: { live: false, iterations: STORY_TURN_ITERATIONS_SETTLED },
};

/**
 * THE SAME THREAD WHEN THE TURN RAN FOR AN HOUR.
 *
 * A trace paints its LAST steps and cuts the rest behind the transcript's own
 * rule, because a turn with a thousand steps in it is not a thread any more —
 * it is a distance. What to look at: the rule reads as a CUT with the count
 * standing in it, the same one earlier turns are folded behind, and the steps
 * under it are the ones the turn ENDED on. Pressing it hands the whole turn
 * back, a chunk per frame.
 */
export const Folded: Story = {
  args: { live: false, whole: false, iterations: STORY_TURN_ITERATIONS_LONG },
};

const forkFromAnswer = fn();

/**
 * THE EXCHANGE — your message and the turn it started, on ONE line.
 *
 * A transcript draws exactly two vertical strokes: the role bar down the human's
 * own bubble, and the thread down the turn that answered it. They are the same
 * column, so they stand on the same x — the eye follows one stroke from what was
 * asked into what the machine did about it, and the bubble's paper begins where
 * a railed reasoning band's does.
 *
 * This is the only sheet where both edges are on screen at once, so it is the
 * one that fails when they drift apart. Look down the left margin, not at the
 * blocks.
 */
export const Exchange: Story = {
  args: { live: false, iterations: STORY_TURN_ITERATIONS_SETTLED },
  render: (args) => (
    <>
      <UserMessage>{STORY_EXCHANGE_TURN.request ?? ''}</UserMessage>
      <AssistantMessage
        turn={{ ...STORY_EXCHANGE_TURN, iterations: args.iterations }}
        whole={args.whole}
        onFork={forkFromAnswer}
      />
    </>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const heading = canvas.getByText('You');
    const answer = canvas.getByText('Vis', { exact: true }).closest('article')!;
    const fork = within(answer).getByRole('button', { name: 'Fork from this turn' });
    await expect(
      within(heading.closest('article')!).queryByRole('button', {
        name: 'Fork from this turn',
      }),
    ).toBeNull();
    await expect(fork).toBeVisible();
    forkFromAnswer.mockClear();
    await userEvent.click(fork);
    await expect(forkFromAnswer).toHaveBeenCalledOnce();
  },
};

export const CodeWithResult: Story = {
  args: {
    live: false,
    showCode: true,
    iterations: STORY_LISTING.map((iteration) => ({
      ...iteration,
      forms: iteration.forms?.map((form) => ({
        ...form,
        stdout: 'Listed 5 entries.',
        duration_ms: 57,
      })),
    })),
  },
  play: async ({ canvasElement }) => {
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    await expect(canvas.queryByRole('button', { name: 'Expand result' })).toBeNull();
    const code = canvasElement.querySelector('[data-execution-code]')!;
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    const band = canvasElement.querySelector('[data-execution-code]')!;
    await expect(band.textContent).not.toContain('Listed 5 entries.');
    canvas.getByRole('button', { name: 'Expand result' }).focus();
    await userEvent.keyboard('{Enter}');
    await expect(band.textContent).toContain('Listed 5 entries.');
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(band.querySelector('summary')).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
    await expect(canvas.queryByText('Listed 5 entries.')).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Expand result' })).toBeNull();
    await expect(code.querySelector('[data-code-body]')).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await expect(canvas.getByRole('button', { name: 'Expand result' })).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
    await expect(canvas.getByRole('button', { name: 'Expand code' })).toBeVisible();
  },
};

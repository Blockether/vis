import type { Meta, StoryObj } from "@storybook/react-vite";
import {
  STORY_EXCHANGE_TURN,
  STORY_TURN_ITERATIONS,
  STORY_TURN_ITERATIONS_ACTIVITY,
  STORY_TURN_ITERATIONS_LONG,
  STORY_TURN_ITERATIONS_SETTLED,
} from "../dev/story-data";
import {
  AssistantMessage,
  IterationTrace,
  UserMessage,
} from "./ChatContent";

/**
 * A TURN, DRAWN AS ONE THREAD.
 *
 * The transcript's unit is not the message, it is the STEP: the model reasons,
 * writes a program, reads what came back, and does it again. Painted as loose
 * blocks those steps read as a pile — nothing says which reasoning belongs to
 * which call, or that the call under your thumb is the fourth of five. So the
 * turn hangs on one vertical line: the line is the turn, a marker on it is a
 * step, and the line stops at the last one, which is the only "this is where it
 * has got to" the screen needs.
 *
 * What to look at, in order:
 *
 * - the line is CONTINUOUS between steps and stops at the last marker, so the
 *   end of the work has a shape and not just an absence;
 * - a thinking band CROSSES the line rather than starting inside it — reasoning
 *   is what a step did first, not a note beside the thread;
 * - a step with no reasoning leaves no hole: the line runs on, the marker still
 *   lands on it;
 * - the running step is the one open ring, and it is the last thing on screen.
 *
 * The data is `STORY_TURN_ITERATIONS` — the same iteration objects the gateway
 * ships — and the activity inside each step is an engine payload parsed by the
 * app's own reader, so a wire change fails this sheet before it reaches a phone.
 */
const meta = {
  title: "Components/Iteration trace",
  component: IterationTrace,
  parameters: { layout: "fullscreen" },
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

/** One step alone — the shortest turn there is, and the case the line must not look broken in. */
export const SingleStep: Story = {
  args: { live: false, iterations: STORY_TURN_ITERATIONS_SETTLED.slice(0, 1) },
};

/** A step that only called: no reasoning above it, and no hole in the thread either. */
export const NoReasoning: Story = {
  args: { live: false, iterations: STORY_TURN_ITERATIONS_SETTLED.slice(1, 2) },
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

/**
 * WHAT HANGS OFF THE LINE, once the iteration has actually done something.
 *
 * The band says what the step COST before anything is opened — did it change the
 * repository, what did it only look at, what did it check. Open it and the step
 * tells the rest, in the order the machine ran it: the program that made every
 * one of those calls, and then what each call did — a search with its answer,
 * the paths a read touched with two more folded behind their count, a patch the
 * repository REFUSED with the head of its output already showing, the patch that
 * landed with its `+7 -3`, a failed check, the step still moving.
 */
export const ActivityAxis: Story = {
  args: { live: true, iterations: STORY_TURN_ITERATIONS_ACTIVITY },
};

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
      <UserMessage onFork={() => {}}>{STORY_EXCHANGE_TURN.request ?? ""}</UserMessage>
      <AssistantMessage
        turn={{ ...STORY_EXCHANGE_TURN, iterations: args.iterations }}
        whole={args.whole}
      />
    </>
  ),
};

/**
 * THE TURN'S OWN VERB, mid-act. A fork is cut AT a turn, on the turn's role line,
 * because that is where the reader can see what they are forking. On a pointer the
 * verb surfaces with the turn under it; while the gateway copies, it says so and
 * takes no second press.
 */
export const Forking: Story = {
  args: { live: false, iterations: STORY_TURN_ITERATIONS_SETTLED },
  render: (args) => (
    <>
      <UserMessage onFork={() => {}} isForking>
        {STORY_EXCHANGE_TURN.request ?? ""}
      </UserMessage>
      <AssistantMessage
        turn={{ ...STORY_EXCHANGE_TURN, iterations: args.iterations }}
        whole={args.whole}
      />
    </>
  ),
};

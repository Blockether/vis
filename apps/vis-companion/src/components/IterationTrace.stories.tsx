import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';
import {
  STORY_JOINED_ACTIVITY,
  STORY_COMPACT_EXECUTIONS,
  STORY_INERT_CLIENT,
  STORY_LISTING,
  STORY_EXCHANGE_TURN,
  STORY_TURN_ITERATIONS,
  STORY_TURN_ITERATIONS_ACTIVITY,
  STORY_TURN_ITERATIONS_LONG,
  STORY_TURN_ITERATIONS_SETTLED,
  STORY_THINKING_AND_CODE,
} from '../dev/story-data';
import { AssistantMessage, IterationTrace, UserMessage } from './ChatContent';
import { ActivityHistoryContext } from './ActivityPanel';
import { GROUPED_ACTIVITY_HISTORY_IDS, groupedActivityHistoryPage } from '../dev/activity-history';

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
  title: 'Components/Iteration trace',
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

/** One step alone — the shortest turn there is, and the case the line must not look broken in. */
export const SingleStep: Story = {
  args: { live: false, iterations: STORY_TURN_ITERATIONS_SETTLED.slice(0, 1) },
};

/** Regression #181: diagnostic details expand without opening the submitted source. */
export const CollapsedToolError: Story = {
  args: {
    live: false,
    showCode: true,
    iterations: [
      {
        id: 'tool-error',
        position: 1,
        forms: [
          {
            source: 'reports = query_reports()\nprint(reports)',
            error: {
              message: 'ToolError: ' + 'Report service unavailable. Retry the query. '.repeat(40),
            },
            duration_ms: 29,
          },
        ],
      },
    ],
  },
  play: async ({ canvas }) => {
    const toggle = canvas.getByRole('button', { name: 'Expand error details' });
    await expect(toggle).toHaveTextContent('Failed');
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
    await expect(canvas.queryByText(/Report service unavailable/)).toBeNull();
    toggle.focus();
    await userEvent.keyboard('{Enter}');
    await expect(toggle).toHaveAttribute('aria-expanded', 'true');
    await expect(canvas.getByText(/Report service unavailable/)).toBeVisible();
    await expect(canvas.queryByRole('button', { name: 'Collapse code' })).toBeNull();
    await userEvent.keyboard(' ');
    await expect(canvas.queryByText(/Report service unavailable/)).toBeNull();
    await userEvent.click(toggle);
    await expect(canvas.getByText(/Report service unavailable/)).toBeVisible();
    await userEvent.click(toggle);
    await expect(canvas.queryByText(/Report service unavailable/)).toBeNull();
  },
};

/** Opening narration uses the role label's gap, without an extra block inset. */
export const OpeningProse: Story = {
  args: {
    live: true,
    iterations: STORY_THINKING_AND_CODE.flatMap((iteration) =>
      [
        'I will check what caused this turn to stop.',
        'The stop event is recorded. I will check its source next.',
      ].map((assistant_prose, index) => ({
        ...iteration,
        id: `${iteration.id}-prose-${index}`,
        thinking: '',
        assistant_prose,
      })),
    ),
  },
  render: (args) => (
    <AssistantMessage
      whole
      streaming
      turn={{
        ...STORY_EXCHANGE_TURN,
        status: 'running',
        iterations: args.iterations,
        content: [],
      }}
    />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const role = canvas.getByText('Vis', { exact: true });
    const header = role.parentElement!;
    const prose = await canvas.findByText('I will check what caused this turn to stop.');
    const later = canvas.getByText('The stop event is recorded. I will check its source next.');
    const code = canvasElement.querySelector('[data-execution-code]')!;
    // Regression: opening prose added its own top padding to the role's margin.
    // Wait for the live segment's entrance animation before measuring its boxes.
    await waitFor(() => {
      expect(prose.getBoundingClientRect().top - header.getBoundingClientRect().bottom).toBeCloseTo(
        parseFloat(getComputedStyle(header).marginBottom),
        0,
      );
      const bottomInset = code.getBoundingClientRect().top - prose.getBoundingClientRect().bottom;
      expect(bottomInset).toBeCloseTo(10, 0);
      const previousActivity = canvasElement.querySelector('[data-execution-activity]')!;
      expect(
        later.getBoundingClientRect().top - previousActivity.getBoundingClientRect().bottom,
      ).toBeCloseTo(bottomInset, 0);
    });
  },
};

/** Regression: prose gaps must not add the previous segment's bottom inset. */
export const ProseSpacing: Story = {
  args: {
    live: false,
    client: STORY_INERT_CLIENT,
    sid: 'prose-spacing',
    iterations: [
      { ...STORY_LISTING[0], id: 'before-prose' },
      {
        ...STORY_LISTING[0],
        id: 'narrated-code',
        assistant_prose: 'I will inspect the existing build without starting another.',
      },
      {
        id: 'prose-only',
        assistant_prose: 'The build is running. I will observe its progress.',
      },
      {
        ...STORY_LISTING[0],
        id: 'prose-with-run',
        assistant_prose: 'The observation is complete. The run is available below.',
        attachments: [
          {
            index: 0,
            iteration_id: 'prose-with-run',
            filename: 'build-observation.live.ndjson',
            media_type: 'application/vnd.vis.live+ndjson',
            size: 3700,
          },
        ],
      },
      {
        id: 'closing-prose',
        assistant_prose: 'The build continues after observation stops.',
      },
    ],
  },
  render: (args) => (
    <AssistantMessage
      whole
      client={args.client}
      sid={args.sid}
      turn={{
        ...STORY_EXCHANGE_TURN,
        iterations: args.iterations,
        content: [{ id: 'answer', type: 'prose', markdown: 'No new build was started.' }],
      }}
    />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const prose = canvas.getAllByText(
      /^(I will inspect|The build is running|The observation is complete|The build continues)/,
      { selector: 'p' },
    );
    const code = canvasElement.querySelectorAll('[data-execution-code]');
    const activity = canvasElement.querySelectorAll('[data-execution-activity]');
    const run = canvas
      .getByRole('button', { name: 'Open run build-observation' })
      .closest('.border-code-edge')!;
    const answer = canvas.getByText('No new build was started.');
    const gaps = [
      [activity[0], prose[0]],
      [prose[0], code[1]],
      [activity[1], prose[1]],
      [prose[1], prose[2]],
      [run, prose[3]],
      [prose[3], answer],
      // An attachment without code already has a margin; do not add prose padding to it.
      ...(code[2] ? [[prose[2], code[2]]] : [[prose[2], run]]),
    ];
    for (const [before, after] of gaps) {
      expect(
        after.getBoundingClientRect().top - before.getBoundingClientRect().bottom,
        `${before.textContent?.slice(0, 40)} → ${after.textContent?.slice(0, 40)}`,
      ).toBeCloseTo(10, 0);
    }
  },
};

export const ProseWithAttachment: Story = {
  ...ProseSpacing,
  args: {
    ...ProseSpacing.args,
    iterations: ProseSpacing.args!.iterations!.map((iteration) =>
      iteration.id === 'prose-with-run' ? { ...iteration, forms: [] } : iteration,
    ),
  },
};

/** Reasoning and its program read as one step, with the same vertical inset. */
export const ThinkingAndCode: Story = {
  args: {
    live: false,
    showCode: true,
    iterations: STORY_THINKING_AND_CODE,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const thinking = canvas.getByText('Checking files').closest('section')!;
    const code = canvasElement.querySelector('[data-execution-code]')!;
    const thought = thinking.getBoundingClientRect();
    const program = code.getBoundingClientRect();
    // Regression: a margin split the step, and CODE padded an already padded control.
    await expect(program.top).toBeCloseTo(thought.bottom, 0);
    await expect(
      code.querySelector('button')!.getBoundingClientRect().height,
    ).toBeGreaterThanOrEqual(28);
    await expect(program.left - thought.left).toBeCloseTo(0, 0);
    await expect(program.right).toBeCloseTo(thought.right, 0);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await expect(code.querySelector('pre')?.textContent).toContain('print(paths)');
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
    await expect(code.getBoundingClientRect().top).toBeCloseTo(thought.bottom, 0);
  },
};

/** Metadata stays close to copy without moving the glyph or joining the controls. */
export const TrailingMetadata: Story = {
  args: ThinkingAndCode.args,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const mouse = matchMedia('(min-width: 40rem) and (pointer: fine)').matches;
    const codeCopy = canvas.getByRole('button', { name: 'Copy code' });
    const activityCopy = canvas.getByRole('button', { name: 'Copy activity' });
    const code = canvasElement.querySelector('[data-execution-code]')!;
    const duration = code.querySelector('.text-code-duration')!;
    const activityToggle = canvas.getByRole('button', { name: 'Expand Activity' });
    const chevron = activityToggle.querySelector('svg:last-child')!;
    const checkSpacing = async () => {
      for (const [metadata, copy] of [
        [duration, codeCopy],
        [chevron, activityCopy],
      ]) {
        const glyph = copy.querySelector('svg')!.getBoundingClientRect();
        await expect(glyph.left - metadata.getBoundingClientRect().right).toBeCloseTo(
          mouse ? 8 : 16,
          0,
        );
        await expect(code.getBoundingClientRect().right - glyph.right).toBeCloseTo(12, 0);
        const box = copy.getBoundingClientRect();
        const minimum = mouse ? 28 : 44;
        await expect(box.height).toBeGreaterThanOrEqual(minimum);
        for (const x of [box.left + 1, box.left + minimum - 1]) {
          await expect(
            copy.contains(copy.ownerDocument.elementFromPoint(x, box.top + box.height / 2)),
          ).toBe(true);
        }
      }
      await expect(
        activityCopy.getBoundingClientRect().left - activityToggle.getBoundingClientRect().right,
      ).toBeCloseTo(8, 0);
    };
    await checkSpacing();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await userEvent.click(activityToggle);
    await checkSpacing();
  },
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

const forkFromAnswer = fn();

async function expectForkAlignment(answer: HTMLElement) {
  const canvas = within(answer);
  const role = canvas.getByText('Vis', { exact: true }).getBoundingClientRect();
  const action = canvas.getByRole('button', { name: 'Fork from here' }).getBoundingClientRect();
  // Regression: the icon-led action must share the role label's vertical center.
  await expect(action.top + action.height / 2).toBeCloseTo(role.top + role.height / 2, 0);
}

async function expectRoleSpacing(canvasElement: HTMLElement) {
  const canvas = within(canvasElement);
  const userRole = canvas.getByText('You', { exact: true });
  const userBody = userRole.nextElementSibling!;
  const userGap = userBody.getBoundingClientRect().top - userRole.getBoundingClientRect().bottom;
  // Regression: every answer starts at the same distance below its role as a request.
  for (const role of canvas.getAllByText('Vis', { exact: true })) {
    const body = role.closest('article')!.children[1];
    await waitFor(() => {
      expect(body.getBoundingClientRect().top - role.getBoundingClientRect().bottom).toBeCloseTo(
        userGap,
        0,
      );
    });
  }
}

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
    const bubble = heading.closest('article')!.querySelector('.border-l-2')!;
    const code = canvasElement.querySelector('[data-execution-code]')!;
    await expect(bubble.getBoundingClientRect().left).toBeCloseTo(
      heading.getBoundingClientRect().left,
      0,
    );
    await expect(code.getBoundingClientRect().left).toBeCloseTo(
      bubble.getBoundingClientRect().left,
      0,
    );
    await expect(getComputedStyle(bubble).paddingLeft).toBe(getComputedStyle(code).paddingLeft);
    const answer = canvas.getByText('Vis', { exact: true }).closest('article')!;
    const fork = within(answer).getByRole('button', { name: 'Fork from here' });
    await expect(
      within(heading.closest('article')!).queryByRole('button', {
        name: 'Fork from here',
      }),
    ).toBeNull();
    await userEvent.hover(answer);
    await expect(fork).toBeVisible();
    await expectForkAlignment(answer);
    await expectRoleSpacing(canvasElement);
    forkFromAnswer.mockClear();
    await userEvent.click(fork);
    await expect(forkFromAnswer).toHaveBeenCalledOnce();
  },
};

/** The answer's fork action reports progress and prevents a second press. */
export const Forking: Story = {
  args: { live: false, iterations: STORY_TURN_ITERATIONS_SETTLED },
  render: (args) => (
    <>
      <UserMessage>{STORY_EXCHANGE_TURN.request ?? ''}</UserMessage>
      <AssistantMessage
        turn={{ ...STORY_EXCHANGE_TURN, iterations: args.iterations }}
        whole={args.whole}
        onFork={() => {}}
        isForking
      />
    </>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const answer = canvas.getByText('Vis', { exact: true }).closest('article')!;
    const fork = within(answer).getByRole('button', { name: 'Fork from here' });
    await userEvent.hover(answer);
    await expect(fork).toBeDisabled();
    await expect(fork).toHaveTextContent('Forking...');
    await expectForkAlignment(answer);
    await expectRoleSpacing(canvasElement);
  },
};

const councilRequest = {
  entry_id: 832,
  thread_id: 831,
  kind: 'coordination' as const,
  content: [
    'Can you confirm which checks cover Council notifications in the terminal and Companion?',
    'Please share the exact test paths and any findings from the previous implementation.',
    'The transcript should show this actual request, not the synthetic instruction used to wake the agent.',
    'Persist the request origin and Council entry kind so reopening a session keeps the same attribution.',
    'Keep long requests collapsed after four rendered lines, with the full message available on demand.',
    'Include any remaining uncertainty in your reply.',
  ].join('\n'),
};

/** Production request rail with durable Council provenance and a four-line preview. */
export const CouncilWake: Story = {
  args: { live: false, iterations: STORY_TURN_ITERATIONS_SETTLED },
  render: (args) => (
    <>
      <UserMessage requestKind="council" council={councilRequest}>
        {councilRequest.content}
      </UserMessage>
      <AssistantMessage
        turn={{ ...STORY_EXCHANGE_TURN, iterations: args.iterations }}
        whole={args.whole}
      />
    </>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(
      canvas.getByText('Council · Coordination · Thread #831', { exact: true }),
    ).toBeVisible();
    await expect(canvas.queryByText('You', { exact: true })).toBeNull();
    const toggle = await canvas.findByRole('button', { name: 'Show full message' });
    const body = canvasElement.querySelector('.line-clamp-4')!;
    const lineHeight = Number.parseFloat(getComputedStyle(body).lineHeight);
    await expect(body.getBoundingClientRect().height).toBeCloseTo(lineHeight * 4, 0);
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-expanded', 'true');
    await expect(body.getBoundingClientRect().height).toBeGreaterThan(lineHeight * 4);
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
  },
};

/** Role spacing does not depend on an answer's content or lifecycle state. */
export const MessageSpacing: Story = {
  render: () => (
    <>
      <UserMessage>{STORY_EXCHANGE_TURN.request ?? ''}</UserMessage>
      {[false, true].map((withFork) => (
        <AssistantMessage
          key={`answer-${withFork}`}
          turn={{ ...STORY_EXCHANGE_TURN, iterations: [] }}
          onFork={withFork ? forkFromAnswer : undefined}
        />
      ))}
      <AssistantMessage
        turn={{ ...STORY_EXCHANGE_TURN, iterations: STORY_THINKING_AND_CODE }}
        onFork={forkFromAnswer}
        whole
      />
      {['running', 'cancelled', 'failed', 'completed'].map((status) => (
        <AssistantMessage
          key={status}
          turn={{ ...STORY_EXCHANGE_TURN, status, content: [], iterations: [] }}
          settled
        />
      ))}
      <AssistantMessage
        turn={{
          ...STORY_EXCHANGE_TURN,
          status: 'running',
          content: [],
          iterations: [],
        }}
        streaming
      />
      <AssistantMessage
        turn={{ ...STORY_EXCHANGE_TURN, content: [], iterations: [] }}
        pending="Loading latest changes"
      />
    </>
  ),
  play: async ({ canvasElement }) => {
    await expectRoleSpacing(canvasElement);
  },
};

/** Retained histories share the same band and operation groups as inline receipts. */
export const GroupedActivityHistories: Story = {
  args: {
    live: false,
    showCode: true,
    iterations: GROUPED_ACTIVITY_HISTORY_IDS.map((id, index) => ({
      id: `history-${index}`,
      position: index + 1,
      forms: [
        {
          source: `print(cat('src/review-${index + 1}-1.clj'))`,
          duration_ms: 100,
          activity: groupedActivityHistoryPage(id),
        },
      ],
    })),
  },
  decorators: [
    (Story) => (
      <ActivityHistoryContext.Provider
        value={{
          load: async (id, after, query) => groupedActivityHistoryPage(id, after, query),
        }}
      >
        <Story />
      </ActivityHistoryContext.Provider>
    ),
  ],
  play: async ({ canvas, canvasElement }) => {
    await expect(canvas.getAllByRole('button', { name: 'Expand code' })).toHaveLength(1);
    const toggle = canvas.getByRole('button', { name: 'Expand Activity' });
    await expect(toggle).toHaveTextContent('7 operations');
    toggle.focus();
    await userEvent.keyboard('{Enter}');
    await expect(canvas.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
    await userEvent.click(canvas.getByRole('button', { name: /Read ×6/ }));
    await expect(canvasElement.querySelectorAll('[data-activity-row]')).toHaveLength(6);
    await userEvent.click(canvas.getByRole('button', { name: 'Show more operations' }));
    await expect(await canvas.findByText('review-3-3.clj')).toBeVisible();
    await expect(canvasElement.querySelectorAll('[data-activity-row]')).toHaveLength(1);
    await userEvent.click(canvas.getByRole('button', { name: 'Show earlier operations' }));
    await canvas.findByRole('button', { name: /Read ×6/ });
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse Activity' }));
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
  },
};

/** Consecutive verification outputs share one result disclosure. */
export const MergedResults: Story = {
  args: {
    live: false,
    showCode: true,
    iterations: [
      {
        id: 'verification',
        position: 1,
        forms: [
          {
            source: "print(await run_tests({'language': 'clojure'}))",
            stdout: 'run_tests: PASS\n98 tests\n0 failures',
            duration_ms: 1200,
          },
          {
            source: "print(await format_code({'language': 'clojure'}))",
            stdout: 'format_code: PASS\n2 files checked',
            duration_ms: 40,
          },
          {
            source: "print(await lint_code({'language': 'clojure'}))",
            stdout: 'lint_code: PASS\n0 warnings',
            duration_ms: 80,
          },
        ],
      },
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await expect(canvas.getAllByRole('button', { name: 'Expand result' })).toHaveLength(1);
    const result = canvas.getByRole('button', { name: 'Expand result' });
    await expect(result).toHaveTextContent('RESULT +7 more');
    await expect(canvas.queryByText('run_tests: PASS', { exact: false })).toBeNull();
    await userEvent.click(result);
    const body = canvasElement.querySelector('[data-code-result]')!;
    await expect(body).toHaveTextContent('run_tests: PASS');
    await expect(body).toHaveTextContent('lint_code: PASS');
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse result' }));
    await expect(body).not.toHaveTextContent('run_tests: PASS');
  },
};

export const CompactGroup: Story = {
  args: { live: true, showCode: true, iterations: STORY_COMPACT_EXECUTIONS },
};

/** Measured durations survive grouping, including calls below millisecond resolution. */
export const GroupDurations: Story = {
  args: {
    live: false,
    showCode: true,
    iterations: [
      {
        position: 1,
        forms: [
          { source: 'read_files()', duration_ms: 120 },
          { source: 'check_files()', duration_ms: 180 },
        ],
      },
      {
        position: 2,
        thinking: 'Checking the result',
        forms: [{ source: 'pass', duration_ms: 0 }],
      },
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText('300ms')).toBeVisible();
    await expect(canvas.getByText('<1ms')).toBeVisible();
    await userEvent.click(canvas.getAllByRole('button', { name: 'Expand code' })[0]);
    await expect(canvas.getByText('300ms')).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
  },
};

export const GroupStages: Story = {
  ...CompactGroup,
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.queryByRole('list', { name: 'Operation groups' })).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(canvas.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
    await expect(canvas.getAllByRole('button', { name: 'Copy code' })).toHaveLength(1);
  },
};

export const HiddenCode: Story = {
  args: { live: true, showCode: false, iterations: STORY_COMPACT_EXECUTIONS },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.queryByRole('list', { name: 'Operation groups' })).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(canvas.queryByRole('button', { name: 'Copy code' })).toBeNull();
    await expect(canvas.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
  },
};

export const Listing: Story = {
  args: { live: false, showCode: true, iterations: STORY_LISTING },
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
    const canvas = within(canvasElement);
    await expect(canvas.queryByRole('button', { name: 'Expand result' })).toBeNull();
    const code = canvasElement.querySelector('[data-execution-code]')!;
    const activity = canvasElement.querySelector('[data-execution-activity]')!;
    const assertHeader = async (button: HTMLElement, label: string) => {
      await expect(button).toHaveTextContent(label);
      const text = button.firstElementChild!.getBoundingClientRect();
      const chevron = button.querySelector('svg')!.getBoundingClientRect();
      await expect(chevron.left - text.right).toBeGreaterThanOrEqual(4);
      await expect(chevron.left - text.right).toBeLessThanOrEqual(8);
      await expect(
        button.getBoundingClientRect().left - code.getBoundingClientRect().left,
      ).toBeCloseTo(12, 0);
    };
    await assertHeader(canvas.getByRole('button', { name: 'Expand code' }), 'CODE +2 more');
    const copy = canvas.getByRole('button', { name: 'Copy code' });
    const duration = within(code as HTMLElement).getByText('57ms');
    await expect(duration.getBoundingClientRect().right).toBeLessThanOrEqual(
      copy.getBoundingClientRect().left,
    );
    await expect(copy.getBoundingClientRect().right).toBeLessThanOrEqual(
      code.getBoundingClientRect().right,
    );
    await expect(copy.querySelector('svg')!.getBoundingClientRect().right).toBeLessThan(
      code.getBoundingClientRect().right,
    );
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    const band = canvasElement.querySelector('[data-execution-code]')!;
    const result = canvasElement.querySelector('[data-code-result]')!;
    const codeBody = code.querySelector('[data-code-body]')!;
    const labelSize = getComputedStyle(document.documentElement)
      .getPropertyValue('--text-ui')
      .trim();
    for (const name of ['CODE', 'RESULT', 'ACTIVITY']) {
      const label = canvas.getByText(name, { exact: true, selector: 'span' });
      await expect(getComputedStyle(label).fontSize).toBe(labelSize);
      await expect(getComputedStyle(label).fontWeight).toBe('600');
      await expect(getComputedStyle(label).color).toBe(getComputedStyle(document.body).color);
    }
    // Regression: CODE and RESULT stacked body padding beneath the disclosure.
    // Like ACTIVITY, the first content row begins at the end of its header.
    const headerBottom = (name: string) =>
      canvas.getByRole('button', { name }).getBoundingClientRect().bottom;
    const firstCodeLine = codeBody.querySelector('pre code > div')!;
    await expect(firstCodeLine.getBoundingClientRect().top - headerBottom('Collapse code')).toBe(0);
    for (const surface of [code, result, activity]) {
      await expect(getComputedStyle(surface).borderLeftWidth).toBe('0px');
    }
    await expect(result.getBoundingClientRect().top).toBeGreaterThan(
      code.getBoundingClientRect().top,
    );
    await expect(activity.getBoundingClientRect().top).toBeGreaterThan(
      result.getBoundingClientRect().top,
    );
    await assertHeader(canvas.getByRole('button', { name: 'Expand result' }), 'RESULT +1 more');
    await expect(band.textContent).not.toContain('Listed 5 entries.');
    canvas.getByRole('button', { name: 'Expand result' }).focus();
    await userEvent.keyboard('{Enter}');
    await expect(band.textContent).toContain('Listed 5 entries.');
    const firstResultLine = result.querySelector('pre code > div')!;
    await expect(
      firstResultLine.getBoundingClientRect().top - headerBottom('Collapse result'),
    ).toBe(0);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    const activityBody = canvas.getByRole('list', { name: 'Operation groups' });
    await expect(activityBody.getBoundingClientRect().top - headerBottom('Collapse Activity')).toBe(
      0,
    );
    await expect(band.querySelector('summary')).toBeNull();
    await expect(
      canvas.getByRole('button', { name: 'Collapse code' }).querySelector('svg'),
    ).not.toBeNull();
    await expect(
      canvas.getByRole('button', { name: 'Collapse result' }).querySelector('svg'),
    ).not.toBeNull();
    await expect(band.textContent).toContain('57ms');
    await expect(canvas.getByText('42ms')).toBeTruthy();
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

/** The shipped composition, reviewed with deterministic operation snapshots. */
export const JoinedActivity: Story = {
  args: {
    live: true,
    showCode: true,
    iterations: STORY_JOINED_ACTIVITY.running,
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const code = canvasElement.querySelector('[data-execution-code]')!;
    const activity = canvasElement.querySelector('[data-execution-activity]')!;
    const copyIcon = code.querySelector('button[aria-label="Copy code"] svg')!;
    const activityCopyIcon = activity.querySelector('button[aria-label="Copy activity"] svg')!;
    await expect(copyIcon.getBoundingClientRect().right).toBeCloseTo(
      activityCopyIcon.getBoundingClientRect().right,
      0,
    );
    const thought = canvas
      .getByText('Checking the Activity layout and grouping.')
      .closest('section')!;
    await expect(code.getBoundingClientRect().top).toBeCloseTo(
      thought.getBoundingClientRect().bottom,
      0,
    );
    await expect(activity.getBoundingClientRect().top).toBeCloseTo(
      code.getBoundingClientRect().bottom,
      0,
    );
    await expect(activity.getBoundingClientRect().left).toBeCloseTo(
      code.getBoundingClientRect().left,
      0,
    );
    await expect(activity.getBoundingClientRect().right).toBeCloseTo(
      code.getBoundingClientRect().right,
      0,
    );
    // Execution bands keep a consistent text gutter without a left border.
    const trace = thought.parentElement!;
    const edge = trace.parentElement!.getBoundingClientRect().left;
    const textEdge = (element: Element) => {
      const walker = document.createTreeWalker(element, NodeFilter.SHOW_TEXT);
      let node = walker.nextNode();
      while (node && !node.textContent?.trim()) node = walker.nextNode();
      const range = document.createRange();
      range.selectNodeContents(node!);
      return range.getBoundingClientRect().left;
    };
    await expect(textEdge(thought)).toBeCloseTo(edge + 12, 0);
    const executionEdge = textEdge(code);
    await expect(code.getBoundingClientRect().left - edge).toBeCloseTo(0, 0);
    await expect(getComputedStyle(code).borderLeftWidth).toBe('0px');
    await expect(getComputedStyle(activity).borderLeftWidth).toBe('0px');
    await expect(executionEdge - edge).toBeCloseTo(12, 0);
    const band = canvas.getByRole('button', { name: 'Expand Activity' });
    await expect(band).toHaveTextContent('ACTIVITY');
    await expect(band).toHaveTextContent('13 operations');
    await expect(canvas.queryByRole('button', { name: /Read ×8/ })).toBeNull();
    await expect(textEdge(band)).toBeCloseTo(executionEdge, 0);
    await userEvent.click(band);
    for (const label of [/Read ×8/, /Patch ×3/]) {
      await expect(textEdge(canvas.getByRole('button', { name: label }))).toBeCloseTo(
        executionEdge,
        0,
      );
    }
    const reads = canvas.getByRole('button', { name: /Read ×8/ });
    await expect(reads).toHaveTextContent('6 files');
    await expect(canvas.getByRole('button', { name: /Patch ×3/ })).toHaveTextContent('+42 −11');
    reads.focus();
    await userEvent.keyboard('{Enter}');
    await expect(reads).toHaveAttribute('aria-expanded', 'true');
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await expect(code.querySelector('pre')).not.toBeNull();
    await expect(textEdge(code.querySelector('pre')!)).toBeCloseTo(executionEdge, 0);
    const codeSize = getComputedStyle(code.querySelector('pre')!).fontSize;
    await expect(getComputedStyle(canvas.getByText('6 files')).fontSize).toBe(codeSize);
    const mouse = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    await expect(getComputedStyle(reads).fontSize).toBe(mouse ? '10px' : '11px');
    // Names use the transcript body token; counts remain secondary metadata.
    const labelSize = getComputedStyle(document.documentElement)
      .getPropertyValue('--text-ui')
      .trim();
    const countSize = getComputedStyle(canvas.getByText(/13 operations/)).fontSize;
    await expect(Number.parseFloat(countSize)).toBeLessThanOrEqual(Number.parseFloat(labelSize));
    for (const name of ['CODE', 'ACTIVITY']) {
      const label = canvas.getByText(name);
      await expect(getComputedStyle(label).fontSize).toBe(labelSize);
      await expect(getComputedStyle(label).fontWeight).toBe('600');
      await expect(getComputedStyle(label).color).toBe(getComputedStyle(document.body).color);
    }
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(reads).toHaveAttribute('aria-expanded', 'true');
    // Leave the design story in its compact initial state for visual review.
    await userEvent.click(reads);
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
  },
};
export const JoinedActivitySettled: Story = {
  args: {
    live: false,
    showCode: true,
    iterations: STORY_JOINED_ACTIVITY.succeeded,
  },
};
export const JoinedActivityFailed: Story = {
  args: {
    live: false,
    showCode: true,
    iterations: STORY_JOINED_ACTIVITY.failed,
  },
};
export const JoinedActivityWithoutCode: Story = {
  args: {
    live: true,
    showCode: false,
    iterations: STORY_JOINED_ACTIVITY.running,
  },
};

/** One interactive review sheet using the same production trace in three fixture states. */
export const JoinedActivityReview: Story = {
  render: () => (
    <main className="grid min-w-0 gap-8 pb-8">
      <header>
        <h1 className="text-head font-semibold text-vis-message">Activity · joined execution</h1>
        <p className="mt-2 text-ui text-dialog-hint">
          Interactive design · fixture data, no gateway connection.
        </p>
      </header>
      {(['running', 'succeeded', 'failed'] as const).map((state) => (
        <section key={state} aria-label={state}>
          <h2 className="mb-2 text-title font-semibold text-vis-message">
            {state === 'running'
              ? 'Working'
              : state === 'succeeded'
                ? 'Finished'
                : 'Needs attention'}
          </h2>
          <IterationTrace
            whole
            showCode
            live={state === 'running'}
            iterations={STORY_JOINED_ACTIVITY[state]}
          />
        </section>
      ))}
    </main>
  ),
};

/** Prose and reasoning share their text edge, including the final answer. */
export const ProseAlignment: Story = {
  render: () => (
    <AssistantMessage
      whole
      turn={{
        ...STORY_EXCHANGE_TURN,
        iterations: STORY_JOINED_ACTIVITY.succeeded.map((iteration) => ({
          ...iteration,
          assistant_prose: 'I checked the files before making these changes.',
        })),
        content: [
          {
            id: 'alignment-answer',
            type: 'prose',
            markdown: 'The changes are ready for review.\n\n- Read the files\n- Check the results',
          },
        ],
      }}
    />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const thought = canvas.getByText('Checking the Activity layout and grouping.');
    const prose = canvas.getByText('I checked the files before making these changes.');
    const answer = canvas.getByText('The changes are ready for review.');
    // Match the real answer, not an assumed size from the typography scale.
    for (const name of ['CODE', 'ACTIVITY']) {
      await expect(getComputedStyle(canvas.getByText(name)).fontSize).toBe(
        getComputedStyle(answer).fontSize,
      );
    }
    for (const paragraph of [prose, answer]) {
      await expect(paragraph.getBoundingClientRect().left).toBeCloseTo(
        thought.closest('section')!.getBoundingClientRect().left,
        0,
      );
      await expect(paragraph.getBoundingClientRect().right).toBeCloseTo(
        // Prose aligns to both outer edges, not the inset reasoning text.
        thought.closest('section')!.getBoundingClientRect().right,
        0,
      );
    }
    // Regression: narration touched Thinking but kept a gap before Code.
    const thinkingBand = thought.closest('section')!.getBoundingClientRect();
    const codeBand = canvasElement.querySelector('[data-execution-code]')!.getBoundingClientRect();
    const paragraph = prose.getBoundingClientRect();
    const above = paragraph.top - thinkingBand.bottom;
    const below = codeBand.top - paragraph.bottom;
    await expect(above).toBeGreaterThanOrEqual(8);
    await expect(above).toBeCloseTo(below, 0);
    await expect(canvasElement.querySelector('[data-step-node]')).toBeNull();
  },
};

/** The session API supplies identity; no client-local rename is needed. */
export const CustomAgentName: Story = {
  render: () => (
    <AssistantMessage
      agentName="Ada"
      turn={{
        turn_id: 'named-agent',
        request: 'Inspect the configuration',
        status: 'running',
        iterations: STORY_TURN_ITERATIONS_SETTLED,
      }}
      settled
    />
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(canvas.getByText('Ada', { exact: true })).toBeVisible();
    await expect(canvas.queryByText('Vis', { exact: true })).toBeNull();
    await expect(canvas.getByRole('status')).toHaveTextContent('Ada is');
  },
};

/** A monochrome review must preserve symbols, disclosure and a still page. */
const monochromePlay: Story['play'] = async ({ canvas, canvasElement }) => {
  const trace = canvas.getAllByRole('button', { name: 'Expand Activity' })[0];
  await userEvent.click(trace);
  await expect(canvas.getByRole('button', { name: 'Collapse Activity' })).toHaveAttribute(
    'aria-expanded',
    'true',
  );
  await expect(canvas.getByRole('list', { name: 'Operation groups' })).toBeVisible();
  const icons = canvasElement.querySelectorAll('[data-execution-activity] svg');
  await expect(icons.length).toBeGreaterThan(0);
  for (const icon of icons) {
    await expect(getComputedStyle(icon).fill).toBe('none');
  }
  await expect(canvasElement.getAnimations({ subtree: true })).toHaveLength(0);
};

export const Paper: Story = { ...Exchange, globals: { theme: 'paper' }, play: monochromePlay };
export const HighContrastDark: Story = {
  ...Exchange,
  globals: { theme: 'high-contrast-dark' },
  play: monochromePlay,
};

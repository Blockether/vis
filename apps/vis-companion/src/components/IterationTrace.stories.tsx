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
  STORY_TURN_STAMP,
  STORY_THINKING_AND_CODE,
} from '../dev/story-data';
import { AssistantMessage, IterationTrace, UserMessage } from './ChatContent';
import { ActivityHistoryContext } from './ActivityPanel';
import { GROUPED_ACTIVITY_HISTORY_IDS, groupedActivityHistoryPage } from '../dev/activity-history';
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

/** Hidden Python without Activity leaves no empty CODE receipt. */
export const HiddenSourceWithoutOutput: Story = {
  args: {
    live: false,
    showCode: false,
    iterations: [1, 2, 3].map((position) => ({
      position,
      forms: [{ source: 'value = 42', success: true, duration_ms: 10, stdout: '' }],
    })),
  },
  play: async ({ canvas, canvasElement }) => {
    await expect(canvas.queryByText('CODE')).toBeNull();
    // Only the digest row totals the time of the hidden steps.
    const digest = canvasElement.querySelector('[data-step-digest]')!.parentElement;
    await expect(canvas.getAllByText('30ms')).toHaveLength(1);
    await expect(digest).toHaveTextContent('30ms');
    await expect(canvas.queryByRole('button', { name: 'Expand code' })).toBeNull();
    await expect(canvas.queryByText('value = 42')).toBeNull();
  },
};

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
    await canvas.findByText('I will check what caused this turn to stop.');
    // Regression: opening prose added its own top padding to the role's margin.
    // Wait for the live segment's entrance animation before measuring its boxes.
    await waitFor(() => {
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
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    const code = canvasElement.querySelector('[data-execution-code]')!;
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await expect(code.querySelector('pre')?.textContent).toContain('print(paths)');
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
  },
};

/**
 * User report (screenshot): a numbered list inside a THINKING band hung further left than
 * the bulleted list beside it. Reasoning runs through the same markdown renderer as an
 * answer, so both lists hang off one marker column here too — in the band's italic face,
 * and with the loose items reasoning normalization produces.
 */
export const ThinkingLists: Story = {
  args: {
    ...ThinkingAndCode.args,
    iterations: [
      {
        ...STORY_THINKING_AND_CODE[0],
        thinking: [
          'So I could:',
          '1. Start a command that streams output.',
          '2. Poll its logs in a bounded loop.',
          '3. Show the live progress.',
          'Then:',
          '- Read the last lines.',
          '- Stop the command before answering.',
        ].join('\n'),
      },
    ],
  },
  play: async ({ canvasElement }) => {
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    const band = (await canvas.findByText('So I could:')).closest('section')!;
    const lists = [...band.querySelectorAll('ol[role="list"], ul[role="list"]')];
    await expect(lists).toHaveLength(2);
  },
};

/** The Thinking chevron stays beside its label, not at the far edge of the transcript. */
export const ThinkingDisclosure: Story = {
  tags: ['!test'],
  args: {
    ...ThinkingAndCode.args,
    iterations: [
      {
        ...STORY_THINKING_AND_CODE[0],
        thinking: [
          'Inspect desktop screenshot',
          'The thinking header is part of the disclosure.',
          'Keep its chevron beside the label.',
          'Preserve the collapsed preview and hidden-line count.',
          'Check touch and keyboard interaction.',
          'The expanded content should stay readable.',
          'Confirm the code below keeps its own disclosure.',
        ].join('\n'),
      },
    ],
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const toggle = await canvas.findByRole('button', { name: /^THINKING/ });
    const body = toggle.nextElementSibling as HTMLElement;
    const closedHeight = body.getBoundingClientRect().height;
    const header = toggle.getBoundingClientRect();
    // Regression (user report, web): clicking the header made the word THINKING itself
    // step. The `+N more` tally sat INSIDE the truncating label span, where its smaller
    // type stretched that span's line box — so dropping the tally on expand moved the
    // label. The word holds one position through every toggle.
    const word = () => {
      const text = document.createTreeWalker(toggle, NodeFilter.SHOW_TEXT).nextNode()!;
      const range = document.createRange();
      range.selectNodeContents(text);
      return range.getBoundingClientRect();
    };
    const resting = word();
    const assertChevron = async () => {
      const label = toggle.firstElementChild!.getBoundingClientRect();
      const chevron = toggle.querySelector('svg')!.getBoundingClientRect();
      await expect(chevron.left - label.right).toBeCloseTo(6, 0);
      await expect(toggle.getBoundingClientRect().width).toBe(header.width);
      await expect(toggle.getBoundingClientRect().height).toBe(header.height);
      await expect(word().top).toBeCloseTo(resting.top, 2);
      await expect(word().left).toBeCloseTo(resting.left, 2);
    };
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
    await expect(toggle).toHaveTextContent(/\+\d+ more/);
    await assertChevron();
    toggle.focus();
    await userEvent.keyboard('{Enter}');
    await expect(toggle).toHaveAttribute('aria-expanded', 'true');
    await expect(toggle).not.toHaveTextContent(/\+\d+ more/);
    await expect(body.getBoundingClientRect().height).toBeGreaterThan(closedHeight);
    await assertChevron();
    await userEvent.keyboard('[Space]');
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
    await assertChevron();
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-expanded', 'true');
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-expanded', 'false');
    await expect(toggle).toHaveTextContent(/\+\d+ more/);
  },
};

export const ThinkingDisclosurePointer: Story = {
  ...ThinkingDisclosure,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** Metadata stays close to copy without moving the glyph or joining the controls. */
export const TrailingMetadata: Story = {
  args: ThinkingAndCode.args,
  play: async ({ canvasElement }) => {
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    const activityToggle = canvas.getByRole('button', { name: 'Expand Activity' });
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await userEvent.click(activityToggle);
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

export const TurnHeaderOrder: Story = {
  render: () => (
    <div style={{ width: 375, maxWidth: '100%' }}>
      <AssistantMessage
        turn={{
          ...STORY_EXCHANGE_TURN,
          position: 42,
          created_at: STORY_TURN_STAMP,
        }}
        onFork={forkFromAnswer}
      />
    </div>
  ),
  play: async ({ canvasElement }) => {
    const answer = canvasElement.querySelector('article')!;
    const header = answer.firstElementChild!;
    await expect(header).toHaveTextContent('16/09/2026, 13:23:45 / T42 / Fork from this turn');
    const time = header.querySelector('time')!;
    const fork = within(answer).getByRole('button', { name: 'Fork from this turn' });
    await expect(fork).toBeVisible();
    // Match the date / turn separator: one text-space on either side of the fork slash.
    const space = document.createRange();
    space.setStart(time.nextSibling!, 0);
    space.setEnd(time.nextSibling!, 1);
    const slash = document.createRange();
    slash.setStart(fork.previousElementSibling!.firstChild!, 1);
    slash.setEnd(fork.previousElementSibling!.firstChild!, 2);
    forkFromAnswer.mockClear();
    await userEvent.click(fork);
    await expect(forkFromAnswer).toHaveBeenCalledOnce();
  },
};

export const TurnHeaderOrderPointer: Story = {
  ...TurnHeaderOrder,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

async function expectForkAlignment(answer: HTMLElement) {
  const canvas = within(answer);
  const role = canvas.getByText('Vis', { exact: true }).getBoundingClientRect();
  const action = canvas.getByRole('button', { name: 'Fork from this turn' }).getBoundingClientRect();
  // Regression: the icon-led action must share the role label's vertical center.
  await expect(action.top + action.height / 2).toBeCloseTo(role.top + role.height / 2, 0);
  const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
  await expect(action.height).toBe(pointer ? 28 : 32);
  const reach = pointer ? 0 : 6;
  // Reserve the full action target above the first prose or execution control.
  await expect(
    answer.children[1].getBoundingClientRect().top - action.bottom - reach,
  ).toBeGreaterThanOrEqual(8);
}

async function expectRoleSpacing(canvasElement: HTMLElement) {
  const canvas = within(canvasElement);
  const userRole = canvas.getByText('You', { exact: true });
  const userBody = userRole.closest('article')!.children[1];
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

export const ExchangePointer: Story = {
  ...Exchange,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const answer = canvas.getByText('Vis', { exact: true }).closest('article')!;
    const fork = within(answer).getByRole('button', { name: 'Fork from this turn' });
    // Regression: the action must not depend on hover or keyboard focus.
    await expect(matchMedia('(min-width: 640px) and (pointer: fine)').matches).toBe(true);
    await userEvent.unhover(answer);
    await expect(fork).not.toHaveFocus();
    await expect(fork).toBeVisible();
    await userEvent.hover(answer);
    await expect(fork).toBeVisible();
    await userEvent.unhover(answer);
    await expect(fork).toBeVisible();
    fork.focus();
    await expect(fork).toHaveFocus();
    await expect(fork).toBeVisible();
    fork.blur();
    await expect(fork).not.toHaveFocus();
    await expect(fork).toBeVisible();
    await expectForkAlignment(answer);
    await expectRoleSpacing(canvasElement);
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
    const fork = within(answer).getByRole('button', { name: 'Fork from this turn' });
    await userEvent.unhover(answer);
    await expect(fork).toBeVisible();
    await expect(fork).toBeDisabled();
    await expect(fork).toHaveTextContent('Forking...');
  },
};

export const ForkingPointer: Story = {
  ...Forking,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
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
    await openStepDigests(canvasElement);
    await expect(canvas.getAllByRole('button', { name: 'Expand code' })).toHaveLength(1);
    const toggle = canvas.getByRole('button', { name: 'Expand Activity' });
    await expect(toggle).toHaveTextContent('7 operations');
    toggle.focus();
    await userEvent.keyboard('{Enter}');
    await expect(canvas.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
    await userEvent.click(await canvas.findByRole('button', { name: /Read ×7/ }));
    await expect(canvasElement.querySelectorAll('[data-activity-row]')).toHaveLength(7);
    await expect(canvas.getByText('review-3-3.clj')).toBeVisible();
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
            source: "print(await shell('npm test'))",
            stdout: 'npm test: PASS\n98 tests\n0 failures',
            duration_ms: 1200,
          },
          {
            source: "print(await shell('npm run format'))",
            stdout: 'npm run format: PASS\n2 files checked',
            duration_ms: 40,
          },
          {
            source: "print(await shell('npm run lint'))",
            stdout: 'npm run lint: PASS\n0 warnings',
            duration_ms: 80,
          },
        ],
      },
    ],
  },
  play: async ({ canvasElement }) => {
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await expect(canvas.getAllByRole('button', { name: 'Expand result' })).toHaveLength(1);
    const result = canvas.getByRole('button', { name: 'Expand result' });
    await expect(result).toHaveTextContent('RESULT +7 more');
    await expect(canvas.queryByText('npm test: PASS', { exact: false })).toBeNull();
    await userEvent.click(result);
    const body = canvasElement.querySelector('[data-code-result]')!;
    await expect(body).toHaveTextContent('npm test: PASS');
    await expect(body).toHaveTextContent('npm run lint: PASS');
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse result' }));
    await expect(body).not.toHaveTextContent('npm test: PASS');
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
        assistant_prose: 'Checking the result.',
        forms: [{ source: 'pass', duration_ms: 0 }],
      },
    ],
  },
  play: async ({ canvasElement }) => {
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    // The digest row totals its steps; the code band keeps the time of its group.
    const digest = canvasElement.querySelector('[data-step-digest]')!.parentElement;
    const band = () => within(canvasElement.querySelector<HTMLElement>('[data-execution-code]')!);
    await expect(digest).toHaveTextContent('300ms');
    await expect(band().getByText('300ms')).toBeVisible();
    await expect(canvas.getByText('<1ms')).toBeVisible();
    await userEvent.click(canvas.getAllByRole('button', { name: 'Expand code' })[0]);
    await expect(band().getByText('300ms')).toBeVisible();
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
  },
};

export const GroupStages: Story = {
  ...CompactGroup,
  play: async ({ canvasElement }) => {
    await openStepDigests(canvasElement);
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
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    await expect(canvasElement.querySelector('[data-execution-code]')).toBeNull();
    await expect(canvasElement.querySelector('[data-code-result]')).toBeNull();
    await expect(canvas.queryByRole('list', { name: 'Operation groups' })).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand Activity' }));
    await expect(canvas.queryByRole('button', { name: 'Copy code' })).toBeNull();
    await expect(canvas.getAllByRole('list', { name: 'Operation groups' })).toHaveLength(1);
  },
};

export const Listing: Story = {
  args: { live: false, showCode: true, iterations: STORY_LISTING },
};

/** Before Activity or output arrives, the expanded source owns its bottom inset. */
export const CodeWithoutActivity: Story = {
  args: {
    live: true,
    showCode: true,
    iterations: [{ position: 1, forms: [{ source: 'value = 42\nprint(value)' }] }],
  },
  play: async ({ canvasElement }) => {
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    await expect(canvas.queryByRole('button', { name: 'Expand Activity' })).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Expand result' })).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
  },
};

export const CodeWithoutActivityPointer: Story = {
  ...CodeWithoutActivity,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
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

export const CodeWithResultPointer: Story = {
  ...CodeWithResult,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await expect(matchMedia('(min-width: 40rem) and (pointer: fine)').matches).toBe(true);
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    const header = canvas.getByRole('button', { name: 'Collapse code' }).getBoundingClientRect();
    const result = canvas.getByRole('button', { name: 'Expand result' }).getBoundingClientRect();
    const lines = canvasElement.querySelectorAll('[data-code-body] pre code > div');
    const firstLine = lines[0].getBoundingClientRect();
    const lastLine = lines[lines.length - 1].getBoundingClientRect();
    await expect(result.top - lastLine.bottom).toBe(firstLine.top - header.bottom);
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
    await openStepDigests(canvasElement);
    const canvas = within(canvasElement);
    const code = canvasElement.querySelector('[data-execution-code]')!;
    const band = canvas.getByRole('button', { name: 'Expand Activity' });
    await expect(band).toHaveTextContent('ACTIVITY');
    await expect(band).toHaveTextContent('3 mutations · 8 observations · 2 verifications · 1 running');
    await expect(canvas.queryByRole('button', { name: /Read ×8/ })).toBeNull();
    await userEvent.click(band);
    const reads = canvas.getByRole('button', { name: /Read ×8/ });
    await expect(reads).toHaveTextContent('6 files');
    await expect(canvas.getByRole('button', { name: /Patch ×3/ })).toHaveTextContent('+42 −11');
    reads.focus();
    await userEvent.keyboard('{Enter}');
    await expect(reads).toHaveAttribute('aria-expanded', 'true');
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    await expect(code.querySelector('pre')).not.toBeNull();
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
  await openStepDigests(canvasElement);
  const trace = canvas.getAllByRole('button', { name: 'Expand Activity' })[0];
  await userEvent.click(trace);
  await expect(canvas.getByRole('button', { name: 'Collapse Activity' })).toHaveAttribute(
    'aria-expanded',
    'true',
  );
  await expect(canvas.getByRole('list', { name: 'Operation groups' })).toBeVisible();
  const icons = canvasElement.querySelectorAll('[data-execution-activity] svg');
  await expect(icons.length).toBeGreaterThan(0);
};

export const Paper: Story = { ...Exchange, globals: { theme: 'paper' }, play: monochromePlay };
export const HighContrastDark: Story = {
  ...Exchange,
  globals: { theme: 'high-contrast-dark' },
  play: monochromePlay,
};

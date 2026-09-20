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
  play: async ({ canvas }) => {
    await expect(canvas.queryByText('CODE')).toBeNull();
    await expect(canvas.queryByText('30ms')).toBeNull();
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
    const canvas = within(canvasElement);
    await userEvent.click(await canvas.findByRole('button', { name: /^THINKING/ }));
    const intro = canvas.getByText('So I could:');
    const band = intro.closest('section')!;
    const lists = [...band.querySelectorAll('ol[role="list"], ul[role="list"]')];
    await expect(lists).toHaveLength(2);

    // One `ch` in the band's own italic face: the unit the marker columns are written in.
    const probe = document.createElement('span');
    probe.textContent = '0'.repeat(100);
    probe.style.cssText = 'position:absolute;visibility:hidden;white-space:pre';
    lists[0].appendChild(probe);
    const ch = probe.getBoundingClientRect().width / 100;
    probe.remove();

    // A marker sits on the left edge of its item's padding box; the list box itself keeps
    // the reasoning text edge.
    const markerEdge = (list: Element) =>
      list.getBoundingClientRect().left + parseFloat(getComputedStyle(list).paddingLeft);
    await expect(Math.abs(markerEdge(lists[0]) - markerEdge(lists[1]))).toBeLessThan(0.5);
    await expect(markerEdge(lists[0]) - intro.getBoundingClientRect().left).toBeCloseTo(2 * ch, 0);

    for (const list of lists) {
      const item = list.querySelector('li')!;
      await expect(getComputedStyle(item).listStyleType).toBe('none');
      await expect(getComputedStyle(item, '::before').position).toBe('absolute');
      // Reasoning normalization spaces the items out, so each one carries a paragraph:
      // the marker still shares that paragraph's first line.
      await expect(item.querySelector('p')!.getBoundingClientRect().top).toBeCloseTo(
        item.getBoundingClientRect().top,
        0,
      );
    }
  },
};

/** The Thinking chevron stays beside its label, not at the far edge of the transcript. */
export const ThinkingDisclosure: Story = {
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
  globals: { viewport: { value: 'desktop', isRotated: false } },
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
    const activitySummary = within(activityToggle).getByText(/operations?/);
    const checkSpacing = async () => {
      const durationStyle = getComputedStyle(duration);
      const summaryStyle = getComputedStyle(activitySummary);
      for (const property of [
        'fontFamily',
        'fontSize',
        'fontWeight',
        'fontStyle',
        'lineHeight',
        'letterSpacing',
        'fontVariantNumeric',
      ] as const) {
        await expect(summaryStyle[property], property).toBe(durationStyle[property]);
      }
      for (const [metadata, copy] of [
        [duration, codeCopy],
        [activitySummary, activityCopy],
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
    await document.fonts.ready;
    const answer = canvasElement.querySelector('article')!;
    const header = answer.firstElementChild!;
    await expect(header).toHaveTextContent('16/09/2026, 13:23:45 / T42 / Fork from this turn');
    const time = header.querySelector('time')!;
    const stamp = time.parentElement!.getBoundingClientRect();
    const fork = within(answer).getByRole('button', { name: 'Fork from this turn' });
    await expect(fork).toBeVisible();
    const action = fork.getBoundingClientRect();
    await expect(action.top >= stamp.bottom || action.left >= stamp.right).toBe(true);
    await expect(action.right).toBeLessThanOrEqual(header.getBoundingClientRect().right);
    // Match the date / turn separator: one text-space on either side of the fork slash.
    const space = document.createRange();
    space.setStart(time.nextSibling!, 0);
    space.setEnd(time.nextSibling!, 1);
    const gap = space.getBoundingClientRect().width;
    const slash = document.createRange();
    slash.setStart(fork.previousElementSibling!.firstChild!, 1);
    slash.setEnd(fork.previousElementSibling!.firstChild!, 2);
    const separator = slash.getBoundingClientRect();
    const icon = fork.querySelector('svg')!.getBoundingClientRect();
    await expect(gap).toBeGreaterThan(0);
    await expect(separator.left - stamp.right).toBeCloseTo(gap, 0);
    await expect(icon.left - separator.right).toBeCloseTo(gap, 0);
    await expect(action.top + action.height / 2).toBeCloseTo(stamp.top + stamp.height / 2, 0);
    forkFromAnswer.mockClear();
    await userEvent.click(fork);
    await expect(forkFromAnswer).toHaveBeenCalledOnce();
  },
};

export const TurnHeaderOrderPointer: Story = {
  ...TurnHeaderOrder,
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
    const fork = within(answer).getByRole('button', { name: 'Fork from this turn' });
    await expect(
      within(heading.closest('article')!).queryByRole('button', {
        name: 'Fork from this turn',
      }),
    ).toBeNull();
    await expect(fork).toBeVisible();
    await expectForkAlignment(answer);
    await expectRoleSpacing(canvasElement);
    forkFromAnswer.mockClear();
    await userEvent.click(fork);
    await expect(forkFromAnswer).toHaveBeenCalledOnce();
  },
};

export const ExchangePointer: Story = {
  ...Exchange,
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
    await expectForkAlignment(answer);
    await expectRoleSpacing(canvasElement);
  },
};

export const ForkingPointer: Story = {
  ...Forking,
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
    const canvas = within(canvasElement);
    const code = canvasElement.querySelector('[data-execution-code]')!;
    const collapsedHeight = code.getBoundingClientRect().height;
    await expect(canvas.queryByRole('button', { name: 'Expand Activity' })).toBeNull();
    await expect(canvas.queryByRole('button', { name: 'Expand result' })).toBeNull();
    await userEvent.click(canvas.getByRole('button', { name: 'Expand code' }));
    const header = canvas.getByRole('button', { name: 'Collapse code' }).getBoundingClientRect();
    const lines = code.querySelectorAll('[data-code-body] pre code > div');
    const firstLine = lines[0].getBoundingClientRect();
    const lastLine = lines[lines.length - 1].getBoundingClientRect();
    await expect(firstLine.top - header.bottom).toBe(0);
    // Regression: without a following header, the last source line touched the band edge.
    await expect(code.getBoundingClientRect().bottom - lastLine.bottom).toBe(8);
    await userEvent.click(canvas.getByRole('button', { name: 'Collapse code' }));
    await expect(code.getBoundingClientRect().height).toBe(collapsedHeight);
  },
};

export const CodeWithoutActivityPointer: Story = {
  ...CodeWithoutActivity,
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
    const canvas = within(canvasElement);
    await expect(canvas.queryByRole('button', { name: 'Expand result' })).toBeNull();
    const code = canvasElement.querySelector('[data-execution-code]')!;
    const activity = canvasElement.querySelector('[data-execution-activity]')!;
    const assertHeader = async (button: HTMLElement, label: string) => {
      await expect(button).toHaveTextContent(label);
      const text = button.firstElementChild!.getBoundingClientRect();
      const chevron = button.querySelector('svg')!.getBoundingClientRect();
      // The path turns inside a fixed viewport; rotating the SVG enlarged its box mid-turn.
      await expect(getComputedStyle(button.querySelector('svg')!).rotate).toBe('none');
      await expect(chevron.left - text.right).toBeGreaterThanOrEqual(4);
      await expect(chevron.left - text.right).toBeLessThanOrEqual(8);
      await expect(
        button.getBoundingClientRect().left - code.getBoundingClientRect().left,
      ).toBeCloseTo(12, 0);
      return [button, button.firstElementChild!, button.querySelector('svg')!].map((element) => {
        const { x, y, width, height } = element.getBoundingClientRect();
        return { x, y, width, height };
      });
    };
    const closedCodeHeader = await assertHeader(
      canvas.getByRole('button', { name: 'Expand code' }),
      'CODE +2 more',
    );
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
    // Regression: removing the tally shifted the heading and moved its chevron.
    await expect(
      await assertHeader(canvas.getByRole('button', { name: 'Collapse code' }), 'CODE'),
    ).toEqual(closedCodeHeader);
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
    const codeTopGap = firstCodeLine.getBoundingClientRect().top - headerBottom('Collapse code');
    await expect(codeTopGap).toBe(0);
    // Regression: stacked code/result padding made the bottom gap larger than the top.
    const lastCodeLine = codeBody.querySelector('pre code > div:last-child')!;
    const resultHeader = canvas
      .getByRole('button', { name: 'Expand result' })
      .getBoundingClientRect();
    const codeBottomGap = resultHeader.top - lastCodeLine.getBoundingClientRect().bottom;
    await expect(codeBottomGap).toBe(codeTopGap);
    for (const surface of [code, result, activity]) {
      await expect(getComputedStyle(surface).borderLeftWidth).toBe('0px');
    }
    await expect(result.getBoundingClientRect().top).toBeGreaterThan(
      code.getBoundingClientRect().top,
    );
    await expect(activity.getBoundingClientRect().top).toBeGreaterThan(
      result.getBoundingClientRect().top,
    );
    const closedResultHeader = await assertHeader(
      canvas.getByRole('button', { name: 'Expand result' }),
      'RESULT +1 more',
    );
    await expect(band.textContent).not.toContain('Listed 5 entries.');
    canvas.getByRole('button', { name: 'Expand result' }).focus();
    await userEvent.keyboard('{Enter}');
    await expect(band.textContent).toContain('Listed 5 entries.');
    await expect(
      await assertHeader(canvas.getByRole('button', { name: 'Collapse result' }), 'RESULT'),
    ).toEqual(closedResultHeader);
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
    await expect(
      await assertHeader(canvas.getByRole('button', { name: 'Expand code' }), 'CODE +2 more'),
    ).toEqual(closedCodeHeader);
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
    const canvas = within(canvasElement);
    const code = canvasElement.querySelector('[data-execution-code]')!;
    // Regression #222: the execution group owns the common CODE/ACTIVITY/RUN surface.
    const activity = canvasElement.querySelector('[data-execution-group]')!;
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

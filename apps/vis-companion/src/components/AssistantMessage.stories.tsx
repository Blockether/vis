import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';
import { AssistantMessage, UserMessage } from './ChatContent';
import digestCases from '../../../../packages/vis-contract/resources/vis-contract/fixtures/activity-digest.json';
import type { TranscriptTurn } from '../lib/types';

const response = `## Walidacja strategii

Wyniki wszystkich scenariuszy wraz z przedziałem ufności. Tabela i tekst korzystają z tej samej szerokości odpowiedzi.

| scenariusz | dni | PnL | przedział 95% | bez 5 najlepszych dni | I / II połowa |
| --- | --- | --- | --- | --- | --- |
| wybicie 25 bps | 184 | +22,38 | -4,86 … +52,25 | -3,33 | +23,79 / -1,40 |
| wybicie stres 50 bps/2 h | 184 | +15,53 | -10,76 … +44,40 | -10,24 | +18,47 / -2,94 |
| sygnał S06 primary | 365 | -2,10 | -32,47 … +28,37 | -22,97 | -8,75 / +6,64 |
| sygnał S06 stres | 365 | -12,48 | -39,28 … +12,90 | -27,63 | -14,76 / +2,28 |

Wniosek: dodatni wynik nie jest odróżnialny od zera. Wszystkie dane pozostają dostępne również na wąskim ekranie.`;

const meta = {
  title: 'Components/Assistant message',
  component: AssistantMessage,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="mx-auto w-full max-w-3xl px-3.5 pt-4 sm:px-6 sm:pt-6">
        <Story />
      </div>
    ),
  ],
  args: {
    turn: {
      turn_id: 'answer-table',
      status: 'completed',
      iterations: [{ answer: response }],
    },
  },
} satisfies Meta<typeof AssistantMessage>;
export default meta;
type Story = StoryObj<typeof meta>;

export const AnswerTable: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const table = canvas.getByRole('table');
    const answer = table.closest('.bg-answer')!;
    const style = getComputedStyle(answer);
    expect(style.paddingLeft).toBe(style.paddingRight);
    expect(getComputedStyle(table).fontSize).toBe(style.fontSize);
    expect(getComputedStyle(table).lineHeight).toBe(style.lineHeight);
    expect(getComputedStyle(table).borderCollapse).toBe('separate');
    expect(getComputedStyle(table).borderSpacing).toBe('0px');
    const rows = (table as HTMLTableElement).rows;
    for (const row of rows) {
      const last = row.cells[row.cells.length - 1];
      expect(
        Math.abs(last.getBoundingClientRect().right - table.getBoundingClientRect().right),
      ).toBeLessThan(1);
      expect(getComputedStyle(last).borderRightWidth).toBe('0px');
    }
    for (const cell of rows[rows.length - 1].cells) {
      expect(getComputedStyle(cell).borderBottomWidth).toBe('0px');
      expect(
        Math.abs(cell.getBoundingClientRect().bottom - table.getBoundingClientRect().bottom),
      ).toBeLessThan(1);
    }
    const scroller = table.parentElement!;
    const frame = getComputedStyle(scroller);
    for (const edge of [
      frame.borderTopWidth,
      frame.borderRightWidth,
      frame.borderBottomWidth,
      frame.borderLeftWidth,
    ]) {
      expect(edge).toBe('1px');
    }
    expect(table.getBoundingClientRect().bottom).toBeLessThanOrEqual(
      scroller.getBoundingClientRect().bottom - 1,
    );
    expect(scroller.getBoundingClientRect().right).toBeLessThanOrEqual(
      answer.getBoundingClientRect().right + 1,
    );
    expect(canvas.getByRole('region', { name: 'Table' })).toBe(scroller);
    scroller.focus();
    expect(scroller).toHaveFocus();
    scroller.blur();
    expect(canvas.getByText(/^Wniosek:/)).toBeVisible();
  },
};

export const WideAnswerTable: Story = {
  ...AnswerTable,
  args: {
    turn: {
      turn_id: 'wide-answer-table',
      status: 'completed',
      iterations: [
        {
          answer: response.replace(
            'wybicie 25 bps',
            '`public_fragility_validation_with_a_long_unbroken_identifier`',
          ),
        },
      ],
    },
  },
};

const listAnswer = `Both kinds of list hang from one column.

- bulleted item
- another bulleted item

1. first numbered item
2. second numbered item
3. third numbered item
4. fourth numbered item
5. fifth numbered item
6. sixth numbered item
7. seventh numbered item
8. eighth numbered item
9. ninth numbered item
10. tenth numbered item
11. eleventh numbered item
12. twelfth numbered item

Loose items keep the marker on the first line:

1. first loose item

2. second loose item`;

// User report (screenshot): the numbered list under a paragraph started further left than
// the bulleted list above it, because a native marker box is sized by the engine. The
// markers are painted from CSS now, so this measures the geometry in a real browser.
export const AnswerLists: Story = {
  args: {
    turn: {
      turn_id: 'answer-lists',
      status: 'completed',
      iterations: [{ answer: listAnswer }],
    },
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const [bulleted, numbered, loose] = canvas.getAllByRole('list');
    const left = (element: Element) => element.getBoundingClientRect().left;

    // One `ch` in the list's own font: the exact unit the marker columns are written in.
    const probe = document.createElement('span');
    probe.textContent = '0'.repeat(100);
    probe.style.cssText = 'position:absolute;visibility:hidden;white-space:pre';
    bulleted.appendChild(probe);
    const ch = probe.getBoundingClientRect().width / 100;
    probe.remove();

    // The report itself: the markers of every list share one edge, two characters in from
    // prose. A marker sits on the left edge of its item's padding box, so that is the edge
    // to measure — the list box itself always starts on the prose edge.
    const markerEdge = (list: Element) =>
      left(list) + parseFloat(getComputedStyle(list).paddingLeft);
    expect(Math.abs(markerEdge(numbered) - markerEdge(bulleted))).toBeLessThan(0.5);
    expect(Math.abs(markerEdge(loose) - markerEdge(bulleted))).toBeLessThan(0.5);
    const intro = canvas.getByText(/^Both kinds of list/);
    expect(left(bulleted)).toBeCloseTo(left(intro), 0);
    expect(markerEdge(bulleted) - left(intro)).toBeCloseTo(2 * ch, 0);

    // Each list keeps its own text column, wide enough for its widest marker.
    const column = (list: Element) => {
      const item = list.querySelector('li')!;
      expect(getComputedStyle(item).listStyleType).toBe('none');
      return parseFloat(getComputedStyle(item).paddingLeft) / ch;
    };
    expect(column(bulleted)).toBeCloseTo(2, 1);
    expect(column(numbered)).toBeCloseTo(4, 1);
    expect(column(loose)).toBeCloseTo(3, 1);

    // "1." through "12." share one text column: the marker column never moves per item.
    for (const list of [bulleted, numbered, loose]) {
      const columns = new Set(
        [...list.querySelectorAll('li')].map((item) =>
          Math.round(left(item) + parseFloat(getComputedStyle(item).paddingLeft)),
        ),
      );
      expect(columns.size).toBe(1);
      const marker = getComputedStyle(list.querySelector('li')!, '::before');
      expect(marker.position).toBe('absolute');
      expect(marker.left).toBe('0px');
      expect(marker.content).not.toBe('none');
    }
    const bullet = getComputedStyle(bulleted.querySelector('li')!, '::before').content;
    expect(bullet).toBe('"•"');
    expect(getComputedStyle(numbered.querySelector('li')!, '::before').content).not.toBe(bullet);

    // A loose item wraps its text in a paragraph; the marker still shares its first line.
    const looseItem = loose.querySelector('li')!;
    expect(looseItem.querySelector('p')!.getBoundingClientRect().top).toBeCloseTo(
      looseItem.getBoundingClientRect().top,
      0,
    );
  },
};

export const TurnHeaders: Story = {
  render: () => (
    <div className="space-y-6">
      {[280, 720].map((width) => (
        <section key={width} style={{ width, maxWidth: '100%' }}>
          <UserMessage position={42} createdAt={1_789_545_327_000}>
            A request
          </UserMessage>
          <AssistantMessage
            turn={{
              turn_id: 'turn-42',
              position: 42,
              created_at: 1_789_545_327_000,
              status: 'completed',
            }}
            onFork={fn()}
          />
        </section>
      ))}
    </div>
  ),
  play: async ({ canvasElement }) => {
    // Both clients stamp a turn the same way: the time in en-GB on a 24-hour clock,
    // then the turn number (`fix(transcript): align app and TUI turn header format`).
    const timestamp = new Intl.DateTimeFormat('en-GB', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hourCycle: 'h23',
    }).format(1_789_545_327_000);
    const headers = canvasElement.querySelectorAll('article > div:first-child');
    expect(headers).toHaveLength(4);
    for (const header of headers) {
      const time = header.querySelector('time')!;
      expect(header).toHaveTextContent(`${timestamp} / T42`);
      expect(time).toBeVisible();
      const bounds = header.getBoundingClientRect();
      const stamp = time.parentElement!.getBoundingClientRect();
      expect(stamp.left).toBeGreaterThanOrEqual(bounds.left);
      expect(stamp.right).toBeLessThanOrEqual(bounds.right + 1);
      expect(stamp.bottom).toBeLessThanOrEqual(bounds.bottom + 1);
      expect(getComputedStyle(time.parentElement!).opacity).toBe('1');
    }
  },
};

const [failingDigest] = digestCases.filter((sample) => sample.valid).map((sample) => sample.digest);

const foldedTurn = {
  turn_id: 'folded-turn',
  position: 7,
  status: 'completed',
  iterations: [
    {
      id: 'iteration-1',
      position: 1,
      forms: [
        {
          source: 'run_tests()',
          activity: {
            state: 'succeeded',
            counts: { running: 0, succeeded: 1, failed: 0, cancelled: 0 },
            rows: failingDigest.attention,
            omitted: { rows: 0, by_classification: {} },
          },
        },
      ],
    },
  ],
  content: [{ id: 'answer', type: 'prose', markdown: 'The parser still fails one test.' }],
  digest: failingDigest,
} as unknown as TranscriptTurn;

// A finished turn folds to the engine's digest: one row, the failing check pinned beneath it,
// then the answer. Pressing the row opens the whole trace in place of the pinned outcomes.
export const FoldedFinishedTurn: Story = {
  render: () => (
    <div className="space-y-6">
      {[280, 720].map((width) => (
        <section key={width} data-width={width} style={{ width, maxWidth: '100%' }}>
          <AssistantMessage turn={foldedTurn} />
        </section>
      ))}
    </div>
  ),
  play: async ({ canvasElement }) => {
    const sections = [...canvasElement.querySelectorAll<HTMLElement>('[data-width]')];
    expect(sections).toHaveLength(2);
    for (const section of sections) {
      const scope = within(section);
      const row = scope.getByRole('button', { name: failingDigest.summary });
      const attention = section.querySelector('[data-activity-attention]')!;
      const answer = scope.getByText('The parser still fails one test.');

      expect(row).toHaveAttribute('aria-expanded', 'false');
      expect(attention).toBeVisible();
      expect(attention).toHaveTextContent('3 passed, 1 failed');
      expect(answer).toBeVisible();
      expect(section.querySelector('[data-activity-axis]')).toBeNull();
      // The summary wraps inside the column instead of widening it, and the order holds:
      // digest, pinned outcome, answer.
      const bounds = section.getBoundingClientRect();
      expect(row.getBoundingClientRect().right).toBeLessThanOrEqual(bounds.right + 1);
      expect(attention.getBoundingClientRect().top).toBeGreaterThanOrEqual(
        row.getBoundingClientRect().bottom - 1,
      );
      expect(answer.getBoundingClientRect().top).toBeGreaterThanOrEqual(
        attention.getBoundingClientRect().bottom - 1,
      );
    }

    const [narrow, wide] = sections;
    await userEvent.click(within(narrow).getByRole('button', { name: failingDigest.summary }));
    expect(within(narrow).getByRole('button', { name: failingDigest.summary })).toHaveAttribute(
      'aria-expanded',
      'true',
    );
    expect(narrow.querySelector('[data-activity-axis]')).toBeVisible();
    expect(narrow.querySelector('[data-activity-attention]')).toBeNull();
    // Each turn folds on its own: the other width keeps its digest.
    expect(wide.querySelector('[data-activity-attention]')).toBeVisible();
    expect(wide.querySelector('[data-activity-axis]')).toBeNull();
  },
};

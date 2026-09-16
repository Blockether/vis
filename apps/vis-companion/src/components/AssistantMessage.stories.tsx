import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, within } from 'storybook/test';
import { AssistantMessage, UserMessage } from './ChatContent';

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

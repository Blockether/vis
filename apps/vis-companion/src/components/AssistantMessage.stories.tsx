import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, within } from 'storybook/test';
import { AssistantMessage } from './ChatContent';

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
  title: 'Session/Transcript/Answer',
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
    const scroller = table.parentElement!;
    expect(canvas.getByRole('region', { name: 'Table' })).toBe(scroller);
    scroller.focus();
    expect(scroller).toHaveFocus();
    scroller.blur();
    expect(canvas.getByText(/^Wniosek:/)).toBeVisible();
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
// markers are painted from CSS now.
export const AnswerLists: Story = {
  args: {
    turn: {
      turn_id: 'answer-lists',
      status: 'completed',
      iterations: [{ answer: listAnswer }],
    },
  },
};

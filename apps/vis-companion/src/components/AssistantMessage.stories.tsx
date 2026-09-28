import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, within } from 'storybook/test';
import { AssistantMessage, UserMessage } from './ChatContent';
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
    const scroller = table.parentElement!;
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
    const [bulleted, , ] = canvas.getAllByRole('list');

    // One `ch` in the list's own font: the exact unit the marker columns are written in.
    const probe = document.createElement('span');
    probe.textContent = '0'.repeat(100);
    probe.style.cssText = 'position:absolute;visibility:hidden;white-space:pre';
    bulleted.appendChild(probe);
    probe.remove();
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
    }
  },
};

const notesFirstTurn = {
  turn_id: 'notes-first-turn',
  position: 7,
  status: 'completed',
  iterations: [
    {
      id: 'step-1',
      position: 1,
      thinking: 'Read the parser first.',
      forms: [{ source: 'read_parser()', duration_ms: 12 }],
    },
    {
      id: 'step-2',
      position: 2,
      thinking: 'Then its tests.',
      forms: [{ source: 'read_tests()', duration_ms: 9 }],
    },
    {
      id: 'step-3',
      position: 3,
      assistant_prose: 'Sources read; running the suite now.',
      forms: [{ source: 'run_tests()', duration_ms: 840 }],
    },
  ],
  content: [{ id: 'answer', type: 'prose', markdown: 'The parser still fails one test.' }],
} as unknown as TranscriptTurn;

// A finished turn keeps its progress notes: the steps between two notes share one Activity,
// then the answer follows.
export const NotesFirstFinishedTurn: Story = {
  render: () => (
    <div className="space-y-6">
      {[280, 720].map((width) => (
        <section key={width} data-width={width} style={{ width, maxWidth: '100%' }}>
          <AssistantMessage turn={notesFirstTurn} />
        </section>
      ))}
    </div>
  ),
  play: async ({ canvasElement }) => {
    const sections = [...canvasElement.querySelectorAll<HTMLElement>('[data-width]')];
    expect(sections).toHaveLength(2);
    for (const section of sections) {
      const scope = within(section);
      const traces = [...section.querySelectorAll<HTMLElement>('[aria-label="Execution trace"]')];
      const note = scope.getByText('Sources read; running the suite now.');
      const answer = scope.getByText('The parser still fails one test.');
      expect(traces).toHaveLength(2);
      expect(note).toBeVisible();
      expect(answer).toBeVisible();
    }
  },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, waitFor, within } from 'storybook/test';
import {
  STORY_COMPOSER_CLIENT as baseClient,
  STORY_COMPOSER_SESSION as baseSession,
  STORY_COMPOSER_SUBSCRIPTIONS as baseSubscriptions,
} from '../dev/story-data';
import type { RunningTurn } from '../lib/running-turn';
import type { SseEvent, TranscriptIteration, TranscriptTurn } from '../lib/types';
import { isViewportRotating } from '../lib/viewport';
import { SessionScreen } from './SessionScreen';

// BLO-170, reported from the desktop app: "at the end of the turn my scroll goes
// back to the previous turn". The reader is watching the newest turn as it
// finishes; the terminal frame retires the running-turn bubble and its persisted
// row takes that place, with real browser layout and SessionScreen's own
// observers deciding where the scroller ends up.

const lines = (label: string, count: number) =>
  Array.from(
    { length: count },
    (_, index) =>
      `${label} paragraph ${index + 1}: a sentence long enough to wrap in the transcript column and add real height to the scroller.`,
  ).join('\n\n');

function trace(label: string, count: number): TranscriptIteration[] {
  return Array.from({ length: count }, (_, index) => ({
    id: `${label}-i${index + 1}`,
    position: index + 1,
    thinking: lines(`${label} thinking ${index + 1}`, 3),
    assistant_prose: lines(`${label} prose ${index + 1}`, 4),
    forms: [],
  }));
}

const history: TranscriptTurn[] = [1, 2].map((n) => ({
  turn_id: `earlier-${n}`,
  position: n,
  request: `earlier question ${n}`,
  status: 'completed',
  created_at: Date.now() - 600_000,
  completed_at: Date.now() - 500_000,
  content: [{ id: `earlier-${n}-answer`, type: 'prose', markdown: lines(`earlier answer ${n}`, 6) }],
  iterations: trace(`earlier-${n}`, 3),
}));

const RUNNING_ID = 'settling-turn';
const liveTrace = trace('live', 6);
const finalAnswer = lines('final answer', 5);
const turn: RunningTurn = {
  id: RUNNING_ID,
  request: 'THE NEWEST QUESTION',
  answer: '',
  status: 'running',
  startedAt: Date.now() - 60_000,
  iterations: liveTrace,
};
const settledRow: TranscriptTurn = {
  turn_id: RUNNING_ID,
  position: 3,
  request: 'THE NEWEST QUESTION',
  status: 'completed',
  created_at: Date.now() - 60_000,
  completed_at: Date.now(),
  content: [{ id: 'final-answer', type: 'prose', markdown: finalAnswer }],
  iterations: liveTrace,
};

// What a user cancel actually persists, read back from a real gateway transcript:
// an `interrupted` row with `content: []` and HOLLOW iteration records — the same
// positions the reader was watching, with no prose, no thinking and no steps in
// them. Everything on screen lives in this screen's cancelled-turn snapshot.
const cancelledRow: TranscriptTurn = {
  turn_id: RUNNING_ID,
  position: 3,
  request: 'THE NEWEST QUESTION',
  status: 'interrupted',
  created_at: Date.now() - 60_000,
  completed_at: Date.now(),
  content: [],
  iterations: liveTrace.map((iteration) => ({
    id: iteration.id,
    position: iteration.position,
    thinking: '',
    assistant_prose: '',
    forms: [],
  })),
};
const session = {
  ...baseSession,
  id: 'turn-settle-scroll',
  status: 'running' as const,
  live: true,
  current_turn_id: RUNNING_ID,
};

// The engine persists the finished row a moment AFTER the terminal frame.
let persisted = false;
let persistedRow: TranscriptTurn = settledRow;
const client = new Proxy(baseClient, {
  get(target, key) {
    if (key === 'cachedSession') return () => session;
    if (key === 'session') return async () => session;
    if (key === 'cachedRunningTurn') return () => ({ turn, seq: 1 });
    if (key === 'cachedTranscript') return () => history;
    if (key === 'transcript') return async () => (persisted ? [...history, persistedRow] : history);
    // The snapshot on screen already reaches the tail of the session.
    if (key === 'transcriptWindow') return () => ({ offset: 0, total: history.length });
    if (key === 'transcriptIfMoved') return async () => null;
    return Reflect.get(target, key);
  },
});
const listeners = new Set<(event: SseEvent) => void>();
const subscriptions = {
  ...baseSubscriptions,
  subscribeSession: (_sid: string, on: (event: SseEvent) => void) => {
    listeners.add(on);
    return () => {
      listeners.delete(on);
    };
  },
} as typeof baseSubscriptions;

const meta = {
  title: 'Screens/Session Settle scroll',
  component: SessionScreen,
  parameters: { layout: 'fullscreen' },
  globals: { viewport: { value: 'desktop', isRotated: false } },
  decorators: [
    (Story) => (
      <div className="flex h-[800px] w-full flex-col bg-page">
        <Story />
      </div>
    ),
  ],
  args: { client, subscriptions, sid: session.id, onBack: fn(), onOpenSession: fn() },
} satisfies Meta<typeof SessionScreen>;
export default meta;
type Story = StoryObj<typeof meta>;

async function paint() {
  await new Promise<void>((resolve) =>
    requestAnimationFrame(() => requestAnimationFrame(() => resolve())),
  );
}

export const ReaderAtTheEnd: Story = {
  play: async ({ canvasElement }) => {
    persisted = false;
    persistedRow = settledRow;
    const canvas = within(canvasElement);
    const viewport = await canvas.findByRole('region', { name: 'Transcript' });
    await viewport.ownerDocument.fonts.ready;
    await waitFor(() => expect(isViewportRotating()).toBe(false), { timeout: 2000 });
    await paint();
    const gap = () => viewport.scrollHeight - viewport.clientHeight - viewport.scrollTop;

    // The reader is watching the turn being written: at the end of it.
    viewport.scrollTop = viewport.scrollHeight;
    await paint();
    await new Promise((resolve) => setTimeout(resolve, 400));
    await paint();
    await expect(gap()).toBeLessThan(2);

    persisted = true;
    for (const on of listeners)
      on({
        type: 'turn.completed',
        turn_id: RUNNING_ID,
        seq: 2,
        status: 'completed',
        content: [{ id: 'final-answer', type: 'prose', markdown: finalAnswer }],
      } as unknown as SseEvent);

    // Someone reading the end stays at the end through the whole handover: no
    // frame of it is allowed to move the transcript under them.
    let worst = 0;
    const started = Date.now();
    while (Date.now() - started < 1500) {
      worst = Math.max(worst, gap());
      await paint();
    }
    await expect(worst).toBeLessThan(2);
    await waitFor(() => expect(canvasElement.querySelector('[data-live="true"]')).toBeNull());
    await paint();
    await expect(gap()).toBeLessThan(2);
  },
};

export const ReaderInTheNewestTurn: Story = {
  play: async ({ canvasElement }) => {
    persisted = false;
    persistedRow = settledRow;
    const canvas = within(canvasElement);
    const viewport = await canvas.findByRole('region', { name: 'Transcript' });
    await viewport.ownerDocument.fonts.ready;
    await waitFor(() => expect(isViewportRotating()).toBe(false), { timeout: 2000 });
    await paint();

    // The reader is reading the newest turn, a screenful above its live end.
    viewport.scrollTop = viewport.scrollHeight;
    await paint();
    viewport.dispatchEvent(new WheelEvent('wheel', { deltaY: -400, bubbles: true }));
    viewport.scrollTop -= 400;
    await paint();
    // Let the reader-gesture grace expire, as a reader who stopped scrolling does.
    await new Promise((resolve) => setTimeout(resolve, 400));
    await paint();

    // The line they are on, measured against the top edge of the viewport.
    const marker = /live prose 5 paragraph 1/;
    const line = () => canvas.getAllByText(marker).at(-1)!;
    const offset = () => line().getBoundingClientRect().top - viewport.getBoundingClientRect().top;
    const chosen = offset();
    await expect(canvas.getByRole('button', { name: /\d+ messages?/ })).toBeVisible();

    persisted = true;
    for (const on of listeners)
      on({
        type: 'turn.completed',
        turn_id: RUNNING_ID,
        seq: 2,
        status: 'completed',
        content: [{ id: 'final-answer', type: 'prose', markdown: finalAnswer }],
      } as unknown as SseEvent);

    // BLO-170: the end of the turn used to re-run the opening pin and throw the
    // reader down to the end. Their line, and the invitation back to the end, both
    // stay where they are — and not only once it is over. The swap used to unmount
    // the oldest mounted turn in the very commit that mounted the settled row,
    // taking ~940 px out of the transcript; the scroller clamped the reader half a
    // screen up for the frame that painted next, and the anchor could only put them
    // back once the content was tall again. Their line holds on EVERY frame now.
    let worst = 0;
    const started = Date.now();
    while (Date.now() - started < 1500) {
      worst = Math.max(worst, Math.abs(offset() - chosen));
      await paint();
    }
    await expect(worst).toBeLessThan(2);
    await waitFor(() => expect(canvasElement.querySelector('[data-live="true"]')).toBeNull());
    await paint();
    await expect(Math.abs(offset() - chosen)).toBeLessThan(2);
    await expect(canvas.getByRole('button', { name: /\d+ messages?/ })).toBeVisible();
  },
};

// The follow-up: the same reader, the same newest turn, stopped instead of finished.
// A user cancel persists the row WITHOUT the tail the reader is looking at — the
// engine writes `:content []` and the iteration in flight never lands — so the
// durable row is SHORTER than the bubble it replaces, and the streamed tail only
// survives through the cancelled-turn snapshot this screen keeps.
export const ReaderInTheCancelledTurn: Story = {
  play: async ({ canvasElement }) => {
    persisted = false;
    persistedRow = cancelledRow;
    const canvas = within(canvasElement);
    const viewport = await canvas.findByRole('region', { name: 'Transcript' });
    await viewport.ownerDocument.fonts.ready;
    await waitFor(() => expect(isViewportRotating()).toBe(false), { timeout: 2000 });
    await paint();

    // The reader is reading the newest turn, a screenful above its live end.
    viewport.scrollTop = viewport.scrollHeight;
    await paint();
    viewport.dispatchEvent(new WheelEvent('wheel', { deltaY: -400, bubbles: true }));
    viewport.scrollTop -= 400;
    await paint();
    await new Promise((resolve) => setTimeout(resolve, 400));
    await paint();

    const marker = /live prose 5 paragraph 1/;
    const line = () => canvas.getAllByText(marker).at(-1)!;
    const offset = () => line().getBoundingClientRect().top - viewport.getBoundingClientRect().top;
    const chosen = offset();

    persisted = true;
    for (const on of listeners)
      on({
        type: 'turn.cancelled',
        turn_id: RUNNING_ID,
        seq: 2,
        status: 'cancelled',
        content: [],
      } as unknown as SseEvent);

    // Stopping a turn keeps the reader on their line, on every frame of the swap.
    let worst = 0;
    const started = Date.now();
    while (Date.now() - started < 1500) {
      worst = Math.max(worst, Math.abs(offset() - chosen));
      await paint();
    }
    await expect(worst).toBeLessThan(2);
    await waitFor(() => expect(canvasElement.querySelector('[data-live="true"]')).toBeNull());
    await paint();
    await expect(Math.abs(offset() - chosen)).toBeLessThan(2);
    // What was on screen when they pressed stop is still on screen afterwards.
    await expect(canvas.getAllByText(/live prose 6 paragraph 1/).length).toBeGreaterThan(0);
  },
};

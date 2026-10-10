import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, waitFor, within } from 'storybook/test';
import {
  STORY_COMPOSER_CLIENT as baseClient,
  STORY_COMPOSER_SESSION as baseSession,
  STORY_COMPOSER_SUBSCRIPTIONS as baseSubscriptions,
} from '../dev/story-data';
import type { SseEvent, TranscriptIteration, TranscriptTurn } from '../lib/types';
import { isViewportRotating } from '../lib/viewport';
import { SessionScreen } from './SessionScreen';

// A new message threw the reader up the page: a queued turn (or an automation or a
// Council wake) starts in the same breath as the terminal frame of the turn they
// are reading. The next bubble took the finished one's place at once, and the
// finished turn left the transcript until its durable row was read a round trip
// later. The page shrank under the reader, who was left on the turn before it.

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

const HISTORY_TURNS = 12;
const history: TranscriptTurn[] = Array.from({ length: HISTORY_TURNS }, (_, index) => {
  const n = index + 1;
  return {
    turn_id: `earlier-${n}`,
    position: n,
    request: `earlier question ${n}`,
    status: 'completed',
    created_at: Date.now() - 600_000 + n * 1000,
    completed_at: Date.now() - 500_000 + n * 1000,
    content: [{ id: `earlier-${n}-answer`, type: 'prose', markdown: lines(`earlier answer ${n}`, 6) }],
    iterations: trace(`earlier-${n}`, 3),
  } as TranscriptTurn;
});

const FINISHED_ID = 'finished-turn';
const finalAnswer = lines('finished answer', 24);
const finishedRow: TranscriptTurn = {
  turn_id: FINISHED_ID,
  position: HISTORY_TURNS + 1,
  request: 'THE QUESTION BEING READ',
  status: 'completed',
  created_at: Date.now() - 60_000,
  completed_at: Date.now(),
  content: [{ id: 'finished-answer', type: 'prose', markdown: finalAnswer }],
  iterations: [],
};
const session = { ...baseSession, id: 'queue-drain-scroll', status: 'idle' as const, live: false };

// The finished row is readable only after `persistedAt`: past the settle retries,
// so the reconcile tick is what brings it in.
let persistedAt = Number.POSITIVE_INFINITY;
const persisted = () => (Date.now() >= persistedAt ? [...history, finishedRow] : history);
const client = new Proxy(baseClient, {
  get(target, key) {
    if (key === 'cachedSession') return () => session;
    if (key === 'session') return async () => session;
    if (key === 'cachedTranscript') return () => history;
    if (key === 'transcript') return async () => structuredClone(persisted());
    if (key === 'transcriptIfMoved') return async () => structuredClone(persisted());
    if (key === 'transcriptWindow') return () => ({ offset: 0, total: history.length });
    return Reflect.get(target, key);
  },
});
const listeners = new Set<(event: SseEvent) => void>();
const emit = (event: Record<string, unknown>) => {
  for (const on of listeners) on(event as unknown as SseEvent);
};
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
  title: 'Screens/Session Queue drain scroll',
  // Scroll positions need real browser layout, which the jsdom story run lacks: open these
  // stories in Storybook to play them.
  tags: ['!test'],
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
const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

export const QueuedTurnStartsBeforeTheRow: Story = {
  play: async ({ canvasElement }) => {
    persistedAt = Number.POSITIVE_INFINITY;
    const canvas = within(canvasElement);
    const viewport = await canvas.findByRole('region', { name: 'Transcript' });
    await viewport.ownerDocument.fonts.ready;
    await waitFor(() => expect(isViewportRotating()).toBe(false), { timeout: 2000 });

    // The reader follows a long answer as it streams to its end.
    emit({ type: 'turn.started', turn_id: FINISHED_ID, seq: 1, request: finishedRow.request });
    emit({
      type: 'content.block.delta',
      turn_id: FINISHED_ID,
      seq: 2,
      field: 'markdown',
      block_id: `${FINISHED_ID}:answer`,
      cumulative: finalAnswer,
    });
    await sleep(500);
    viewport.scrollTop = viewport.scrollHeight;
    await sleep(500);
    await paint();
    const gap = () => viewport.scrollHeight - viewport.clientHeight - viewport.scrollTop;
    await expect(gap()).toBeLessThan(2);
    const endBefore = viewport.scrollHeight - viewport.clientHeight;
    const firstTurn = () =>
      canvasElement.querySelector('[data-turn-id]')?.getAttribute('data-turn-id');
    const oldestOnScreen = firstTurn();

    // The turn ends, and the queued turn starts in the same breath.
    persistedAt = Date.now() + 1500;
    emit({
      type: 'turn.completed',
      turn_id: FINISHED_ID,
      seq: 3,
      status: 'completed',
      content: [{ id: 'finished-answer', type: 'prose', markdown: finalAnswer }],
    });
    emit({ type: 'turn.started', turn_id: 'queued-turn', seq: 4, request: 'THE QUEUED QUESTION' });
    await canvas.findByText('THE QUEUED QUESTION');

    // Until its row lands and after, the finished answer stays where the reader is,
    // the page never shrinks under them, and no older row leaves the window.
    const started = Date.now();
    let lowestEnd = Number.POSITIVE_INFINITY;
    let worstGap = 0;
    while (Date.now() - started < 3500) {
      lowestEnd = Math.min(lowestEnd, viewport.scrollHeight - viewport.clientHeight);
      worstGap = Math.max(worstGap, gap());
      await expect(canvasElement.textContent).toContain('finished answer paragraph 24');
      await expect(firstTurn()).toBe(oldestOnScreen);
      await paint();
    }
    await expect(lowestEnd).toBeGreaterThanOrEqual(endBefore);
    await expect(worstGap).toBeLessThan(2);
    await expect(canvasElement.querySelector('[data-settling]')).toBeNull();
  },
};

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

// BLO-170, reported a second time from the desktop app: the reader follows a LIVE
// turn to the end, and the moment the running-turn bubble is materialized into its
// persisted answer row they are left looking at the turn ABOVE it, with "↓ Latest"
// offered over the composer.
//
// This session is long enough for the real thing to happen: the render window is
// smaller than the history, so the handover mounts another older row with it, the
// persisted row lands a beat AFTER the terminal frame, and the answer's fenced code
// is highlighted off the critical path — its pixels arrive a frame or two after the
// swap has committed. `ReaderNudgingTheWheel` adds the detail the first fix missed:
// a reader nudging the wheel to follow live output owns the scroller for 300 ms,
// which is exactly the window those pixels land in.

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

const RUNNING_ID = 'settling-turn';
const liveTrace = trace('live', 8);
const codeBlock = [
  '```ts',
  ...Array.from(
    { length: 120 },
    (_, index) => `const answerLine${index} = renderAnswer(${index}, 'a highlighted line of code');`,
  ),
  '```',
].join('\n');
// A real answer carries markdown whose pixels land AFTER React commits: fenced
// code is highlighted off the critical path, so the row grows a frame or two
// after the swap that mounted it.
const finalAnswer = `${lines('final answer', 5)}\n\n${codeBlock}`;
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
  position: HISTORY_TURNS + 1,
  request: 'THE NEWEST QUESTION',
  status: 'completed',
  created_at: Date.now() - 60_000,
  completed_at: Date.now(),
  content: [{ id: 'final-answer', type: 'prose', markdown: finalAnswer }],
  iterations: liveTrace,
};
const session = {
  ...baseSession,
  id: 'turn-settle-scroll-long',
  status: 'running' as const,
  live: true,
  current_turn_id: RUNNING_ID,
};

// The engine persists the finished row a beat AFTER the terminal frame: the
// first settle read misses it, a retry brings it in.
let persistedAt = Number.POSITIVE_INFINITY;
let tracedAt = Number.POSITIVE_INFINITY;
const bareRow: TranscriptTurn = { ...settledRow, iterations: [] };
const client = new Proxy(baseClient, {
  get(target, key) {
    if (key === 'cachedSession') return () => session;
    if (key === 'session') return async () => session;
    if (key === 'cachedRunningTurn') return () => ({ turn, seq: 1 });
    if (key === 'cachedTranscript') return () => history;
    if (key === 'transcript')
      return async () =>
        Date.now() >= persistedAt
          ? [...history, Date.now() >= tracedAt ? settledRow : bareRow]
          : history;
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
  title: 'Screens/Session Handover scroll',
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
function handoverStory(nudge: boolean): Story {
  return {
    play: async ({ canvasElement }) => {
      persistedAt = Number.POSITIVE_INFINITY;
      tracedAt = Number.POSITIVE_INFINITY;
      const canvas = within(canvasElement);
      const viewport = await canvas.findByRole('region', { name: 'Transcript' });
      await viewport.ownerDocument.fonts.ready;
      await waitFor(() => expect(isViewportRotating()).toBe(false), { timeout: 2000 });
      await paint();
      const gap = () => viewport.scrollHeight - viewport.clientHeight - viewport.scrollTop;

      // The reader is at the end of a long transcript, watching the live turn.
      viewport.scrollTop = viewport.scrollHeight;
      await paint();
      await new Promise((resolve) => setTimeout(resolve, 500));
      await paint();
      await expect(gap()).toBeLessThan(2);

      // Native momentum keeps `readerOwnsScroll()` true for the beat the handover
      // lands in — and the reader is still exactly at the end.
      const heightBefore = viewport.scrollHeight;
      if (nudge) {
        viewport.dispatchEvent(new WheelEvent('wheel', { deltaY: 40, bubbles: true }));
        viewport.scrollTop = viewport.scrollHeight;
      }
      // The terminal frame: the persisted row lands a beat later, its highlighted
      // code a beat after that.
      persistedAt = Date.now();
      tracedAt = Date.now() + 60;
      for (const on of listeners)
        on({
          type: 'turn.completed',
          turn_id: RUNNING_ID,
          seq: 2,
          status: 'completed',
          content: [{ id: 'final-answer', type: 'prose', markdown: finalAnswer }],
        } as unknown as SseEvent);

      // The swap puts the persisted row in the bubble's place, and the answer's own
      // pixels — its fenced code, highlighted off the critical path — land after it.
      await waitFor(() => expect(viewport.scrollHeight).toBeGreaterThan(heightBefore), {
        timeout: 2000,
      });
      // A reader who never let go is carried across all of it. A hand on the wheel
      // owns those frames, so the catch-up waits the gesture OUT instead of being
      // dropped with it — which is what left the reader on the previous turn.
      await waitFor(() => expect(gap()).toBeLessThan(2), { timeout: 2000 });

      // ...and they STAY on the newest answer once every late pixel has landed.
      const started = Date.now();
      let worst = 0;
      while (Date.now() - started < 600) {
        worst = Math.max(worst, gap());
        await paint();
      }
      await expect(worst).toBeLessThan(2);
      await expect(
        [...canvasElement.querySelectorAll('button')].some((button) =>
          /\d+ messages?/.test(button.textContent ?? ''),
        ),
      ).toBe(false);
    },
  };
}

export const ReaderAtTheEndOfALongSession: Story = handoverStory(false);
export const ReaderNudgingTheWheel: Story = handoverStory(true);

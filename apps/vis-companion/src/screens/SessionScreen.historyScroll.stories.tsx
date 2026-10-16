import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, waitFor, within } from 'storybook/test';
import {
  STORY_COMPOSER_CLIENT as baseClient,
  STORY_COMPOSER_SESSION as baseSession,
  STORY_COMPOSER_SUBSCRIPTIONS as subscriptions,
  STORY_TURN_ITERATIONS_LONG,
} from '../dev/story-data';
import type { TranscriptTurn } from '../lib/types';
import { isViewportRotating } from '../lib/viewport';
import { SessionScreen } from './SessionScreen';

// Regression, user report: on the web, a wheel scroll up through a long session jumped by
// a whole turn. Each windowed turn reads its earlier steps as it nears the view, and they
// land ABOVE the reader's line while the wheel still owns the scroller.
const TURNS = 20;
const steps = (turn: number) =>
  STORY_TURN_ITERATIONS_LONG.map((step) => ({ ...step, id: `t${turn}-${step.id}` }));
const turns: TranscriptTurn[] = Array.from({ length: TURNS }, (_, turn) => ({
  turn_id: `history-turn-${turn}`,
  request: `Request number ${turn + 1}`,
  status: 'completed',
  iterations: steps(turn).slice(-3),
  iterations_offset: STORY_TURN_ITERATIONS_LONG.length - 3,
  iterations_total: STORY_TURN_ITERATIONS_LONG.length,
  content: [{ id: `answer-${turn}`, type: 'prose', markdown: `Answer number ${turn + 1}.` }],
}));
const session = { ...baseSession, id: 'history-scroll', turn_count: TURNS };
const client = new Proxy(baseClient, {
  get(target, key) {
    if (key === 'cachedSession') return () => session;
    if (key === 'session') return async () => session;
    if (key === 'cachedTranscript') return () => turns;
    if (key === 'transcript' || key === 'transcriptWindow') return async () => turns;
    if (key === 'cachedTurnTrace') return () => null;
    if (key === 'turnTrace')
      return async (_sid: string, turnId: string) => {
        // The gateway answers after the reader has scrolled on: the steps land mid-gesture.
        await new Promise((resolve) => setTimeout(resolve, 120));
        return steps(Number(turnId.split('-').at(-1)));
      };
    return Reflect.get(target, key);
  },
});

const meta = {
  title: 'Screens/Session History scroll',
  // Scroll positions need real browser layout: open this story in Storybook to play it.
  tags: ['!test'],
  component: SessionScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-[720px] w-full flex-col bg-page">
        <Story />
      </div>
    ),
  ],
  args: { client, subscriptions, sid: session.id, onBack: fn(), onOpenSession: fn() },
} satisfies Meta<typeof SessionScreen>;
export default meta;
type Story = StoryObj<typeof meta>;

const frame = () => new Promise<void>((done) => requestAnimationFrame(() => done()));
/** Two frames, then a task: any commit that landed meanwhile has been laid out and painted. */
const settle = async () => {
  await frame();
  await frame();
  await new Promise((done) => setTimeout(done, 0));
};

export const WheelUpThroughWindowedTurns: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const viewport = canvas.getByRole('region', { name: 'Transcript' });
    const transcript = viewport.querySelector('.transcript-column')!;
    await viewport.ownerDocument.fonts.ready;
    await waitFor(() => expect(isViewportRotating()).toBe(false), { timeout: 2000 });
    await waitFor(() => expect(canvas.getByText(`Answer number ${TURNS}.`)).toBeVisible());
    await new Promise((done) => setTimeout(done, 600));
    const box = viewport.getBoundingClientRect();
    const marks = () => viewport.querySelectorAll('[data-earlier-steps]').length;
    const armed = marks();
    const jumps: { step: number; when: string; jump: number }[] = [];
    let step = 0;
    // The line the reader looks at: the middle of the view, where the wheel should leave it.
    let probe: Element | null = null;
    let expected = 0;
    const check = (when: string) => {
      if (!probe?.isConnected) return;
      const jump = probe.getBoundingClientRect().top - expected;
      if (Math.abs(jump) > 2) jumps.push({ step, when, jump: Math.round(jump) });
    };
    // Created after the screen's own observer, so it runs after that correction, before paint.
    const painted = new ResizeObserver(() => check('painted'));
    painted.observe(transcript);
    for (; step < 600 && viewport.scrollTop > 0; step += 1) {
      probe = document.elementFromPoint(box.left + box.width / 2, box.top + box.height / 2);
      const wanted = Math.min(60, viewport.scrollTop);
      expected = (probe?.getBoundingClientRect().top ?? 0) + wanted;
      viewport.dispatchEvent(new WheelEvent('wheel', { deltaY: -60, bubbles: true }));
      viewport.scrollTop -= 60;
      await settle();
      check('settled');
    }
    painted.disconnect();
    (window as unknown as { scrollJumps: unknown }).scrollJumps = { armed, left: marks(), jumps };
    // Earlier steps really landed while the wheel owned the scroller.
    await expect(armed).toBeGreaterThan(marks());
    await expect(jumps).toEqual([]);
  },
};

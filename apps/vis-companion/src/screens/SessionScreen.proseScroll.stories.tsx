import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, waitFor, within } from 'storybook/test';
import {
  STORY_COMPOSER_CLIENT as baseClient,
  STORY_COMPOSER_SESSION as baseSession,
  STORY_COMPOSER_SUBSCRIPTIONS as subscriptions,
} from '../dev/story-data';
import type { TranscriptTurn } from '../lib/types';
import { isViewportRotating } from '../lib/viewport';
import { SessionScreen } from './SessionScreen';

// Regression, user report: on a phone, scrolling up through justified answers moved the
// text and the content below it. Prose above the reader was justified only once the
// scroll came to rest, with new line breaks and a new height.
const TURNS = 24;
const SENTENCES = [
  'The gateway keeps one journal for each session and replays it when a client connects again.',
  'A reader on a narrow phone sees about six words on each line, so every break decides how even the column looks.',
  'Run `vis-agent python -m pytest tests/test_api_guides.py` before you change a guide.',
  'The **fleet stream** covers one gateway, and other machines keep their own poll.',
  'Long identifiers such as `com.blockether.vis.internal.gateway.client/request!` must still wrap where a reader expects.',
  'See the [HTTP API](https://gateway.example.com/docs) for the routes that the Companion uses.',
  'Short lines stay short.',
  'When the column changes width, every paragraph is laid out again for the new width and nothing else moves.',
];
const paragraph = (turn: number, index: number) =>
  Array.from({ length: 3 + ((turn + index) % 4) }, (_, at) => SENTENCES[(turn * 3 + index + at) % SENTENCES.length]).join(' ');
const answer = (turn: number) =>
  Array.from({ length: 4 }, (_, index) => paragraph(turn, index)).join('\n\n');
const turns: TranscriptTurn[] = Array.from({ length: TURNS }, (_, turn) => ({
  turn_id: `prose-turn-${turn}`,
  request: paragraph(turn, 7),
  status: 'completed',
  iterations: [],
  content: [{ id: `answer-${turn}`, type: 'prose', markdown: answer(turn) }],
}));
const session = { ...baseSession, id: 'prose-scroll', turn_count: TURNS };
const client = new Proxy(baseClient, {
  get(target, key) {
    if (key === 'cachedSession') return () => session;
    if (key === 'session') return async () => session;
    if (key === 'cachedTranscript') return () => turns;
    if (key === 'transcript' || key === 'transcriptWindow') return async () => turns;
    if (key === 'cachedTurnTrace') return () => null;
    if (key === 'turnTrace') return async () => [];
    return Reflect.get(target, key);
  },
});

const meta = {
  title: 'Screens/Session Prose scroll',
  // Scroll positions need real browser layout: open this story in Storybook to play it.
  tags: ['!test'],
  component: SessionScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-[780px] w-[390px] flex-col bg-page">
        <Story />
      </div>
    ),
  ],
  args: { client, subscriptions, sid: session.id, onBack: fn(), onOpenSession: fn() },
} satisfies Meta<typeof SessionScreen>;
export default meta;
type Story = StoryObj<typeof meta>;

const frame = () => new Promise<void>((done) => requestAnimationFrame(() => done()));

/** What the reader sees of one scroll up through the session, with lifts and rests. */
export type ProseScrollReport = {
  steps: number;
  jumps: { step: number; when: string; jump: number }[];
  /** Milliseconds of each scroll step: the two frames it waits for, with their work. */
  stepMs: { mean: number; p95: number; max: number };
  /** Nodes that the paragraphs replaced during the scroll. */
  rewrites: number;
  /** Paragraphs that keep a line ragged beside wide gaps. */
  justified: number;
  paragraphs: number;
};

export const ScrollUpThroughJustifiedAnswers: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const viewport = canvas.getByRole('region', { name: 'Transcript' });
    await viewport.ownerDocument.fonts.ready;
    await waitFor(() => expect(isViewportRotating()).toBe(false), { timeout: 2000 });
    await waitFor(() => expect(viewport.scrollTop).toBeGreaterThan(0));
    await new Promise((resolve) => setTimeout(resolve, 600));
    const box = viewport.getBoundingClientRect();
    const jumps: ProseScrollReport['jumps'] = [];
    const times: number[] = [];
    let rewrites = 0;
    const lines = new MutationObserver((records) => {
      for (const record of records)
        if ((record.target as Element).closest?.('p, li')) rewrites += record.addedNodes.length;
    });
    lines.observe(viewport, { subtree: true, childList: true });
    let probe: Element | null = null;
    let expected = 0;
    let step = 0;
    const aim = () => {
      probe = document.elementFromPoint(box.left + box.width / 2, box.top + box.height / 2);
      expected = probe?.getBoundingClientRect().top ?? 0;
    };
    const check = (when: string) => {
      if (!probe?.isConnected) return;
      const jump = probe.getBoundingClientRect().top - expected;
      if (Math.abs(jump) > 2) jumps.push({ step, when, jump: Math.round(jump) });
    };
    for (; step < 400 && viewport.scrollTop > 0; step += 1) {
      aim();
      const wanted = Math.min(80, viewport.scrollTop);
      expected += wanted;
      const started = performance.now();
      viewport.dispatchEvent(new WheelEvent('wheel', { deltaY: -80, bubbles: true }));
      viewport.scrollTop -= 80;
      await frame();
      await frame();
      times.push(performance.now() - started);
      check('scrolled');
      // The reader lifts the finger every few steps and reads.
      if (step % 6 === 5) {
        aim();
        await new Promise((resolve) => setTimeout(resolve, 400));
        await frame();
        check('rested');
      }
    }
    lines.disconnect();
    times.sort((a, b) => a - b);
    const prose = viewport.querySelectorAll('.transcript-column p');
    const report: ProseScrollReport = {
      steps: step,
      jumps,
      stepMs: {
        mean: Math.round((times.reduce((sum, ms) => sum + ms, 0) / times.length) * 10) / 10,
        p95: Math.round(times[Math.floor(times.length * 0.95)] * 10) / 10,
        max: Math.round(times[times.length - 1] * 10) / 10,
      },
      rewrites,
      justified: [...prose].filter((node) => node.hasAttribute('data-justified-lines')).length,
      paragraphs: prose.length,
    };
    (window as unknown as { proseScroll: ProseScrollReport }).proseScroll = report;
    await expect(jumps).toEqual([]);
  },
};

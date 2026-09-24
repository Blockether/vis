import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';
import {
  STORY_COMPOSER_CLIENT as baseClient,
  STORY_COMPOSER_SESSION as baseSession,
  STORY_COMPOSER_SUBSCRIPTIONS as baseSubscriptions,
} from '../dev/story-data';
import type { ActivityProjection, ActivityRow } from '../lib/activity';
import type { RunningTurn } from '../lib/running-turn';
import type { SseEvent } from '../lib/types';
import { isViewportRotating } from '../lib/viewport';
import { SessionScreen } from './SessionScreen';

// #233: use real block.activity delivery, React reconciliation, browser layout
// and SessionScreen's ResizeObserver, rather than a mocked scroll-height change.
function activity(revision: number): ActivityProjection {
  const row = (index: number, operation: string): ActivityRow => ({
    id: `${operation}-${index}`,
    sequence: index,
    operation,
    presenter: 'generic',
    signal: 'observation',
    state: 'succeeded',
    summary: '',
    resources: [],
    evidence: [],
    presentation: {
      headline: `${operation === 'cat' ? 'Read' : 'Search'} file ${index}`,
      summary: '',
      content: [
        {
          type: 'text',
          text: Array.from({ length: 12 }, (_, line) => `Result line ${line + 1}`).join('\n'),
        },
      ],
    },
  });
  const rows = [
    ...Array.from({ length: 10 }, (_, i) => row(i + 1, 'cat')),
    ...Array.from({ length: 12 }, (_, i) => row(i + 11, 'grep')),
    // These arrive last, but regroup ABOVE the reader's Search results.
    ...Array.from({ length: revision - 1 }, (_, i) => row(i + 23, 'cat')),
  ];
  return {
    state: 'running',
    counts: { running: 0, succeeded: rows.length, failed: 0, cancelled: 0 },
    rows,
    omitted: { rows: 0, by_classification: {} },
    history: {
      id: '12345678-1234-1234-1234-123456789012',
      revision,
      total: rows.length,
      after: 0,
      next_after: null,
    },
  };
}

const turn: RunningTurn = {
  id: 'activity-scroll-turn',
  request: 'Inspect these files while more operations arrive.',
  answer: '',
  status: 'running',
  startedAt: Date.now(),
  iterations: [
    {
      id: 'activity-iteration',
      position: 1,
      assistant_prose: '## File inspection\n\n### Operations',
      forms: [
        { block_id: 0, source: 'print(await cat("src/example.clj"))', activity: activity(1) },
      ],
    },
  ],
};
const session = {
  ...baseSession,
  id: 'activity-scroll',
  status: 'running' as const,
  live: true,
  current_turn_id: turn.id,
};
const client = new Proxy(baseClient, {
  get(target, key) {
    if (key === 'cachedSession') return () => session;
    if (key === 'session') return async () => session;
    if (key === 'cachedRunningTurn') return () => ({ turn, seq: 1 });
    if (key === 'cachedTranscript') return () => [];
    if (key === 'transcript' || key === 'transcriptWindow') return async () => [];
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
  title: 'Screens/Session Activity scroll',
  component: SessionScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-[520px] w-full flex-col bg-page">
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

export const LiveRegrouping: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(await canvas.findByRole('button', { name: 'Expand Activity' }));
    await userEvent.click(canvas.getByRole('button', { name: /Read ×10/ }));
    await userEvent.click(canvas.getByRole('button', { name: /Search ×12/ }));
    const target = canvas.getByRole('button', { name: 'Search file 13' });
    await userEvent.click(target);
    const row = target.closest('[data-activity-row]')!;
    const content = row.querySelector<HTMLElement>('[data-activity-content] p')!;
    const viewport = canvas.getByRole('region', { name: 'Transcript' });
    await viewport.ownerDocument.fonts.ready;
    // Settle the phone/desktop viewport change before choosing a reading position.
    await waitFor(() => expect(isViewportRotating()).toBe(false), { timeout: 2000 });
    await paint();
    viewport.scrollTop = viewport.scrollHeight;
    await paint();
    viewport.dispatchEvent(new WheelEvent('wheel', { deltaY: -200, bubbles: true }));
    viewport.scrollTop +=
      content.getBoundingClientRect().top - viewport.getBoundingClientRect().top + 4;
    await paint();
    // Finish the reader-gesture grace period before testing background growth.
    await new Promise((resolve) => setTimeout(resolve, 350));
    const offset = () => content.getBoundingClientRect().top - viewport.getBoundingClientRect().top;
    const chosen = offset();
    await expect(Math.abs(chosen + 4)).toBeLessThan(1);
    await expect(canvas.getByRole('button', { name: /\d+ messages?/ })).toBeVisible();

    const update = async (revision: number) => {
      for (const on of listeners)
        on({ type: 'block.activity', iteration: 1, form_index: 0, activity: activity(revision) });
      await waitFor(() =>
        expect(canvas.getByRole('button', { name: /Collapse Activity/ })).toHaveTextContent(
          `${21 + revision} operations`,
        ),
      );
      await paint();
    };
    for (let revision = 2; revision <= 4; revision++) {
      await update(revision);
      await expect(row.isConnected).toBe(true);
      await expect(target).toHaveAttribute('aria-expanded', 'true');
      await expect(Math.abs(offset() - chosen)).toBeLessThan(1);
    }

    // The same updates must still follow the end after the reader chooses to jump down.
    await userEvent.click(canvas.getByRole('button', { name: /\d+ messages?/ }));
    await waitFor(() =>
      expect(viewport.scrollHeight - viewport.clientHeight - viewport.scrollTop).toBeLessThan(1),
    );
    for (let revision = 5; revision <= 7; revision++) {
      await update(revision);
      await expect(viewport.scrollHeight - viewport.clientHeight - viewport.scrollTop).toBeLessThan(
        1,
      );
    }
  },
};

export const LiveRegroupingPointer: Story = {
  ...LiveRegrouping,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

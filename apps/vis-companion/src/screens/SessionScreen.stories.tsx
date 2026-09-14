import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import {
  STORY_COMPOSER_CLIENT as client,
  STORY_COMPOSER_SESSION as session,
  STORY_COMPOSER_SUBSCRIPTIONS as subscriptions,
  STORY_QUEUED_TURNS,
} from '../dev/story-data';
import { draftMessageKey, hydrateDraftMessages, writeDraftMessage } from '../lib/draft-messages';
import type { RunningTurn } from '../lib/running-turn';
import type { GatewayCapabilities } from '../lib/types';
import { SessionScreen } from './SessionScreen';

const meta = {
  title: 'Screens/Session',
  component: SessionScreen,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="flex h-dvh w-full flex-col bg-page">
        <Story />
      </div>
    ),
  ],
  beforeEach: async () => {
    await hydrateDraftMessages();
    writeDraftMessage(draftMessageKey(client.base, session.id), { text: '' });
  },
  args: {
    client,
    subscriptions,
    sid: session.id,
    onBack: fn(),
    onOpenSession: fn(),
  },
} satisfies Meta<typeof SessionScreen>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Native typing stays independent of the deferred screen snapshot. */
export const ComposerInput: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const composer = await page.findByRole('textbox', { name: 'Message Vis' });
    await expect(composer).toHaveAttribute('spellcheck', 'false');
    await expect(composer).toHaveAttribute('autocorrect', 'off');
    await expect(composer).toHaveAttribute('autocapitalize', 'none');
    const text = 'Keep --budget, — prose, – ranges and "quotes" unchanged. Żółć, gęślą jaźń.';
    await userEvent.type(composer, text);
    await expect(composer).toHaveValue(text);
    await expect((composer as HTMLTextAreaElement).defaultValue).toBe('');
    await expect(composer).toHaveFocus();

    await userEvent.clear(composer);
    await userEvent.type(composer, '/relo');
    await userEvent.click(await page.findByText('/reload'));
    await expect(composer).toHaveValue('/reload ');
    await expect(composer).toHaveFocus();
    await userEvent.clear(composer);
    await userEvent.type(composer, text);
    await expect(composer).toHaveValue(text);
  },
};

/** The input and bare actions share one line, without overlapping touch targets. */
export const ComposerHeights: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const composer = await page.findByRole('textbox', { name: 'Message Vis' });
    await composer.ownerDocument.fonts.ready;
    const attach = page.getByRole('button', { name: 'Choose photos, clips, recordings or files' });
    const send = page.getByRole('button', { name: 'Send message' });
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const height = pointer ? 28 : 32;
    const input = composer.getBoundingClientRect();
    for (const button of [attach, send]) {
      const box = button.getBoundingClientRect();
      await expect(box.height).toBe(height);
      await expect(box.top).toBe(input.top);
      await expect(box.bottom).toBe(input.bottom);
    }
    const reach = pointer ? 0 : 6;
    await expect(input.left - attach.getBoundingClientRect().right - reach).toBeGreaterThanOrEqual(
      8,
    );

    await userEvent.type(
      composer,
      'First line{Shift>}{Enter}{/Shift}Second line{Shift>}{Enter}{/Shift}Third line',
    );
    await expect(composer.getBoundingClientRect().height).toBeGreaterThan(height);
    for (const button of [attach, send]) {
      await expect(button.getBoundingClientRect().bottom).toBe(
        composer.getBoundingClientRect().bottom,
      );
    }
    await userEvent.clear(composer);
    await expect(composer.getBoundingClientRect().height).toBe(height);
  },
};

export const ComposerHeightsPointer: Story = {
  ...ComposerHeights,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

const runningTurn: RunningTurn = {
  id: 'composer-running-turn',
  request: 'Check the control heights.',
  answer: '',
  iterations: [],
  startedAt: Date.now(),
  status: 'running',
};
const runningSession = {
  ...session,
  status: 'running' as const,
  live: true,
  current_turn_id: runningTurn.id,
};
const voiceCapabilities: GatewayCapabilities = {
  version: 1,
  features: {
    chat: { enabled: true },
    attachments: {
      enabled: true,
      transport: 'inline-base64',
      media_types: ['image/png'],
      max_files: 8,
      max_file_bytes: 8 * 1024 * 1024,
    },
    voice: {
      enabled: true,
      transport: 'audio/wav',
      transcription: 'gateway-local',
      model: { status: 'ready' },
    },
  },
};
const runningClient = new Proxy(client, {
  get(target, key) {
    if (key === 'cachedSession') return () => runningSession;
    if (key === 'session') return async () => runningSession;
    if (key === 'cachedRunningTurn') return () => ({ turn: runningTurn, seq: 1 });
    if (key === 'cachedCapabilities') return () => voiceCapabilities;
    if (key === 'capabilities') return async () => voiceCapabilities;
    if (key === 'voiceModel') return async () => voiceCapabilities.features.voice.model;
    return Reflect.get(target, key);
  },
});

/** The full row still fits while voice and cancellation are both available. */
export const RunningComposerHeights: Story = {
  args: { client: runningClient },
  play: async ({ canvasElement }) => {
    const composer = await within(canvasElement).findByRole('textbox', { name: 'Message Vis' });
    await composer.ownerDocument.fonts.ready;
    await expect(composer).toHaveAttribute('placeholder', 'Message Vis — queues next');
    const row = composer.parentElement!;
    const buttons = within(row).getAllByRole('button');
    await expect(buttons).toHaveLength(4);
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const input = composer.getBoundingClientRect();
    // A long placeholder must not paint a clipped second line in an empty one-line field.
    await expect(composer.scrollHeight).toBe(input.height);
    for (const button of buttons) {
      const box = button.getBoundingClientRect();
      await expect(box.height).toBe(pointer ? 28 : 32);
      await expect(box.top).toBe(input.top);
      await expect(box.bottom).toBe(input.bottom);
    }
    const targets = [...row.querySelectorAll('button, textarea')].map((element) => {
      const box = element.getBoundingClientRect();
      const reach = !pointer && element.tagName === 'BUTTON' ? 6 : 0;
      return { left: box.left - reach, right: box.right + reach };
    });
    for (let index = 1; index < targets.length; index += 1) {
      await expect(targets[index].left - targets[index - 1].right).toBeGreaterThanOrEqual(8);
    }
  },
};

export const RunningComposerHeightsPointer: Story = {
  ...RunningComposerHeights,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** The queued message and composer share a square frame at every input density. */
export const QueuedComposer: Story = {
  args: {
    client: new Proxy(runningClient, {
      get(target, key) {
        if (key === 'cachedQueuedTurns') return () => STORY_QUEUED_TURNS.slice(0, 1);
        if (key === 'queuedTurns') {
          return async () => ({ turns: STORY_QUEUED_TURNS.slice(0, 1), paused: null });
        }
        return Reflect.get(target, key);
      },
    }),
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const composer = await page.findByRole('textbox', { name: 'Message Vis' });
    const queued = await page.findByRole('region', { name: 'Queued messages' });
    const field = composer.closest('.rounded-none')!;
    const tray = queued.parentElement!;
    await expect(getComputedStyle(field).borderRadius).toBe('0px');
    await expect(getComputedStyle(tray).borderRadius).toBe('0px');
    await expect(tray.getBoundingClientRect().width).toBe(field.getBoundingClientRect().width);
    await userEvent.type(composer, 'Keep this message queued.');
    await expect(composer).toHaveValue('Keep this message queued.');
  },
};

/** Desktop prose and input use the reading scale, not control or metadata sizes. */
export const ReadingLayout: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const composer = await page.findByRole('textbox', { name: 'Message Vis' });
    await composer.ownerDocument.fonts.ready;
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const title = page.getByRole('heading', { name: session.title! });
    const transcript = page.getByRole('region', { name: 'Transcript' });
    const column = transcript.firstElementChild!;
    const answer = transcript.querySelector('.bg-answer')!;
    const prose = transcript.querySelectorAll(
      '.bg-answer p, .text-you-message-foreground, .text-thinking p, .text-vis-message p',
    );
    await expect(prose.length).toBeGreaterThan(2);
    for (const element of [...prose, composer]) {
      const style = getComputedStyle(element);
      await expect(style.fontFamily).toBe(getComputedStyle(title).fontFamily);
      await expect(style.fontSize).toBe(pointer ? getComputedStyle(title).fontSize : '11px');
      await expect(style.lineHeight).toBe(pointer ? '20px' : '16px');
    }
    const cap = pointer ? 1152 : 768;
    await expect(column.getBoundingClientRect().width).toBe(Math.min(cap, transcript.clientWidth));
    await expect(getComputedStyle(column).maxWidth).toBe(`${cap}px`);
    await expect(transcript.scrollWidth).toBe(transcript.clientWidth);
    if (pointer) {
      const field = composer.closest('.bg-input')!.getBoundingClientRect();
      const message = answer.getBoundingClientRect();
      await expect(field.left).toBe(message.left);
      await expect(field.right).toBe(message.right);
      await expect(getComputedStyle(transcript.querySelector('footer')!).fontSize).toBe('10px');
    }
    // Larger type must not clip a wrapped draft or change the single-line control height.
    await userEvent.type(composer, 'First line{Shift>}{Enter}{/Shift}Second line');
    await expect(composer.getBoundingClientRect().height).toBeGreaterThan(pointer ? 28 : 32);
    await userEvent.clear(composer);
    await expect(composer.getBoundingClientRect().height).toBe(pointer ? 28 : 32);
  },
};

export const ReadingLayoutPointer: Story = {
  ...ReadingLayout,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

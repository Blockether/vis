import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import {
  STORY_COMPOSER_CLIENT as client,
  STORY_COMPOSER_SESSION as session,
  STORY_COMPOSER_SUBSCRIPTIONS as subscriptions,
  STORY_QUEUED_TURNS,
  STORY_COMPOSER_PASTE,
  STORY_PENDING_ATTACHMENTS,
} from '../dev/story-data';
import { openStepDigests } from '../dev/story-steps';
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

    await userEvent.type(
      composer,
      'First line{Shift>}{Enter}{/Shift}Second line{Shift>}{Enter}{/Shift}Third line',
    );
    await userEvent.clear(composer);
  },
};

export const ComposerHeightsPointer: Story = {
  ...ComposerHeights,
  tags: ['!test'],
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
    await expect(composer).toHaveAttribute('placeholder', 'Message Vis — queues next');
    const row = composer.parentElement!;
    const buttons = within(row).getAllByRole('button');
    await expect(buttons).toHaveLength(4);
  },
};

export const RunningComposerHeightsPointer: Story = {
  ...RunningComposerHeights,
  tags: ['!test'],
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
    await page.findByRole('region', { name: 'Queued messages' });
    await userEvent.type(composer, 'Keep this message queued.');
    await expect(composer).toHaveValue('Keep this message queued.');
  },
};

/** Desktop prose and input use the reading scale, not control or metadata sizes. */
export const ReadingLayout: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const composer = await page.findByRole('textbox', { name: 'Message Vis' });
    const transcript = page.getByRole('region', { name: 'Transcript' });
    await openStepDigests(transcript);
    const prose = transcript.querySelectorAll(
      '.bg-answer p, .text-you-message-foreground, .text-vis-message p, .text-thinking p',
    );
    await expect(prose.length).toBeGreaterThan(2);
    // The THINKING trace is a quiet aside: it keeps the ui step on every
    // surface, one step below the reading scale the answer grows to on pointer.
    const trace = transcript.querySelectorAll('.text-thinking p');
    await expect(trace.length).toBeGreaterThan(0);
    // Larger type must not clip a wrapped draft or change the single-line control height.
    await userEvent.type(composer, 'First line{Shift>}{Enter}{/Shift}Second line');
    await userEvent.clear(composer);
  },
};

export const ReadingLayoutPointer: Story = {
  ...ReadingLayout,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** File intake, stable labels and keyboard removal share the same editor. */
export const ImageReferences: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const composer = await page.findByRole('textbox', { name: 'Message Vis' });
    const input = page.getByLabelText('Choose attachment files');
    const pixel = Uint8Array.from(
      atob('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII='),
      (char) => char.charCodeAt(0),
    );
    const blob = new Blob([pixel], { type: 'image/png' });
    await userEvent.type(composer, 'Compare these: ');
    await userEvent.upload(input, new File([blob], 'first.png', { type: 'image/png' }));
    await expect(await page.findByText('[IMAGE #1]')).toBeVisible();
    await userEvent.upload(input, new File([blob], 'second.png', { type: 'image/png' }));
    await expect(await page.findByText('[IMAGE #2]')).toBeVisible();
    await userEvent.click(page.getByRole('button', { name: 'Remove first.png' }));
    await expect(composer).toHaveValue('Compare these:  [IMAGE #2]');
    await userEvent.click(composer);
    await userEvent.keyboard('{End}{Backspace}');
    await expect(page.queryByRole('button', { name: 'Remove second.png' })).not.toBeInTheDocument();
    await userEvent.upload(input, new File([blob], 'third.png', { type: 'image/png' }));
    await expect(await page.findByText('[IMAGE #1]')).toBeVisible();
    await expect(composer).toHaveValue('Compare these:  [IMAGE #1]');
  },
};

export const ImageReferencesMixed: Story = {
  beforeEach: async () => {
    await hydrateDraftMessages();
    writeDraftMessage(draftMessageKey(client.base, session.id), {
      text: `Compare [IMAGE #1] with the notes ${STORY_COMPOSER_PASTE.token}`,
      pastes: [STORY_COMPOSER_PASTE],
      counter: STORY_COMPOSER_PASTE.id,
      imageCounter: 1,
      attachments: STORY_PENDING_ATTACHMENTS.map((attachment) => ({
        ...attachment,
        reference: attachment.media_type.startsWith('image/') ? '[IMAGE #1]' : undefined,
      })),
    });
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    await expect(await page.findByText('[IMAGE #1]')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Edit pasted block 4' })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Remove release-note.m4a' })).toBeVisible();
  },
};

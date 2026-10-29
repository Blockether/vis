import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

import {
  STORY_COMPOSER_CLIENT as client,
  STORY_COMPOSER_SESSION as session,
  STORY_COMPOSER_SUBSCRIPTIONS as subscriptions,
  STORY_QUEUED_TURNS,
  STORY_OUTLINE_CLIENT,
  STORY_OUTLINE_SESSION,
  STORY_OUTLINE_TURNS,
} from '../dev/story-data';
import { openStepDigests } from '../dev/story-steps';
import { draftMessageKey, hydrateDraftMessages, writeDraftMessage } from '../lib/draft-messages';
import type { RunningTurn } from '../lib/running-turn';
import type { GatewayCapabilities } from '../lib/types';
import { SessionScreen } from './SessionScreen';

const meta = {
  title: 'Session/Screen',
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

/**
 * The transcript outline over a session of eight turns (Blockether/vis#316). The
 * rail stands at the right edge of the reading column. The session opens at its
 * newest turn, and picking the first prompt puts that turn at the top.
 */
export const JumpToAMessage: Story = {
  args: { client: STORY_OUTLINE_CLIENT, sid: STORY_OUTLINE_SESSION.id },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement);
    const rail = await page.findByRole('button', { name: 'Jump to a message' });
    const transcript = page.getByRole('region', { name: 'Transcript' });
    await waitFor(() =>
      expect(transcript.querySelectorAll('[data-turn-id]')).toHaveLength(STORY_OUTLINE_TURNS.length),
    );

    await userEvent.click(rail);
    const panel = await within(document.body).findByRole('dialog', { name: 'Jump to a message' });
    const rows = within(panel).getAllByRole('button');
    await expect(rows).toHaveLength(STORY_OUTLINE_TURNS.length);
    await waitFor(() => expect(rows.at(-1)).toHaveAttribute('aria-current', 'true'));

    await userEvent.click(rows[0]);
    const first = transcript.querySelector('[data-turn-id="outline-1"]')!;
    await waitFor(() => {
      const offset = first.getBoundingClientRect().top - transcript.getBoundingClientRect().top;
      expect(offset).toBeGreaterThanOrEqual(0);
      expect(offset).toBeLessThan(40);
    });
  },
};

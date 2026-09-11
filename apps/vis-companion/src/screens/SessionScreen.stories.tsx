import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import {
  STORY_COMPOSER_CLIENT as client,
  STORY_COMPOSER_SESSION as session,
  STORY_COMPOSER_SUBSCRIPTIONS as subscriptions,
} from '../dev/story-data';
import { draftMessageKey, hydrateDraftMessages, writeDraftMessage } from '../lib/draft-messages';
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

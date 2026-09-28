import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_COMPOSER_PASTE, STORY_PENDING_ATTACHMENTS } from '../dev/story-data';
import { ComposerPayloadShelf } from './ComposerPayloadShelf';

const meta = {
  title: 'Session/Composer payload shelf',
  component: ComposerPayloadShelf,
  parameters: { layout: 'centered' },
  args: {
    pastes: [STORY_COMPOSER_PASTE],
    attachments: STORY_PENDING_ATTACHMENTS.map((attachment) => ({
      ...attachment,
      reference: attachment.media_type.startsWith('image/') ? '[IMAGE #1]' : undefined,
    })),
    commands: {
      editPaste: fn(),
      removePaste: fn(),
      editAttachment: fn(),
      removeAttachment: fn(),
    },
  },
} satisfies Meta<typeof ComposerPayloadShelf>;

export default meta;
type Story = StoryObj<typeof meta>;

export const MixedPayload: Story = {
  name: 'Paste, image and recording',
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    for (const attachment of args.attachments) {
      const remove = canvas.getByRole('button', { name: `Remove ${attachment.filename}` });
      await userEvent.click(remove);
      await expect(args.commands.removeAttachment).toHaveBeenCalledWith(attachment.id);
    }
    for (const paste of args.pastes) {
      await userEvent.click(canvas.getByRole('button', { name: `Edit pasted block ${paste.id}` }));
      await expect(args.commands.editPaste).toHaveBeenCalledWith(paste.id);
    }
  },
};

export const UnnumberedImage: Story = {
  args: {
    pastes: [],
    attachments: STORY_PENDING_ATTACHMENTS.filter((attachment) =>
      attachment.media_type.startsWith('image/'),
    ),
  },
  play: MixedPayload.play,
};

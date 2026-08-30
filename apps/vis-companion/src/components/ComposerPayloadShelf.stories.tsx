import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, fn, userEvent, within } from "storybook/test";

import {
  STORY_COMPOSER_PASTE,
  STORY_PENDING_ATTACHMENTS,
} from "../dev/story-data";
import { ComposerPayloadShelf } from "./ComposerPayloadShelf";

const meta = {
  title: "Session/Composer payload shelf",
  component: ComposerPayloadShelf,
  parameters: { layout: "centered" },
  args: {
    pastes: [STORY_COMPOSER_PASTE],
    attachments: STORY_PENDING_ATTACHMENTS,
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
  name: "Paste, image and recording",
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Edit pasted block 4" }),
    );
    await expect(args.commands.editPaste).toHaveBeenCalledWith(4);
  },
};

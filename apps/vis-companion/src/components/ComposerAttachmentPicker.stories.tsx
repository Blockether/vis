import { expect, fn, userEvent, within } from "storybook/test";
import type { Meta, StoryObj } from "@storybook/react-vite";

import { ComposerAttachmentPicker } from "./ComposerAttachmentPicker";

const meta = {
  title: "Session/Composer attachment picker",
  component: ComposerAttachmentPicker,
  parameters: { layout: "fullscreen" },
  render: (args) => (
    <div className="flex h-screen items-end justify-center p-8">
      <ComposerAttachmentPicker {...args} />
    </div>
  ),
  args: {
    accept: "image/*,video/*,audio/*",
    disabled: false,
    isNative: true,
    commands: {
      addBrowserFiles: fn(),
      pickNative: fn(),
    },
  },
} satisfies Meta<typeof ComposerAttachmentPicker>;

export default meta;
type Story = StoryObj<typeof meta>;

export const NativeMenu: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", {
        name: "Attach a photo, clip, recording or file",
      }),
    );
    await expect(canvas.getByRole("dialog", { name: "Attach" })).toBeVisible();
  },
};

import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, fn, userEvent, within } from "storybook/test";

import { STORY_SESSION_ROW } from "../dev/story-data";
import { RenameSessionDialog } from "./RenameSessionDialog";

const meta = {
  title: "Sessions/Rename session",
  component: RenameSessionDialog,
  parameters: { layout: "fullscreen" },
  args: {
    session: STORY_SESSION_ROW,
    onDismiss: fn(),
    onRename: fn(async () => {}),
  },
  render: (args) => (
    <div className="relative h-screen min-h-[32rem] w-screen overflow-hidden bg-ink">
      <RenameSessionDialog {...args} />
    </div>
  ),
} satisfies Meta<typeof RenameSessionDialog>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Editing: Story = {
  play: async ({ args, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const input = page.getByPlaceholderText("Session name");
    await userEvent.clear(input);
    await userEvent.type(input, "Release notes");
    await expect(input).toHaveValue("Release notes");
    await expect(args.onRename).not.toHaveBeenCalled();
  },
};

import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, fn, userEvent } from "storybook/test";

import {
  STORY_GATEWAYS,
  STORY_SESSION,
  STORY_SESSION_ROW,
} from "../dev/story-data";
import { EMPTY_DRAFT_MESSAGE } from "../lib/draft-messages";
import { SessionRow } from "./SessionList";

const onOpen = fn();

const meta = {
  title: "Session/Navigator row",
  component: SessionRow,
  parameters: { layout: "padded" },
  args: {
    session: STORY_SESSION_ROW,
    draft: EMPTY_DRAFT_MESSAGE,
    conn: STORY_GATEWAYS[0],
    match: null,
    needle: "",
    onOpen,
    onRename: fn(),
    onFork: fn(),
    onDelete: fn(),
    onToggleStar: fn(),
    isConfirmingDelete: false,
    deleteBusy: false,
    deleteError: null,
    onConfirmDelete: fn(),
    onCancelDelete: fn(),
  },
} satisfies Meta<typeof SessionRow>;

export default meta;
type Story = StoryObj<typeof meta>;

export const AwaitingInput: Story = {
  play: async ({ canvas }) => {
    const openSession = canvas.getByText(STORY_SESSION.title).closest("button");
    await expect(openSession).not.toBeNull();
    await userEvent.click(openSession!);
    await expect(onOpen).toHaveBeenCalledWith(
      STORY_GATEWAYS[0],
      STORY_SESSION_ROW.id,
    );
  },
};

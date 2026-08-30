import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect } from "storybook/test";

import {
  STORY_SESSION_ROW,
  STORY_SESSION_USAGE,
} from "../dev/story-data";
import { SessionStatsPanel } from "./SessionList";

const meta = {
  title: "Session/Navigator stats",
  component: SessionStatsPanel,
  parameters: { layout: "padded" },
  args: {
    session: STORY_SESSION_ROW,
    usage: STORY_SESSION_USAGE,
    phase: "ready",
  },
} satisfies Meta<typeof SessionStatsPanel>;

export default meta;
type Story = StoryObj<typeof meta>;

/** A mixed exact/estimated sample says so in both the number and its explanation. */
export const EstimatedReuse: Story = {
  play: async ({ canvas }) => {
    await expect(canvas.getByText("≈98%")).toBeVisible();
    await expect(
      canvas.getByText(/Estimated share of reusable prior input/),
    ).toBeVisible();
  },
};

import type { Meta, StoryObj } from "@storybook/react-vite";

import {
  STORY_QUEUED_TURNS,
  STORY_QUEUE_CLIENT,
  STORY_QUEUE_PAUSED,
} from "../dev/story-data";
import { QueuedTurnsTray } from "./QueuedTurnsTray";

const meta = {
  title: "Session/Queued turns",
  component: QueuedTurnsTray,
  parameters: { layout: "centered" },
  args: {
    client: STORY_QUEUE_CLIENT,
    sid: "session-preview",
    queued: STORY_QUEUED_TURNS,
    paused: STORY_QUEUE_PAUSED,
    onError: () => {},
  },
  render: (args) => (
    <div className="w-[min(24rem,calc(100vw-2rem))]">
      <QueuedTurnsTray {...args} />
    </div>
  ),
} satisfies Meta<typeof QueuedTurnsTray>;

export default meta;
type Story = StoryObj<typeof meta>;

export const PausedWithTwoTurns: Story = {};

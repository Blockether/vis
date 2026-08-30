import type { Meta, StoryObj } from "@storybook/react-vite";

import { STORY_SESSION } from "../dev/story-data";
import { SessionHeader } from "./SessionHeader";

const meta = {
  title: "Session/Header",
  component: SessionHeader,
  parameters: { layout: "fullscreen" },
  args: {
    model: {
      title: STORY_SESSION.title,
      sessionId: STORY_SESSION.id,
      connected: true,
      artifacts: { count: 4, isOpen: false },
    },
    commands: { back: () => {}, toggleArtifacts: () => {} },
  },
} satisfies Meta<typeof SessionHeader>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Connected: Story = {};

export const Reconnecting: Story = {
  args: {
    model: {
      title: STORY_SESSION.title,
      sessionId: STORY_SESSION.id,
      connected: false,
      artifacts: { count: 4, isOpen: false },
    },
  },
};

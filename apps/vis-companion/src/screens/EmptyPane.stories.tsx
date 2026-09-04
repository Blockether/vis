import type { Meta, StoryObj } from "@storybook/react-vite";

import { EmptyPane } from "./EmptyPane";

const meta = {
  title: 'Screens/Empty pane',
  component: EmptyPane,
  parameters: { layout: "fullscreen" },
} satisfies Meta<typeof EmptyPane>;

export default meta;
type Story = StoryObj<typeof meta>;

export const Desk: Story = {};

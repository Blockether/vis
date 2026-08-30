import type { Meta, StoryObj } from "@storybook/react-vite";
import { expect, fn, userEvent, within } from "storybook/test";

import { STORY_RESPONSE_CONTROL_VALUES } from "../dev/story-data";
import { ComposerResponseControls } from "./ComposerResponseControls";

const meta = {
  title: "Session/Composer response controls",
  component: ComposerResponseControls,
  parameters: { layout: "centered" },
  args: {
    controls: {
      model: { ...STORY_RESPONSE_CONTROL_VALUES.model, choose: fn() },
      reasoning: {
        ...STORY_RESPONSE_CONTROL_VALUES.reasoning,
        busy: false,
        cycle: fn(),
      },
      verbosity: {
        ...STORY_RESPONSE_CONTROL_VALUES.verbosity,
        busy: false,
        cycle: fn(),
      },
      fast: {
        ...STORY_RESPONSE_CONTROL_VALUES.fast,
        busy: false,
        toggle: fn(),
      },
    },
  },
} satisfies Meta<typeof ComposerResponseControls>;

export default meta;
type Story = StoryObj<typeof meta>;

export const AvailableOptions: Story = {
  name: "All provider options",
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    await userEvent.click(
      canvas.getByRole("button", { name: "Change provider and model" }),
    );
    await expect(args.controls.model.choose).toHaveBeenCalledOnce();
  },
};

import type { Meta, StoryObj } from "@storybook/react-vite";

import {
  STORY_FILE_SUGGESTIONS,
  STORY_SLASH_COMMANDS,
} from "../dev/story-data";
import { ComposerSuggestions } from "./ComposerSuggestions";

const meta = {
  title: "Session/Composer suggestions",
  component: ComposerSuggestions,
  parameters: { layout: "fullscreen" },
  decorators: [
    (Story) => (
      <div className="relative mx-auto h-dvh max-w-[46rem]">
        <div className="absolute inset-x-0 bottom-12 h-px">
          <Story />
        </div>
      </div>
    ),
  ],
} satisfies Meta<typeof ComposerSuggestions>;

export default meta;
type Story = StoryObj;

export const FileMentions: Story = {
  name: "File mentions",
  render: () => (
    <ComposerSuggestions
      kind="files"
      items={STORY_FILE_SUGGESTIONS}
      selectedIndex={0}
      onSelect={() => {}}
    />
  ),
};

export const SlashCommands: Story = {
  name: "Slash commands",
  render: () => (
    <ComposerSuggestions
      kind="slashes"
      items={STORY_SLASH_COMMANDS}
      selectedIndex={1}
      onSelect={() => {}}
    />
  ),
};

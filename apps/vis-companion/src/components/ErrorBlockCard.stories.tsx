import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect } from 'storybook/test';

import { STORY_PROVIDER_ERRORS } from '../dev/story-data';
import { ErrorBlockCard } from './ChatContent';

const failure = (kind: string) => {
  const block = STORY_PROVIDER_ERRORS.find((one) => one.kind === kind);
  if (!block) throw new Error(`Missing provider failure story: ${kind}`);
  return block;
};

const meta = {
  title: 'Session/Transcript/Provider failure',
  component: ErrorBlockCard,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <main className="mx-auto w-full max-w-3xl px-3.5 py-6 sm:px-6">
        <Story />
      </main>
    ),
  ],
  args: { block: failure('rate-limit') },
} satisfies Meta<typeof ErrorBlockCard>;

export default meta;
type Story = StoryObj<typeof meta>;

/** The complete engine taxonomy, compared on one deterministic sheet. */
export const AllKinds: Story = {
  render: () => (
    <div className="grid gap-5">
      {STORY_PROVIDER_ERRORS.map((block) => (
        <ErrorBlockCard key={block.id} block={block} />
      ))}
    </div>
  ),
  play: async ({ canvas }) => {
    await expect(canvas.getAllByRole('alert')).toHaveLength(STORY_PROVIDER_ERRORS.length);
  },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_RESPONSE_CONTROL_VALUES } from '../dev/story-data';
import { ComposerResponseControls } from './ComposerResponseControls';

const meta = {
  title: 'Session/Composer response controls',
  component: ComposerResponseControls,
  parameters: { layout: 'centered' },
  // Leave room above the standalone strip for its extended touch targets.
  decorators: [
    (Story) => (
      <div className="pt-2">
        <Story />
      </div>
    ),
  ],
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
  name: 'All provider options',
  play: async ({ canvasElement, args }) => {
    const canvas = within(canvasElement);
    const buttons = canvas.getAllByRole('button');
    // Desktop settings sit closer to the input; mobile keeps its 44px touch reach.
    const row = buttons[0].parentElement!;
    const dividers = row.querySelectorAll(':scope > [aria-hidden="true"]');
    await expect(dividers.length).toBe(buttons.length - 1);
    await userEvent.click(canvas.getByRole('button', { name: 'Change provider and model' }));
    await expect(args.controls.model.choose).toHaveBeenCalledOnce();
    await userEvent.click(canvas.getByRole('button', { name: /^Verbosity —/ }));
    await expect(args.controls.verbosity?.cycle).toHaveBeenCalledOnce();
  },
};

export const AvailableOptionsPointer: Story = {
  ...AvailableOptions,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

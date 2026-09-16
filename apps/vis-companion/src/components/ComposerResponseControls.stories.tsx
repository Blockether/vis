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
    await canvasElement.ownerDocument.fonts.ready;
    const buttons = canvas.getAllByRole('button');
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    // Desktop settings sit closer to the input; mobile keeps its 44px touch reach.
    const row = buttons[0].parentElement!;
    await expect(getComputedStyle(row).paddingTop).toBe(pointer ? '2px' : '4px');
    await expect(getComputedStyle(row).columnGap).toBe(pointer ? '6px' : '4px');
    const dividers = row.querySelectorAll(':scope > [aria-hidden="true"]');
    await expect(dividers.length).toBe(buttons.length - 1);
    for (const divider of dividers) {
      await expect(divider.getBoundingClientRect().height).toBe(pointer ? 8 : 10);
    }
    for (const [index, button] of buttons.entries()) {
      const box = button.getBoundingClientRect();
      await expect(box.height).toBe(pointer ? 28 : 32);
      await expect(getComputedStyle(button).fontSize).toBe('10px');
      await expect(getComputedStyle(button).letterSpacing).toBe('normal');
      const icon = button.querySelector('svg');
      if (icon) {
        await expect(icon.getBoundingClientRect().width).toBe(pointer ? 10 : 12);
        await expect(icon.getBoundingClientRect().height).toBe(pointer ? 10 : 12);
      }
      if (index > 0) {
        await expect(
          box.left - buttons[index - 1].getBoundingClientRect().right,
        ).toBeGreaterThanOrEqual(8);
      }
      if (!pointer) {
        for (const y of [box.top - 5, box.bottom + 5]) {
          await expect(
            button.contains(button.ownerDocument.elementFromPoint(box.left + box.width / 2, y)),
          ).toBe(true);
        }
      }
    }
    await userEvent.click(canvas.getByRole('button', { name: 'Change provider and model' }));
    await expect(args.controls.model.choose).toHaveBeenCalledOnce();
    await userEvent.click(canvas.getByRole('button', { name: /^Verbosity —/ }));
    await expect(args.controls.verbosity?.cycle).toHaveBeenCalledOnce();
  },
};

export const AvailableOptionsPointer: Story = {
  ...AvailableOptions,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

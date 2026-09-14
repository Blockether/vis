import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_PICTURES } from '../dev/story-data';
import { ImageViewer } from './ImageViewer';

/** The full-screen image tool, with the drawing state changed in its own footer slot. */
const meta = {
  title: 'Components/Image viewer',
  component: ImageViewer,
  args: {
    src: STORY_PICTURES[0].src,
    name: STORY_PICTURES[0].name,
    onClose: fn(),
    onApply: fn(),
  },
} satisfies Meta<typeof ImageViewer>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Entering Draw replaces its pencil with the check that accepts the drawing. */
export const Drawing: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(page.getByRole('button', { name: 'Draw on image' }));
    await expect(page.queryByRole('button', { name: 'Draw on image' })).not.toBeInTheDocument();
    await expect(page.getByRole('button', { name: 'Save changes' })).toBeInTheDocument();
    const rail = page.getByRole('group', { name: 'Drawing tools' });
    const buttons = within(rail).getAllByRole('button');
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const pitch = pointer ? 36 : 52;
    for (let index = 1; index < buttons.length; index += 1) {
      const previous = buttons[index - 1].getBoundingClientRect();
      const current = buttons[index].getBoundingClientRect();
      await expect(current.y + current.height / 2 - previous.y - previous.height / 2).toBe(pitch);
    }
  },
};

/** The pen stays active while its rail folds down to a single way back. */
export const DrawingToolsHidden: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(page.getByRole('button', { name: 'Draw on image' }));
    await userEvent.click(page.getByRole('button', { name: 'Hide drawing tools' }));
    await expect(page.queryByRole('group', { name: 'Drawing tools' })).not.toBeInTheDocument();
    await expect(page.getByRole('button', { name: 'Show drawing tools' })).toBeInTheDocument();
  },
};

/** Regression: zoom controls keep their targets without borders or circular faces. */
export const ZoomControls: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const zoomIn = page.getByRole('button', { name: 'Zoom in' });
    const zoomOut = page.getByRole('button', { name: 'Zoom out' });
    for (const button of [zoomOut, zoomIn]) {
      const assertUnframed = async () => {
        const box = button.getBoundingClientRect();
        const style = getComputedStyle(button);
        await expect(box.width).toBe(box.height);
        await expect(box.width).toBeGreaterThanOrEqual(28);
        await expect(parseFloat(style.borderRadius)).toBe(0);
        for (const side of ['Top', 'Right', 'Bottom', 'Left'] as const) {
          await expect(parseFloat(style[`border${side}Width`])).toBe(0);
        }
      };
      await assertUnframed();
      await userEvent.hover(button);
      await assertUnframed();
      await userEvent.unhover(button);
      if (!matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
        const box = button.getBoundingClientRect();
        for (const y of [box.top - 5, box.bottom + 5]) {
          const hit = button.ownerDocument.elementFromPoint(box.left + box.width / 2, y);
          await expect(button.contains(hit)).toBe(true);
        }
      }
    }
    // Invisible reach counts as part of the target, including between toolbar groups.
    const toolbar = zoomIn.closest('[role="group"]')!.parentElement!;
    const targets = [...toolbar.querySelectorAll('button')].map((button) => {
      const box = button.getBoundingClientRect();
      const reach = getComputedStyle(button, '::after');
      const width =
        reach.content === 'none' ? box.width : Math.max(box.width, parseFloat(reach.width));
      return { left: box.x + (box.width - width) / 2, right: box.x + (box.width + width) / 2 };
    });
    for (let index = 1; index < targets.length; index += 1) {
      await expect(targets[index].left - targets[index - 1].right).toBeGreaterThanOrEqual(8);
    }
    const reset = page.getByRole('button', { name: 'Reset zoom' });
    await userEvent.click(zoomIn);
    await expect(reset).toHaveTextContent('135%');
    await userEvent.click(zoomOut);
    await expect(reset).toHaveTextContent('100%');
    await userEvent.click(zoomIn);
    await userEvent.click(reset);
    await expect(reset).toHaveTextContent('100%');
  },
};

export const DrawingPointer: Story = {
  ...Drawing,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

export const ZoomControlsPointer: Story = {
  ...ZoomControls,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

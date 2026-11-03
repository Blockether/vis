import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fireEvent, userEvent, within } from 'storybook/test';

import { CARD_AFTER_PNG, CARD_BEFORE_PNG, CARD_SIZE } from '../dev/story-pictures';
import { liveViewFromWire, type LiveView } from '../lib/live-view';
import { LiveViewPanel } from './LiveView';

/**
 * The `image` node of an app: one picture that keeps its box while it loads, and
 * a comparison of the old and the new picture, side by side or with a swipe line.
 */
function pictures(node: Record<string, unknown>): LiveView {
  const view = liveViewFromWire({
    id: 'app-pictures',
    title: 'Screenshot review',
    placement: 'attachment',
    seq: 1,
    nodes: [{ id: 'shot', type: 'image', label: 'Settings card', alt: 'The settings card', ...node }],
  });
  if (!view) throw new Error('the picture fixture must be paintable');
  return view;
}

const AFTER = { media_type: 'image/png', data: CARD_AFTER_PNG, ...CARD_SIZE };
const BEFORE = { media_type: 'image/png', data: CARD_BEFORE_PNG, ...CARD_SIZE };

const meta = {
  title: 'Session/Artifacts/Review images',
  component: LiveViewPanel,
  parameters: { layout: 'padded' },
  args: { view: pictures(AFTER), inDialog: true },
} satisfies Meta<typeof LiveViewPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** One picture: it fits the width and keeps its aspect ratio; a press opens the viewer. */
export const Single: Story = {};

/** The old and the new picture next to each other, under their labels. */
export const CompareSideBySide: Story = { args: { view: pictures({ ...AFTER, before: BEFORE }) } };

/** The swipe line over both pictures; the slider moves it with a drag or the arrow keys. */
export const CompareSwipe: Story = {
  args: { view: pictures({ ...AFTER, before: BEFORE }) },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    await userEvent.click(canvas.getByRole('button', { name: /Swipe/ }));
    const line = canvas.getByRole('slider', { name: 'Swipe line of The settings card' });
    await expect(line).toHaveAttribute('aria-valuetext', '50% before');
    // jsdom does not step a range on arrow keys; a browser does. A drag sends the same change.
    fireEvent.change(line, { target: { value: '30' } });
    await expect(line).toHaveAttribute('aria-valuetext', '30% before');
  },
};

/** A picture whose data the host left out: its facts in its box, no preview. */
export const WithoutData: Story = {
  args: { view: pictures({ media_type: 'image/png', bytes: 48211, ...CARD_SIZE }) },
};

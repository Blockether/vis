import type { Meta, StoryObj } from '@storybook/react-vite';

import {
  STORY_CANVAS_COMPLEX,
  STORY_CANVAS_FOCUSED,
  STORY_CANVAS_GALLERY,
  STORY_CANVAS_SHOWCASE,
} from '../dev/canvas-stories';
import { LiveViewPanel } from './LiveView';

/**
 * A BOARD OF ITEMS AND ARROWS. The showcase is the canvas of the shared contract
 * fixture, so a wire change shows here first. The gallery draws each word of the
 * vocabulary one time.
 */
const meta = {
  title: 'Session/Live view/Canvas',
  component: LiveViewPanel,
  parameters: { layout: 'padded' },
  args: { view: STORY_CANVAS_SHOWCASE, onSelect: () => {} },
} satisfies Meta<typeof LiveViewPanel>;

export default meta;

type Story = StoryObj<typeof meta>;

/** A real board: frames in frames, shapes, a note, comments, a picture and media. */
export const Showcase: Story = {};

/** The camera of the view: full size, with one item in the middle. */
export const Focused: Story = {
  args: { view: STORY_CANVAS_FOCUSED },
};

/** A large board: many frames, and arrows that go around them. */
export const Complex: Story = {
  args: { view: STORY_CANVAS_COMPLEX },
};

/** Each shape, colour, fill, border, kind, size, mark, route, motion and pattern. */
export const Gallery: Story = {
  args: { view: STORY_CANVAS_GALLERY },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';

import {
  STORY_CANVAS_COMPLEX,
  STORY_CANVAS_FOCUSED,
  STORY_CANVAS_GALLERY,
  STORY_CANVAS_SHOWCASE,
} from '../dev/canvas-stories';
import { applyLivePatch, liveThreads, type LiveView } from '../lib/live-view';
import type { AppNodeActions } from './AppNodes';
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

/** A real board: frames in frames, shapes, a note, two threads, a picture and media. */
export const Showcase: Story = {};

/** The camera of the view: full size, with one item in the middle. */
export const Focused: Story = {
  args: { view: STORY_CANVAS_FOCUSED },
};

/**
 * A board that keeps each draft as a host does, so a person can write a comment on an
 * item, reply in a thread and remove a draft.
 */
function Commented({ view: first }: { view: LiveView }) {
  const [view, setView] = useState(first);
  const app: AppNodeActions = {
    isReadOnly: false,
    draft: async (nodeId, { threadId, itemId, body }) => {
      const node = view.nodes.find((one) => one.id === nodeId);
      const held = node ? liveThreads(node).find((thread) => thread.id === threadId) : undefined;
      const id = threadId ?? `story-${view.seq + 1}`;
      const text = body?.trim() ? body : undefined;
      const op =
        !text && (held?.messages.length ?? 0) === 0
          ? { op: 'remove', node_id: nodeId, item_ids: [id] }
          : {
              op: 'thread',
              node_id: nodeId,
              thread: {
                id,
                item_id: held?.item_id ?? itemId,
                messages: held?.messages ?? [],
                ...(held?.is_resolved === undefined ? {} : { is_resolved: held.is_resolved }),
                ...(text ? { draft: text } : {}),
              },
            };
      setView(applyLivePatch(view, { patch: { seq: view.seq + 1, ops: [op] } }));
      return { status: 'saved', seq: view.seq + 1, threadId: id };
    },
  };
  return <LiveViewPanel view={view} app={app} onSelect={() => {}} />;
}

/** The threads of the items, with an app: a person comments, replies and removes a draft. */
export const Comments: Story = {
  render: (args) => <Commented view={args.view as LiveView} />,
};

/** A large board: many frames, and arrows that go around them. */
export const Complex: Story = {
  args: { view: STORY_CANVAS_COMPLEX },
};

/** Each shape, colour, fill, border, kind, size, mark, route, motion and pattern. */
export const Gallery: Story = {
  args: { view: STORY_CANVAS_GALLERY },
};

import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_GATEWAYS, STORY_SESSION, STORY_SESSION_ROW } from '../dev/story-data';
import { EMPTY_DRAFT_MESSAGE } from '../lib/draft-messages';
import { SessionRow } from './SessionList';

const onOpen = fn();
const commands = {
  open: onOpen,
  rename: fn(async () => {}),
  fork: fn(async () => {}),
  requestDelete: fn(),
  toggleStar: fn(),
};

const meta = {
  title: 'Session list/Row',
  component: SessionRow,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story) => (
      <div className="@container">
        <Story />
      </div>
    ),
  ],
  args: {
    session: STORY_SESSION_ROW,
    group: null,
    draft: EMPTY_DRAFT_MESSAGE,
    conn: STORY_GATEWAYS[0],
    needle: '',
    commands,
    deletion: null,
  },
} satisfies Meta<typeof SessionRow>;

export default meta;
type Story = StoryObj<typeof meta>;

export const AwaitingInput: Story = {
  play: async ({ canvas }) => {
    const openSession = canvas.getByText(STORY_SESSION.title).closest('button');
    await expect(openSession).not.toBeNull();
    await userEvent.click(openSession!);
    await expect(onOpen).toHaveBeenCalledWith(STORY_GATEWAYS[0], STORY_SESSION_ROW.id);
  },
};

export const Renaming: Story = {
  play: async ({ canvas, canvasElement }) => {
    const pointer = canvasElement.ownerDocument.defaultView!.matchMedia(
      '(min-width: 640px) and (pointer: fine)',
    ).matches;
    if (pointer) {
      await userEvent.click(
        canvas.getByRole('button', { name: `Actions for ${STORY_SESSION_ROW.title}` }),
      );
      const menu = within(canvasElement.ownerDocument.body).getByRole('dialog');
      await userEvent.click(within(menu).getByRole('button', { name: 'Rename' }));
    } else {
      const actions = canvas.getByRole('group', { name: `${STORY_SESSION_ROW.title} actions` });
      const rename = within(actions).getByRole('button', { name: 'Rename' });
      rename.focus();
      await userEvent.keyboard('{Enter}');
    }
    await expect(
      await canvas.findByRole('textbox', {
        name: `Rename ${STORY_SESSION_ROW.title}`,
      }),
    ).toHaveValue(STORY_SESSION_ROW.title);
    await expect(canvas.getByText(STORY_SESSION_ROW.id)).toBeInTheDocument();
    await expect(canvas.getByText(`${STORY_SESSION_ROW.turn_count} turns`)).toBeInTheDocument();
    await expect(canvas.getByText('HITL')).toBeInTheDocument();
    await expect(
      canvas.getByRole('button', {
        name: `Show details for ${STORY_SESSION_ROW.title}`,
      }),
    ).toBeInTheDocument();
    await expect(
      canvas.queryByRole('group', {
        name: `${STORY_SESSION_ROW.title} actions`,
      }),
    ).not.toBeInTheDocument();
    await expect(canvas.queryByRole('dialog')).not.toBeInTheDocument();
  },
};

/**
 * Regression, user report: the two answers stood their own 48px floor while the row
 * they replace stands 52 on a phone — metadata stacks under the title there — so the
 * list lost four pixels the moment the question appeared. The confirmation stands
 * exactly what the row stood, under a finger and under a pointer.
 */
export const Deleting: Story = {
  render: function DeletingRow(args) {
    const [asking, setAsking] = useState(false);
    return (
      <SessionRow
        {...args}
        commands={{ ...args.commands, requestDelete: () => setAsking(true) }}
        deletion={
          asking
            ? { isBusy: false, error: null, confirm: fn(), cancel: () => setAsking(false) }
            : null
        }
      />
    );
  },
  play: async ({ canvas, canvasElement }) => {
    const win = canvasElement.ownerDocument.defaultView!;
    if (win.matchMedia('(min-width: 640px) and (pointer: fine)').matches) {
      await userEvent.click(
        canvas.getByRole('button', { name: `Actions for ${STORY_SESSION_ROW.title}` }),
      );
      const menu = within(canvasElement.ownerDocument.body).getByRole('dialog');
      await userEvent.click(within(menu).getByRole('button', { name: 'Delete' }));
    } else {
      const actions = canvas.getByRole('group', { name: `${STORY_SESSION_ROW.title} actions` });
      within(actions).getByRole('button', { name: 'Delete' }).focus();
      await userEvent.keyboard('{Enter}');
    }
    const question = await canvas.findByRole('group', {
      name: `Delete ${STORY_SESSION_ROW.title}?`,
    });
    await expect(question).toBeVisible();
  },
};

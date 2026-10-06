import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';

import { STORY_SESSION_SEARCH_MATCH } from '../dev/story-data';
import { SearchMessages } from './SearchMessages';
import { SessionSearchDialog } from './SessionSearchDialog';
import type { ForkPoint } from '../lib/types';

/** The newest turns of the previewed session, as the gateway lists them: oldest first. */
const RECENT_TURNS: ForkPoint[] = [
  { turn_id: 't1', request: 'Run the Windows checks', answer: 'The **Windows** runtime checks pass.', created_at: 1_717_200_000_000 },
  { turn_id: 't2', request: 'Now check macOS too', created_at: 1_717_200_600_000 },
];

/** The search stands over the app in its own dialog; the list behind it keeps its rows. */
const meta = {
  title: 'Session/Search dialog',
  component: SessionSearchDialog,
  parameters: { layout: 'fullscreen' },
  args: {
    query: '',
    onQuery: fn(),
    onClose: fn(),
    results: (
      <p className="px-5 py-16 text-center font-mono text-ui text-dialog-hint">
        Type to search titles and messages.
      </p>
    ),
  },
} satisfies Meta<typeof SessionSearchDialog>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Opening puts the caret in the field, and Escape leaves the dialog. */
export const Empty: Story = {
  play: async ({ args, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const dialog = page.getByRole('dialog', { name: 'Search sessions' });
    await expect(within(dialog).getByRole('searchbox', { name: 'Search session titles and messages' })).toHaveFocus();

    await userEvent.keyboard('{Escape}');
    await expect(args.onClose).toHaveBeenCalledOnce();
  },
};

/** On desktop, recent sessions stay beside the message preview before you type. */
export const Recents: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  args: {
    results: (
      <ul className="px-3 py-3">
        <li className="font-mono text-body text-white">Windows runtime checks</li>
      </ul>
    ),
    messages: (
      <SearchMessages
        title="Windows runtime checks"
        match={null}
        query=""
        isSearching={false}
        recent={RECENT_TURNS}
        onOpen={fn()}
        className="min-h-0 flex-1"
      />
    ),
  },
  play: async ({ canvasElement }) => {
    const dialog = within(within(canvasElement.ownerDocument.body).getByRole('dialog', { name: 'Search sessions' }));
    await expect(dialog.getByRole('region', { name: 'Recent sessions' })).toHaveTextContent('Windows runtime checks');
    const pane = within(dialog.getByRole('region', { name: 'Matching messages' }));
    await expect(pane.getByRole('heading')).toHaveTextContent('Windows runtime checks');
    await waitFor(() => expect(pane.getByText('Now check macOS too')).toBeVisible());
    await expect(pane.getByText('Run the Windows checks')).toBeVisible();
  },
};

/** Found sessions beside the messages that matched in the picked one. */
export const WithMessages: Story = {
  args: {
    query: 'windows',
    scope: (
      <span className="font-mono text-chip font-bold text-accent-ink">1 match</span>
    ),
    results: (
      <ul className="px-3 py-3">
        <li className="font-mono text-body text-white">Windows runtime checks</li>
      </ul>
    ),
    messages: (
      <SearchMessages
        title="Windows runtime checks"
        match={STORY_SESSION_SEARCH_MATCH}
        query="windows"
        isSearching={false}
        onOpen={fn()}
        className="min-h-0 flex-1"
      />
    ),
  },
  play: async ({ args, canvasElement }) => {
    const dialog = within(within(canvasElement.ownerDocument.body).getByRole('dialog', { name: 'Search sessions' }));
    await expect(dialog.getByRole('region', { name: 'Matching sessions' })).toHaveTextContent('Windows runtime checks');
    await expect(dialog.getByRole('region', { name: 'Matching messages' })).toBeInTheDocument();

    await userEvent.click(dialog.getByRole('button', { name: 'Clear search' }));
    await expect(args.onQuery).toHaveBeenCalledWith('');
    await userEvent.click(dialog.getByRole('button', { name: 'Close search' }));
    await expect(args.onClose).toHaveBeenCalledOnce();
  },
};

/** On a desk the two answers stand side by side in a wide box. */
export const DesktopWithMessages: Story = {
  ...WithMessages,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

// This regression needs browser layout to check responsive visibility and available space.
export const MobilePreviewVisibility: Story = {
  args: Recents.args,
  tags: ['!test'],
  globals: { viewport: { value: 'phone', isRotated: false } },
  render: function Render(args) {
    const [query, setQuery] = useState(args.query);
    return (
      <SessionSearchDialog
        {...args}
        query={query}
        onQuery={setQuery}
        messages={
          <SearchMessages
            title="Windows runtime checks"
            match={query.trim() ? STORY_SESSION_SEARCH_MATCH : null}
            query={query}
            isSearching={false}
            recent={RECENT_TURNS}
            onOpen={fn()}
            className="min-h-0 flex-1"
          />
        }
      />
    );
  },
  play: async ({ canvasElement }) => {
    const dialog = within(within(canvasElement.ownerDocument.body).getByRole('dialog', { name: 'Search sessions' }));
    const field = dialog.getByRole('searchbox', { name: 'Search session titles and messages' });
    const pane = dialog.getByLabelText('Matching messages');
    const list = dialog.getByRole('region', { name: 'Recent sessions' });
    const expectHiddenPreview = async () => {
      await expect(pane).not.toBeVisible();
      await expect(Math.abs(list.getBoundingClientRect().bottom - list.parentElement!.getBoundingClientRect().bottom)).toBeLessThan(1);
    };

    await waitFor(() => expect(field).toBeVisible());
    await expectHiddenPreview();
    const fullHeight = list.getBoundingClientRect().height;
    await userEvent.type(field, 'w');
    await expect(pane).toBeVisible();
    await expect(list.getBoundingClientRect().height).toBeLessThan(fullHeight);

    await userEvent.click(dialog.getByRole('button', { name: 'Clear search' }));
    await expectHiddenPreview();
    await expect(field).toHaveFocus();

    await userEvent.type(field, 'w');
    await expect(pane).toBeVisible();
    await userEvent.keyboard('{Backspace}');
    await expectHiddenPreview();

    await userEvent.type(field, '   ');
    await expectHiddenPreview();
    await userEvent.clear(field);
  },
};

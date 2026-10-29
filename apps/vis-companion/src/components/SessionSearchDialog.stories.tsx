import type { Meta, StoryObj } from '@storybook/react-vite';
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
  title: 'Session list/Search/Dialog',
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

import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_SESSION_SEARCH_MATCH } from '../dev/story-data';
import { SearchMessages } from './SearchMessages';
import { SessionSearchDialog } from './SessionSearchDialog';

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
    await expect(within(dialog).getByRole('searchbox', { name: 'Search sessions on every machine' })).toHaveFocus();
    await expect(within(dialog).queryByRole('region', { name: 'Matching messages' })).toBeNull();

    await userEvent.keyboard('{Escape}');
    await expect(args.onClose).toHaveBeenCalledOnce();
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

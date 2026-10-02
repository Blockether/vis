import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import type { ComponentProps } from 'react';
import type { SettingsResponse, SettingsTarget } from '../../lib/types';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';

const target: SettingsTarget = { scope: 'group', target_id: 'wallet', label: 'Wallet work' };
const settings: SettingsResponse = {
  revision: 'wallet-1',
  scope: 'group', target_id: 'wallet',
  groups: [
    { id: 'agent', title: 'Agent', toggles: [
      { id: 'plans', label: 'Plans', description: 'Plan work before changing files.',
        type: 'boolean', enabled: true, scopes: ['global', 'group'], source: 'group', is_override: true },
      { id: 'summary', label: 'Summaries', description: 'Show a short summary after each turn.',
        type: 'boolean', enabled: false, scopes: ['global', 'group'], source: 'global', is_override: false },
    ] },
    { id: 'experimental', title: 'Experimental', toggles: [
      { id: 'draft_backend', label: 'Draft backend', description: 'Try isolated drafts for changes.',
        type: 'boolean', enabled: false, scopes: ['global', 'group'], source: 'global', is_override: false },
    ] },
  ],
};
function fixtureClient(catalog: SettingsResponse): ComponentProps<typeof ScopedSettingsDialog>['client'] {
  return {
    cachedSettings: () => catalog,
    settings: async () => catalog,
    cachedMcpServers: () => [],
    mcpServers: async () => [],
  } as unknown as ComponentProps<typeof ScopedSettingsDialog>['client'];
}

const client = fixtureClient(settings);

const meta = {
  title: 'Screens/Scoped settings dialog',
  component: ScopedSettingsDialog,
  args: { client, target, onClose: () => {} },
} satisfies Meta<typeof ScopedSettingsDialog>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Adjacent sections should have one continuous rule, even before MCP servers. */
export const Catalog: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const panel = (title: string) => page.getByRole('heading', { name: title }).closest('section');
    await expect(panel('Agent')).toBeVisible();
    await expect(panel('Experimental')).toBeVisible();
    await expect(panel('MCP servers')).toBeVisible();
    const sections = panel('Agent')?.parentElement;
    await expect(sections).toHaveClass('divide-y', 'divide-dialog-edge');
    await expect(Array.from(sections?.children ?? [])).toEqual([
      panel('Agent'), panel('Experimental'), panel('Extensions'), panel('MCP servers'),
    ]);
  },
};

/** A search with no matches keeps the query, a way back, and a compact result. */
export const NoMatches: Story = {
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.type(page.getByRole('searchbox', { name: 'Search settings' }), 'dd');
    await expect(page.getByRole('heading', { name: 'No settings match “dd”' })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Clear search' })).toBeVisible();
  },
};

/** A long catalog still scrolls under the search field rather than overflowing the viewport. */
export const LongCatalog: Story = {
  tags: ['!test'],
  args: {
    client: fixtureClient({
      ...settings,
      groups: [{ ...settings.groups[0], toggles: Array.from({ length: 24 }, (_, index) => ({
        ...settings.groups[0].toggles[0], id: `option-${index}`, label: `Option ${index + 1}`,
      })) }],
    }),
  },
};

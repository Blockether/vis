import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
import type { ComponentProps } from 'react';
import type { SettingsResponse, SettingsTarget, Toggle } from '../../lib/types';
import { ScopedSettingsDialog } from './ScopedSettingsDialog';
import { OpenBands } from '../../dev/OpenBands';

const target: SettingsTarget = { scope: 'group', target_id: 'wallet', label: 'Wallet work' };
const settings: SettingsResponse = {
  revision: 'wallet-1',
  scope: 'group', target_id: 'wallet',
  groups: [
    { id: 'agent', title: 'Agent', toggles: [
      { id: 'subagents', label: 'Subagents', description: 'Delegate work to managed agents.',
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
  title: 'Settings/Scoped settings',
  component: ScopedSettingsDialog,
  // These stories show what the bands hold, so every band starts open.
  decorators: [
    (Story) => (
      <OpenBands>
        <Story />
      </OpenBands>
    ),
  ],
  args: { client, target, onClose: () => {} },
} satisfies Meta<typeof ScopedSettingsDialog>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Adjacent sections should have one continuous rule, also around Tools. */
export const Default: Story = {
};

const engine = (name: string): Toggle => ({
  id: `engines_${name}`, label: name, description: 'Auto detects applicability; On stays active; Off denies tools.',
  type: 'enum', choices: ['auto', 'on', 'off'], value: 'auto', scopes: ['global', 'group'], source: 'global', is_override: false,
});

/** Every extension stands under Extensions once, with its install scope, never its file path. */
export const Extensions: Story = {
  args: {
    client: fixtureClient({
      ...settings,
      groups: [
        ...settings.groups,
        { id: 'extension:language-clojure', title: 'language-clojure',
          extension: { name: 'language-clojure', origin: 'built_in', status: 'loaded' }, toggles: [engine('language-clojure')] },
        { id: 'extension:vis-optmem', title: 'vis-optmem',
          extension: { name: 'vis-optmem', origin: 'global', path: '~/.vis/extensions/vis-optmem/0.1.0/extension.py', status: 'loaded' },
          toggles: [engine('vis-optmem')] },
        { id: 'extension:vis-spel', title: 'vis-spel',
          extension: { name: 'vis-spel', origin: 'global', path: '~/.vis/extensions/vis-spel/0.1.13/extension.py', status: 'loaded' },
          toggles: [engine('vis-spel'), { id: 'skills_browser', label: 'vis-spel/browser', type: 'boolean', enabled: true,
            scopes: ['global', 'group'], source: 'global', is_override: false }] },
        { id: 'extension:review.py', title: 'review.py',
          extension: { name: 'review.py', origin: 'project', path: '.vis/extensions/review.py', status: 'failed',
            error: 'SyntaxError: invalid syntax' }, toggles: [] },
      ],
    }),
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

/** A slow gateway: before the catalog arrives, one loading panel stands alone. */
export const Loading: Story = {
  args: {
    client: {
      cachedSettings: () => null,
      settings: () => new Promise<SettingsResponse>(() => {}),
      cachedMcpServers: () => null,
      mcpServers: () => new Promise(() => {}),
    } as unknown as ComponentProps<typeof ScopedSettingsDialog>['client'],
  },
};

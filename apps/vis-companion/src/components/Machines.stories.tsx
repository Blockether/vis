import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_GATEWAYS, STORY_GATEWAY_HEALTH } from '../dev/story-data';
import { MachineRows } from './Machines';

/** The paired fleet, including one machine whose latest probe failed. */
const meta = {
  title: 'Components/Machine rows',
  component: MachineRows,
  parameters: { layout: 'padded' },
  args: {
    conns: STORY_GATEWAYS,
    primaryUrl: STORY_GATEWAYS[0].url,
    health: STORY_GATEWAY_HEALTH,
    onPick: () => {},
    onRetry: fn(),
    actionLabel: 'Open',
  },
} satisfies Meta<typeof MachineRows>;

export default meta;
type Story = StoryObj<typeof meta>;

const pick = fn();

/** Rank, latency, long name and failure all compete in the same one-line rail. */
export const Fleet: Story = {
  args: { onPick: pick },
  play: async ({ args, canvas }) => {
    await userEvent.click(canvas.getByRole('button', { name: /tower/i }));
    await expect(args.onPick).toHaveBeenCalledWith(STORY_GATEWAYS[0]);
  },
};

/** One machine pays no width for fleet rank. */
export const OneMachine: Story = {
  args: {
    conns: STORY_GATEWAYS.slice(0, 1),
    health: { [STORY_GATEWAYS[0].url]: STORY_GATEWAY_HEALTH[STORY_GATEWAYS[0].url] },
  },
};

/** Offline rows retry without mounting their settings, even if previously opened. */
export const OfflineSettings: Story = {
  args: {
    actionLabel: undefined,
    openUrls: new Set([STORY_GATEWAYS[2].url]),
    renderPanel: (conn) => <p>Settings for {conn.label}</p>,
  },
  play: async ({ args, canvas }) => {
    const retry = canvas.getByRole('button', { name: 'Retry connection to mini' });
    await expect(retry).not.toHaveAttribute('aria-expanded');
    await expect(canvas.queryByText('Settings for mini')).not.toBeInTheDocument();
    await userEvent.click(retry);
    await expect(args.onRetry).toHaveBeenCalledWith(STORY_GATEWAYS[2]);
  },
};

/** The same row stays closed while its reload icon spins. */
export const CheckingSettings: Story = {
  args: {
    ...OfflineSettings.args,
    health: {
      ...STORY_GATEWAY_HEALTH,
      [STORY_GATEWAYS[2].url]: { state: 'checking', at: Number.MAX_SAFE_INTEGER },
    },
  },
  play: async ({ canvas }) => {
    const checking = canvas.getByRole('button', { name: 'Checking connection to mini' });
    await expect(checking).toHaveAttribute('aria-busy', 'true');
    await expect(checking).toHaveAttribute('aria-disabled', 'true');
    await expect(canvas.queryByText('Settings for mini')).not.toBeInTheDocument();
  },
};

/** The row already names the machine; its menu offers only the actions. */
export const ConciseActions: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  args: {
    conns: [{ url: 'http://127.0.0.1:61397', pinned: true }],
    primaryUrl: undefined,
    onMakePrimary: fn(),
    onRename: fn(),
    onSelectAddress: fn(),
    onForget: fn(),
  },
  play: async ({ canvas, canvasElement }) => {
    await userEvent.click(canvas.getByRole('button', { name: 'Actions for 127.0.0.1:61397' }));
    const menu = within(canvasElement.ownerDocument.body).getByRole('dialog', {
      name: '127.0.0.1:61397 actions',
    });
    await expect(within(menu).getAllByRole('button').map((button) => button.textContent)).toEqual([
      'Make primary',
      'Rename',
      'Bind to another address',
      'Forget',
    ]);
  },
};

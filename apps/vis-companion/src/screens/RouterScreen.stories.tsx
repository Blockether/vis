import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_ROUTER_CLIENT, STORY_SESSION } from '../dev/story-data';
import { ProviderRouterDialog } from './RouterScreen';

/**
 * THE MODEL PICKER AS THE SHIPPED DIALOG, not a header drawn beside a sample list.
 * Its gateway boundary is the deterministic client in `story-data`; everything from
 * the modal glass through the title band, scrolling body and provider rows is production.
 */
const meta = {
  title: 'Screens/Model picker',
  component: ProviderRouterDialog,
  parameters: { layout: 'fullscreen' },
  args: {
    client: STORY_ROUTER_CLIENT,
    sid: STORY_SESSION.id,
    onClose: fn(),
    onPicked: fn(),
    onManageProviders: fn(),
  },
} satisfies Meta<typeof ProviderRouterDialog>;

export default meta;

type Story = StoryObj<typeof meta>;

/** The body meets the title band on one edge, with compact chrome above the fleet. */
export const Fleet: Story = {
  play: async ({ args, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await expect(await page.findByRole('dialog', { name: 'Model' })).toBeVisible();
    await expect(page.getByText('Anthropic')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Refresh models' })).toBeVisible();
    const settings = page.getByRole('button', { name: 'Open provider settings' });
    await expect(settings).toBeVisible();
    await userEvent.click(settings);
    await expect(args.onManageProviders).toHaveBeenCalledOnce();
  },
};

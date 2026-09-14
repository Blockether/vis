import type { Meta, StoryObj } from '@storybook/react-vite';
import { useEffect, useState, type ReactNode } from 'react';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';
import { STORY_GATEWAYS, storySettingsFetch } from '../dev/story-data';
import { setThemePref } from '../lib/storage';
import { resolveTheme } from '../lib/theme';
import { SettingsDialog } from './SettingsScreen';

/** The real dialog over a fixture transport; preferences remain local to this preview. */
function StorySettings({ theme, children }: { theme: string; children: ReactNode }) {
  const [ready, setReady] = useState(false);
  useEffect(() => {
    let active = true;
    const previous = globalThis.fetch;
    globalThis.fetch = storySettingsFetch();
    void setThemePref(resolveTheme(theme).id).then(() => {
      if (active) setReady(true);
    });
    return () => {
      active = false;
      globalThis.fetch = previous;
    };
  }, [theme]);
  return ready ? children : null;
}

const meta = {
  title: 'Screens/Settings',
  component: SettingsDialog,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story, { globals }) => (
      <StorySettings key={String(globals.theme)} theme={String(globals.theme)}>
        <Story />
      </StorySettings>
    ),
  ],
  args: {
    gateways: STORY_GATEWAYS,
    primaryUrl: STORY_GATEWAYS[0].url,
    onAddMachine: fn(async () => {}),
    onMakePrimary: fn(),
    onRename: fn(async () => {}),
    onRemove: fn(),
    onSelectAddress: fn(),
    onClose: fn(),
  },
} satisfies Meta<typeof SettingsDialog>;
export default meta;
type Story = StoryObj<typeof meta>;

/** Machines lead; the application fold opens by the same control used in the app. */
export const Appearance: Story = {
  play: async ({ canvasElement, globals }) => {
    const page = within(canvasElement.ownerDocument.body);
    await expect(await page.findByRole('heading', { name: 'Settings' })).toBeVisible();
    const application = page.queryByRole('button', { name: 'Show application settings' });
    if (application) await userEvent.click(application);
    const theme = await page.findByRole('button', {
      name: resolveTheme(String(globals.theme)).label,
    });
    await waitFor(() => expect(theme).toHaveAttribute('aria-pressed', 'true'));
  },
};

/** A sole machine shows its full settings without a disclosure or an extra press. */
export const SingleMachine: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await expect(await page.findByText('MCP servers')).toBeVisible();
    await expect(page.getByText('Providers')).toBeVisible();
    const name = page.getByText('tower');
    await expect(name.closest('button')).toBeNull();
    await expect(name.closest('[aria-expanded]')).toBeNull();
    await expect(name.parentElement?.parentElement?.querySelector('.lucide-chevron-right')).toBeNull();
    await userEvent.click(name);
    await expect(page.getByText('MCP servers')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Add a machine' })).toBeVisible();
  },
};

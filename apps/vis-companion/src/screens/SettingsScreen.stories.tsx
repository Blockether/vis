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

/** Settings names and explanatory copy follow the desktop hierarchy without shrinking touch. */
export const ReadingLayout: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await page.findByText('MCP servers');
    const application = page.queryByRole('button', { name: 'Show application settings' });
    if (application) await userEvent.click(application);
    const dialog = page.getByRole('dialog', { name: 'Settings' });
    await dialog.ownerDocument.fonts.ready;
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const label = page.getByText('Show Python code', { exact: true });
    const title = page.getByRole('heading', { name: 'Settings', level: 2 });
    for (const [element, touchSize, touchLine] of [
      [title, '12px', '18px'],
      [label, '11px', '16px'],
      [page.getByRole('heading', { name: 'Machines' }), '11px', '16px'],
      [page.getByRole('heading', { name: 'Application' }), '11px', '16px'],
      [page.getByRole('heading', { name: 'Transcript' }), '8px', '14px'],
      [page.getByRole('heading', { name: 'Theme' }), '8px', '14px'],
    ] as const) {
      const style = getComputedStyle(element);
      await expect(style.fontFamily).toBe(getComputedStyle(title).fontFamily);
      await expect(style.fontSize).toBe(pointer ? '13px' : touchSize);
      await expect(style.lineHeight).toBe(pointer ? '20px' : touchLine);
    }
    const description = page.getByText(
      'One expandable source line before Activity. Hiding code keeps every activity and result.',
    );
    await expect(getComputedStyle(description).fontSize).toBe(pointer ? '12px' : '11px');
    await expect(getComputedStyle(description).lineHeight).toBe(pointer ? '18px' : '16px');
    const available = innerWidth >= 640 ? innerWidth - 32 : innerWidth;
    await expect(dialog.getBoundingClientRect().width).toBe(Math.min(pointer ? 1152 : 896, available));
    await expect(dialog.scrollWidth).toBe(dialog.clientWidth);
    const toggle = page.getByRole('switch', { name: /^Show Python code:/ });
    const checked = toggle.getAttribute('aria-checked');
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-checked', checked === 'true' ? 'false' : 'true');
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-checked', checked);
    const emptyServers = page.getByText('No MCP servers on this gateway.');
    await expect(getComputedStyle(emptyServers).fontSize).toBe(pointer ? '12px' : '10px');
    await expect(getComputedStyle(emptyServers).lineHeight).toBe(pointer ? '18px' : '16px');
    await userEvent.click(page.getByRole('button', { name: /^ASR/ }));
    const emptySpeech = await page.findByText('No ASR engine is registered on this machine.');
    await expect(getComputedStyle(emptySpeech).fontSize).toBe(pointer ? '12px' : '8px');
    await expect(getComputedStyle(emptySpeech).lineHeight).toBe(pointer ? '18px' : '14px');
  },
};

export const ReadingLayoutPointer: Story = {
  ...ReadingLayout,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** MCP textareas use the same control scale as inputs; labels and hints keep their own roles. */
export const FormTypography: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(await page.findByRole('button', { name: 'Add an MCP server' }));
    const name = await page.findByRole('textbox', { name: 'Server name' });
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const label = page.getByText('Server name', { exact: true });
    const hint = page.getByText('Arguments are passed directly, never through a shell.');
    await expect(getComputedStyle(label).fontSize).toBe(pointer ? '13px' : '8px');
    await expect(getComputedStyle(label).lineHeight).toBe(pointer ? '20px' : '14px');
    await expect(getComputedStyle(hint).fontSize).toBe(pointer ? '12px' : '8px');
    await expect(getComputedStyle(hint).lineHeight).toBe(pointer ? '18px' : '14px');
    const args = page.getByRole('textbox', { name: /^Arguments — one per line/ });
    const environment = page.getByRole('textbox', { name: /^Environment variables/ });
    for (const field of [args, environment]) {
      await expect(getComputedStyle(field).fontFamily).toBe(getComputedStyle(name).fontFamily);
      await expect(getComputedStyle(field).fontSize).toBe(pointer ? '11px' : '10px');
      await expect(getComputedStyle(field).lineHeight).toBe('16px');
    }
    await userEvent.type(args, '-y{Enter}server-filesystem');
    await expect(args).toHaveValue('-y\nserver-filesystem');
    await userEvent.click(page.getByRole('button', { name: 'Streamable HTTP' }));
    const headers = page.getByRole('textbox', { name: /^Headers/ });
    await expect(getComputedStyle(headers).fontSize).toBe(pointer ? '11px' : '10px');
    await expect(getComputedStyle(headers).lineHeight).toBe('16px');
  },
};

export const FormTypographyPointer: Story = {
  ...FormTypography,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

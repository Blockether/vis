import type { Meta, StoryObj } from '@storybook/react-vite';
import { useEffect, useState, type ReactNode } from 'react';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';
import { STORY_GATEWAYS, storySettingsFetch } from '../dev/story-data';
import { setThemePref } from '../lib/storage';
import { resolveTheme } from '../lib/theme';
import { SettingsDialog } from './SettingsScreen';

/** The real dialog over a fixture transport; preferences remain local to this preview. */
function StorySettings({
  theme,
  populated,
  children,
}: {
  theme: string;
  populated: boolean;
  children: ReactNode;
}) {
  const [ready, setReady] = useState(false);
  useEffect(() => {
    let active = true;
    const previous = globalThis.fetch;
    globalThis.fetch = storySettingsFetch(populated);
    void setThemePref(resolveTheme(theme).id).then(() => {
      if (active) setReady(true);
    });
    return () => {
      active = false;
      globalThis.fetch = previous;
    };
  }, [theme, populated]);
  return ready ? children : null;
}

const meta = {
  title: 'Screens/Settings',
  component: SettingsDialog,
  parameters: { layout: 'fullscreen' },
  decorators: [
    (Story, { globals, parameters }) => (
      <StorySettings
        key={String(globals.theme)}
        theme={String(globals.theme)}
        populated={parameters.populated === true}
      >
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
    // Regression: the dialog title, sections and values all rendered at 13px bold.
    const columns = ['Machines', 'Application'].map((name) => page.getByRole('heading', { name }));
    const sections = ['Transcript', 'Theme'].map((name) => page.getByRole('heading', { name }));
    const size = (element: Element) => parseFloat(getComputedStyle(element).fontSize);
    await expect(size(title)).toBeGreaterThan(size(label));
    for (const column of columns) {
      await expect(size(column)).toBe(size(label));
      await expect(getComputedStyle(column).fontWeight).toBe('600');
    }
    for (const section of sections) {
      await expect(size(section)).toBeLessThan(size(label));
      await expect(section).toHaveAttribute('aria-level', '4');
    }
    for (const element of [title, label, ...columns, ...sections]) {
      const style = getComputedStyle(element);
      await expect(style.fontFamily).toBe(getComputedStyle(title).fontFamily);
      await expect(style.textTransform).toBe('none');
      await expect(style.letterSpacing).toBe('normal');
    }
    const choice = page.getByText('Blockether Dark', { exact: true });
    await expect(size(choice)).toBe(size(label));
    await expect(getComputedStyle(choice).fontWeight).toBe('400');
    const description = page.getByText(
      'One expandable source line before Activity. Hiding code keeps every activity and result.',
    );
    await expect(getComputedStyle(description).fontSize).toBe('12px');
    await expect(getComputedStyle(description).lineHeight).toBe('18px');
    const available = innerWidth >= 640 ? innerWidth - 32 : innerWidth;
    await expect(dialog.getBoundingClientRect().width).toBe(
      Math.min(pointer ? 1152 : 896, available),
    );
    await expect(dialog.scrollWidth).toBe(dialog.clientWidth);
    const toggle = page.getByRole('switch', { name: /^Show Python code:/ });
    const checked = toggle.getAttribute('aria-checked');
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-checked', checked === 'true' ? 'false' : 'true');
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-checked', checked);
    const emptyServers = page.getByText('No MCP servers on this gateway.');
    await expect(getComputedStyle(emptyServers).fontSize).toBe(
      getComputedStyle(description).fontSize,
    );
    await expect(getComputedStyle(emptyServers).lineHeight).toBe(
      getComputedStyle(description).lineHeight,
    );
    await userEvent.click(page.getByRole('button', { name: /^ASR/ }));
    const emptySpeech = await page.findByText('No ASR engine is registered on this machine.');
    await expect(getComputedStyle(emptySpeech).fontSize).toBe(
      getComputedStyle(description).fontSize,
    );
    await expect(getComputedStyle(emptySpeech).lineHeight).toBe(
      getComputedStyle(description).lineHeight,
    );
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
    await expect(getComputedStyle(label).fontSize).toBe(pointer ? '13px' : '15px');
    await expect(getComputedStyle(label).lineHeight).toBe(pointer ? '20px' : '22px');
    await expect(getComputedStyle(hint).fontSize).toBe('12px');
    await expect(getComputedStyle(hint).lineHeight).toBe('18px');
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

/** Full production screen with representative provider and MCP rows. */
export const Populated: Story = {
  // Gateway caches are keyed by URL; keep this fleet separate from the empty stories.
  args: {
    gateways: [{ ...STORY_GATEWAYS[0], url: 'http://127.0.0.1:7781' }],
    primaryUrl: 'http://127.0.0.1:7781',
  },
  parameters: { populated: true },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await expect(await page.findByText('Anthropic', { exact: true })).toBeVisible();
    await expect(await page.findByText('filesystem', { exact: true })).toBeVisible();
    const application = page.queryByRole('button', { name: 'Show application settings' });
    if (application) await userEvent.click(application);
    const toggle = page.getByRole('switch', { name: 'filesystem MCP server: on' });
    await userEvent.click(toggle);
    await waitFor(() => expect(toggle).toHaveAttribute('aria-checked', 'false'));
    await userEvent.click(toggle);
    await waitFor(() => expect(toggle).toHaveAttribute('aria-checked', 'true'));
  },
};

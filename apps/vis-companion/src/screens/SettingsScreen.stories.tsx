import type { Meta, StoryObj } from '@storybook/react-vite';
import { useEffect, useState, type ReactNode } from 'react';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';
import { STORY_COMPACT_EXECUTIONS, STORY_GATEWAYS, storySettingsFetch } from '../dev/story-data';
import { openStepDigests } from '../dev/story-steps';
import { getThemePref, setThemePref } from '../lib/storage';
import { resolveTheme } from '../lib/theme';
import { THEMES } from '../lib/themes.generated';
import { SettingsDialog } from './SettingsScreen';
import { IterationTrace } from '../components/ChatContent';

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
    const heading = page.getByRole('heading', { name: 'Theme' });
    const panel = heading.closest('section')!;
    const choices = within(panel).getAllByRole('button');
    await expect(choices).toHaveLength(THEMES.length);
    const alternative = THEMES.find(
      (choice) => choice.id !== resolveTheme(String(globals.theme)).id,
    )!;
    const next = within(panel).getByRole('button', { name: alternative.label });
    await userEvent.click(next);
    await waitFor(() => expect(next).toHaveAttribute('aria-pressed', 'true'));
    await expect(theme).toHaveAttribute('aria-pressed', 'false');
    await expect(getThemePref()).resolves.toBe(alternative.id);
    // Keyboard selection keeps the original theme and its persisted preference in sync.
    theme.focus();
    await userEvent.keyboard('{Enter}');
    await waitFor(() => expect(theme).toHaveAttribute('aria-pressed', 'true'));
    await expect(next).toHaveAttribute('aria-pressed', 'false');
    await expect(getThemePref()).resolves.toBe(resolveTheme(String(globals.theme)).id);
  },
};

/** A sole machine shows its full settings without a disclosure or an extra press. */
export const SingleMachine: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await expect(await page.findByText('MCP servers')).toBeVisible();
    await expect(page.getByText('Providers')).toBeVisible();
    // The Rooms response must use the gateway contract, also in the preview.
    await expect(await page.findByRole('heading', { name: 'Council rooms' })).toBeVisible();
    const name = page.getByText('tower');
    await expect(name.closest('button')).toBeNull();
    await expect(name.closest('[aria-expanded]')).toBeNull();
    await expect(name.parentElement?.parentElement?.querySelector('.lucide-chevron-right')).toBeNull();
    await userEvent.click(name);
    await expect(page.getByText('MCP servers')).toBeVisible();
    await expect(page.getByRole('button', { name: 'Add a machine' })).toBeVisible();
  },
};

/**
 * PAIRING IS A BAND IN THE MACHINES COLUMN, never a dialog over the dialog: the
 * ＋ that opens it becomes − to hide it again, and the fleet stays on the same
 * plane as the form that joins it.
 */
export const PairingInline: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const dialog = await page.findByRole('dialog', { name: 'Settings' });
    await userEvent.click(page.getByRole('button', { name: 'Add a machine' }));
    // Regression, settings screenshot: the expanded machine control used an X rather
    // than the minus mark of the matching provider control.
    const toggle = page.getByRole('button', { name: 'Cancel adding a machine' });
    await expect(toggle.querySelector('.lucide-minus')).not.toBeNull();
    await expect(toggle.querySelector('.lucide-x')).toBeNull();
    await expect(toggle).toHaveAttribute('aria-expanded', 'true');

    await expect(page.getAllByRole('dialog')).toHaveLength(1);
    await expect(within(dialog).getByPlaceholderText(/vis:\/\/gateway/)).toBeVisible();
    const machine = within(dialog).getByText('tower');
    await expect(machine).toBeVisible();

    await userEvent.click(toggle);
    const addButton = page.getByRole('button', { name: 'Add a machine' });
    await expect(addButton.querySelector('.lucide-plus')).not.toBeNull();
    await expect(addButton).toHaveAttribute('aria-expanded', 'false');
    await waitFor(() => expect(page.queryByPlaceholderText(/vis:\/\/gateway/)).toBeNull());
  },
};

/** Experimental workflows require a separate, explicit opt-in on each machine. */
export const ExperimentalFeatures: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    for (const label of ['Subagents', 'Improve', 'Plan before coding']) {
      const toggle = await page.findByRole('switch', { name: `${label}: off` });
      await expect(toggle).not.toBeChecked();
      const row = toggle.closest('.grid')!;
      await expect(within(row as HTMLElement).getByText('Experimental')).toBeVisible();
      await userEvent.click(toggle);
      await waitFor(() => expect(toggle).toBeChecked());
      await userEvent.click(toggle);
      await waitFor(() => expect(toggle).not.toBeChecked());
    }
  },
};

/** Settings names and explanatory copy follow the desktop hierarchy without shrinking touch. */
export const ReadingLayout: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  render: (args) => (
    <>
      <IterationTrace whole iterations={STORY_COMPACT_EXECUTIONS} />
      <SettingsDialog {...args} />
    </>
  ),
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await page.findByText('MCP servers');
    await openStepDigests(canvasElement);
    const application = page.queryByRole('button', { name: 'Show application settings' });
    if (application) await userEvent.click(application);
    const sections = ['Responses', 'Theme'].map((name) => page.getByRole('heading', { name }));
    for (const section of sections) {
      await expect(section).toHaveAttribute('aria-level', '4');
    }
    const toggle = page.getByRole('switch', { name: /^Show Python code and results:/ });
    const checked = toggle.getAttribute('aria-checked');
    for (const shown of [checked !== 'true', checked === 'true']) {
      await userEvent.click(toggle);
      await expect(toggle).toHaveAttribute('aria-checked', String(shown));
      await waitFor(() =>
        expect(Boolean(canvasElement.querySelector('[data-execution-code]'))).toBe(shown),
      );
      if (!shown) {
        await expect(canvasElement.querySelector('[data-code-result]')).toBeNull();
        await expect(canvasElement.querySelector('[data-execution-activity]')).not.toBeNull();
      }
    }
    await userEvent.click(page.getByRole('button', { name: /^ASR/ }));
    await page.findByText('No ASR engine is registered on this machine.');
  },
};

export const ReadingLayoutPointer: Story = {
  ...ReadingLayout,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** MCP textareas use the same control scale as inputs; labels and hints keep their own roles. */
export const FormTypography: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await userEvent.click(await page.findByRole('button', { name: 'Add an MCP server' }));
    await page.findByRole('textbox', { name: 'Server name' });
    const args = page.getByRole('textbox', { name: /^Arguments — one per line/ });
    await userEvent.type(args, '-y{Enter}server-filesystem');
    await expect(args).toHaveValue('-y\nserver-filesystem');
    await userEvent.click(page.getByRole('button', { name: 'Streamable HTTP' }));
  },
};

export const FormTypographyPointer: Story = {
  ...FormTypography,
  tags: ['!test'],
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
    await userEvent.click(page.getByRole('button', { name: 'Add an MCP server' }));
    const form = page.getByRole('group', { name: 'MCP transport' }).parentElement!;
    await userEvent.click(within(form).getByRole('button', { name: 'Cancel' }));
  },
};

export const PopulatedPointer: Story = {
  ...Populated,
  tags: ['!test'],
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

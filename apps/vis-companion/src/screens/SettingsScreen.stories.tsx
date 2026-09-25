import type { Meta, StoryObj } from '@storybook/react-vite';
import { useEffect, useState, type ReactNode } from 'react';
import { expect, fn, userEvent, waitFor, within } from 'storybook/test';
import { STORY_COMPACT_EXECUTIONS, STORY_GATEWAYS, storySettingsFetch } from '../dev/story-data';
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
    const body = panel.lastElementChild!;
    const choices = within(panel).getAllByRole('button');
    const grid = choices[0].parentElement!;
    await expect(choices).toHaveLength(THEMES.length);
    // Regression: the theme picker was an inset input-colored slab without row separators.
    await expect(getComputedStyle(grid).rowGap).toBe('1px');
    await expect(getComputedStyle(grid).backgroundColor).toBe(
      getComputedStyle(body).borderTopColor,
    );
    for (const [index, choice] of choices.entries()) {
      const box = choice.getBoundingClientRect();
      const style = getComputedStyle(choice);
      await expect(box.left).toBe(body.getBoundingClientRect().left);
      await expect(box.width).toBe(body.getBoundingClientRect().width);
      await expect(choice.firstElementChild!.getBoundingClientRect().left).toBe(
        heading.getBoundingClientRect().left,
      );
      await expect(style.borderTopWidth).toBe('0px');
      await expect(style.borderBottomWidth).toBe('0px');
      if (index > 0) {
        await expect(box.top - choices[index - 1].getBoundingClientRect().bottom).toBe(1);
      }
      if (choice !== theme) {
        await expect(style.backgroundColor).toBe(getComputedStyle(panel).backgroundColor);
      }
    }
    await expect(getComputedStyle(theme).backgroundColor).not.toBe(
      getComputedStyle(panel).backgroundColor,
    );
    await expect(choices.at(-1)!.getBoundingClientRect().bottom).toBe(
      body.getBoundingClientRect().bottom,
    );
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
 * ＋ that opens it is the × that takes it away, and the fleet stays on the same
 * plane as the form that joins it.
 */
export const PairingInline: Story = {
  args: { gateways: STORY_GATEWAYS.slice(0, 1) },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const dialog = await page.findByRole('dialog', { name: 'Settings' });
    await userEvent.click(page.getByRole('button', { name: 'Add a machine' }));

    await expect(page.getAllByRole('dialog')).toHaveLength(1);
    const panel = within(dialog).getByRole('heading', { name: 'Add a machine' }).closest('section')!;
    await expect(within(dialog).getByPlaceholderText(/vis:\/\/gateway/)).toBeVisible();
    const machine = within(dialog).getByText('tower');
    await expect(machine).toBeVisible();
    // The form stands ABOVE the fleet, in the column whose band opened it.
    await expect(panel.getBoundingClientRect().bottom).toBeLessThanOrEqual(
      machine.getBoundingClientRect().top,
    );

    await userEvent.click(page.getByRole('button', { name: 'Cancel adding a machine' }));
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
    const application = page.queryByRole('button', { name: 'Show application settings' });
    if (application) await userEvent.click(application);
    const dialog = page.getByRole('dialog', { name: 'Settings' });
    await dialog.ownerDocument.fonts.ready;
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const label = page.getByText('Show Python code and results', { exact: true });
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
      'Show source code and raw results before Activity. Turn off to show only Activity.',
    );
    await expect(getComputedStyle(description).fontSize).toBe('12px');
    await expect(getComputedStyle(description).lineHeight).toBe('18px');
    const available = innerWidth >= 640 ? innerWidth - 32 : innerWidth;
    await expect(dialog.getBoundingClientRect().width).toBe(
      Math.min(pointer ? 1152 : 896, available),
    );
    await expect(dialog.scrollWidth).toBe(dialog.clientWidth);
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
    const panelBody = page
      .getByRole('heading', { name: 'MCP servers' })
      .closest('section')!.lastElementChild!;
    const form = page.getByRole('group', { name: 'MCP transport' }).parentElement!;
    // An empty list's add form uses the panel's separator, never a second top border.
    await expect(getComputedStyle(panelBody).borderTopWidth).toBe('1px');
    await expect(getComputedStyle(form).borderTopWidth).toBe('0px');
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const label = page.getByText('Server name', { exact: true });
    const hint = page.getByText('Arguments are passed directly, never through a shell.');
    // A reading role is one step on both faces; only the control's box is tighter
    // under a pointer, which is what the textareas below still answer to.
    await expect(getComputedStyle(label).fontSize).toBe('13px');
    await expect(getComputedStyle(label).lineHeight).toBe('20px');
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
    await canvasElement.ownerDocument.fonts.ready;
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    // Keep the real call sites on the shared header rhythm, not just the layout fixture.
    for (const name of ['Machines', 'Providers', 'Notifications', 'MCP servers']) {
      const heading = page.getByRole('heading', { name });
      const header = heading.closest('header')!;
      const title = heading.getBoundingClientRect();
      await expect(header.clientHeight).toBe(pointer ? 40 : 44);
      await expect(title.top + title.height / 2 - header.getBoundingClientRect().top).toBeCloseTo(
        header.clientHeight / 2,
        1,
      );
      const body = header.nextElementSibling!;
      if (getComputedStyle(body).display !== 'none') {
        // Regression: section headings merged into the first row of otherwise bordered lists.
        await expect(
          parseFloat(getComputedStyle(header).borderBottomWidth) +
            parseFloat(getComputedStyle(body).borderTopWidth),
        ).toBe(1);
      }
    }
    const notification = page.getByRole('switch', { name: /^Notifications from/ });
    const notificationBox = notification.getBoundingClientRect();
    // The + keeps its standard touch box but stands over the row chevrons, while
    // the switch and disclosure glyphs retain their own right edge.
    for (const name of ['Add a machine', 'Add an MCP server']) {
      const action = page.getByRole('button', { name });
      const box = action.getBoundingClientRect();
      const mark = action.querySelector('svg')!.getBoundingClientRect();
      await expect(box.right).toBeGreaterThan(notificationBox.right);
      await expect(box.right).toBeLessThanOrEqual(
        page.getByRole('dialog').getBoundingClientRect().right,
      );
      await expect((mark.left + mark.right) / 2).toBeCloseTo((box.left + box.right) / 2, 1);
      await expect(box.height).toBe(pointer ? 28 : 32);
      if (!pointer) {
        const reach = getComputedStyle(action, '::after');
        const left = box.left + parseFloat(reach.left);
        const right = box.right - parseFloat(reach.right);
        await expect(right).toBeLessThanOrEqual(
          page.getByRole('dialog').getBoundingClientRect().right,
        );
        await expect(right - left).toBeGreaterThanOrEqual(44);
      }
    }
    // Regression: in the phone settings screenshot, the + sat ten pixels left
    // of every row chevron. Values and switches keep their own right edge;
    // disclosure glyphs share the add action's center instead.
    const addMark = page.getByRole('button', { name: 'Add an MCP server' }).querySelector('svg')!;
    const center = (element: Element) => {
      const box = element.getBoundingClientRect();
      return (box.left + box.right) / 2;
    };
    if (!pointer) {
      for (const row of [
        page.getByText('Anthropic', { exact: true }).closest('button')!,
        page.getByText('filesystem', { exact: true }).closest('button')!,
        page.getByRole('button', { name: /ASR/ }),
      ]) {
        const chevron = row.querySelector('.lucide-chevron-right')!;
        await expect(center(chevron)).toBeCloseTo(center(addMark), 1);
      }
    }
    // Regression, settings screenshot: the real machine, provider and MCP menus share
    // the add action's rail on a pointer; only touch drawers keep the list inset.
    if (pointer) {
      for (const name of ['tower', 'Anthropic', 'filesystem']) {
        const menuMark = page
          .getByRole('button', { name: `Actions for ${name}` })
          .querySelector('svg')!;
        await expect(center(menuMark)).toBeCloseTo(center(addMark), 1);
      }
    }
    const toggle = page.getByRole('switch', { name: 'filesystem MCP server: on' });
    await userEvent.click(toggle);
    await waitFor(() => expect(toggle).toHaveAttribute('aria-checked', 'false'));
    await userEvent.click(toggle);
    await waitFor(() => expect(toggle).toHaveAttribute('aria-checked', 'true'));
    await userEvent.click(page.getByRole('button', { name: 'Add an MCP server' }));
    const form = page.getByRole('group', { name: 'MCP transport' }).parentElement!;
    // In a populated list, the preceding row supplies the add form's single divider.
    await expect(
      parseFloat(getComputedStyle(form.previousElementSibling!).borderBottomWidth) +
        parseFloat(getComputedStyle(form).borderTopWidth),
    ).toBe(1);
    await userEvent.click(within(form).getByRole('button', { name: 'Cancel' }));
  },
};

export const PopulatedPointer: Story = {
  ...Populated,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

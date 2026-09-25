import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect, userEvent } from 'storybook/test';
import { PencilIcon, PlusIcon } from '../../components/icons';
import { SwipeActions } from '../../components/SwipeActions';
import {
  ChoiceCell,
  IconButton,
  NotifyConnectionSwitch,
  SettingsHeader,
  Text,
} from '../../components/ui';
import { THEMES } from '../../lib/themes.generated';
import { DiagnosticsPanel } from './DiagnosticsPanel';
import { SettingsColumn, SettingsPanel } from './SettingsLayout';

/**
 * THE SETTINGS COLUMN'S TWO LIVES.
 *
 * On a phone the dialog's columns stack and the machines lead, so the
 * application's own settings fold behind their band — press the band, the
 * chevron turns, the panels stand under it. Where both columns fit beside each
 * other the same call paints no fold at all: switch the frame to Desktop and
 * the band is a plain heading again.
 */

const body = (
  <SettingsPanel title="Theme">
    <div className="grid grid-cols-1 gap-px bg-dialog-edge">
      <ChoiceCell title="Vis Light" isSelected isLeaf />
      <ChoiceCell title="Vis Dark" isSelected={false} isLeaf />
    </div>
  </SettingsPanel>
);

const meta = {
  title: 'Screens/Settings column',
  component: SettingsColumn,
  parameters: { layout: 'padded' },
  render: function Render(args) {
    const [open, setOpen] = useState(args.disclosure?.isOpen ?? false);
    return (
      <SettingsColumn
        {...args}
        disclosure={
          args.disclosure && {
            isOpen: open,
            onToggle: () => setOpen((current) => !current),
            label: `${open ? 'Hide' : 'Show'} application settings`,
          }
        }
      />
    );
  },
} satisfies Meta<typeof SettingsColumn>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Phone: the machines lead, and the application's settings wait behind the band. */
export const StackedFoldClosed: Story = {
  args: {
    title: 'Application',
    disclosure: {
      isOpen: false,
      onToggle: () => {},
      label: 'Show application settings',
    },
    children: body,
  },
};

/** Phone, unfolded: the panels stand under the band, and the chevron points down. */
export const StackedFoldOpen: Story = {
  args: {
    title: 'Application',
    disclosure: {
      isOpen: true,
      onToggle: () => {},
      label: 'Hide application settings',
    },
    children: body,
  },
};

/** Desktop: both columns stand open, so the band carries no chevron at all. */
export const StandingOpen: Story = {
  args: {
    title: 'Application',
    disclosure: {
      isOpen: true,
      onToggle: () => {},
      label: 'Hide application settings',
    },
    children: body,
  },
  parameters: { viewport: { defaultViewport: 'desktop' } },
};

/** The last settings panel ends without a rule above the phone's safe area. */
export const DiagnosticsFooter: Story = {
  args: {
    title: 'Application',
    children: (
      <>
        {body}
        <DiagnosticsPanel isOpen onToggle={() => {}} />
      </>
    ),
  },
  play: async ({ canvas }) => {
    const diagnostics = canvas.getByRole('heading', { name: 'Diagnostics' }).closest('section')!;
    const columnBody = diagnostics.parentElement!;
    // Regression: the column's bottom border drew a full-width line below Export app logs.
    await expect(getComputedStyle(columnBody).borderBottomWidth).toBe('0px');
    await expect(getComputedStyle(diagnostics.previousElementSibling!).borderBottomWidth).toBe(
      '1px',
    );
    await expect(canvas.getByRole('button', { name: 'Export app logs' })).toBeVisible();
  },
};

/** Header-only panels share one divider with the next panel, never an empty body's rule. */
export const HeaderOnlyPanels: Story = {
  args: {
    title: 'Machine',
    children: (
      <>
        <SettingsPanel
          title="Notifications"
          action={<NotifyConnectionSwitch machine="visgw" isOn={false} onClick={() => {}} />}
        >
          {false}
        </SettingsPanel>
        {body}
        <DiagnosticsPanel isOpen={false} onToggle={() => {}} />
      </>
    ),
  },
  play: async ({ canvas }) => {
    const notifications = canvas.getByRole('heading', { name: 'Notifications' }).closest('section')!;
    const diagnostics = canvas.getByRole('heading', { name: 'Diagnostics' }).closest('section')!;
    // Regression: Notifications' empty body left a header rule beside the column's divider.
    for (const panel of [notifications, diagnostics]) {
      const header = panel.querySelector('header')!;
      await expect(getComputedStyle(header).borderBottomWidth).toBe('0px');
      await expect(panel.lastElementChild).not.toBeVisible();
      await expect(panel.getBoundingClientRect().height).toBe(
        header.getBoundingClientRect().height + parseFloat(getComputedStyle(panel).borderBottomWidth),
      );
    }
    await expect(getComputedStyle(notifications).borderBottomWidth).toBe('1px');
    await expect(getComputedStyle(diagnostics).borderBottomWidth).toBe('0px');
    // Regression: a populated section must separate its heading from the first row.
    const theme = canvas.getByRole('heading', { name: 'Theme' }).closest('section')!;
    await expect(
      parseFloat(getComputedStyle(theme.querySelector('header')!).borderBottomWidth) +
        parseFloat(getComputedStyle(theme.lastElementChild!).borderTopWidth),
    ).toBe(1);
    await expect(canvas.getByRole('switch', { name: 'Notifications from visgw: off' })).toBeVisible();
  },
};

/** Section marks center on the row rail; a switch still ends on the gutter. */
export const HeaderRhythm: Story = {
  args: { title: 'Machines', children: null },
  render: function Render(args) {
    const [notify, setNotify] = useState(false);
    const [diagnostics, setDiagnostics] = useState(false);
    return (
      <SettingsColumn
        {...args}
        action={
          <IconButton variant="quiet" align="trailing" label="Add a machine">
            <PlusIcon className="size-4" />
          </IconButton>
        }
      >
        {/* The shared primitive beside its column and panel compositions. */}
        <section className="bg-panel">
          <header>
            <SettingsHeader
              action={
                <IconButton variant="quiet" align="trailing" label="Add a provider">
                  <PlusIcon className="size-4" />
                </IconButton>
              }
            >
              <Text as="h4" variant="section" className="min-w-0 flex-auto truncate">
                Providers
              </Text>
            </SettingsHeader>
          </header>
          {/* A row as the settings lists build it: the pressable half, then the
              trailing cell every kebab in this dialog lives in. */}
          <SwipeActions
            alignMenuWithHeader
            label="tower"
            actions={[
              {
                key: 'rename',
                label: 'Rename',
                icon: <PencilIcon className="size-4" />,
                onSelect: () => {},
              },
            ]}
          >
            <div className="flex min-h-12 min-w-0 items-center gap-3 px-3 py-2 sm:px-4">
              <Text variant="label">tower</Text>
            </div>
          </SwipeActions>
        </section>
        <SettingsPanel
          title="Notifications"
          action={
            <NotifyConnectionSwitch
              machine="visgw"
              isOn={notify}
              onClick={() => setNotify((current) => !current)}
            />
          }
        >
          {false}
        </SettingsPanel>
        <SettingsPanel
          title="MCP servers"
          action={
            <IconButton variant="quiet" align="trailing" label="Add an MCP server">
              <PlusIcon className="size-4" />
            </IconButton>
          }
        >
          {false}
        </SettingsPanel>
        {body}
        <DiagnosticsPanel
          isOpen={diagnostics}
          onToggle={() => setDiagnostics((current) => !current)}
        />
      </SettingsColumn>
    );
  },
  play: async ({ canvas, canvasElement }) => {
    await canvasElement.ownerDocument.fonts.ready;
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    const titles = [
      'Machines',
      'Providers',
      'Notifications',
      'MCP servers',
      'Theme',
      'Diagnostics',
    ];
    const headings = titles.map((name) => canvas.getByRole('heading', { name }));
    // Regression: asymmetric padding lowered titles by 4px; a switch, a plus and
    // a disclosure each set a different header height and trailing alignment.
    for (const heading of headings) {
      const header = heading.closest('header')!;
      const title = heading.getBoundingClientRect();
      const frame = header.getBoundingClientRect();
      await expect(header.clientHeight).toBe(pointer ? 40 : 44);
      await expect(title.top + title.height / 2 - frame.top).toBe(header.clientHeight / 2);
      await expect(title.left).toBe(headings[0].getBoundingClientRect().left);
    }
    const toggle = canvas.getByRole('switch', { name: 'Notifications from visgw: off' });
    const adds = ['Add a machine', 'Add a provider', 'Add an MCP server'].map((name) =>
      canvas.getByRole('button', { name }),
    );
    const ink = (control: Element) => control.querySelector('svg') ?? control;
    // The add marks now share the row disclosure rail, not the inset action-menu
    // rail. A switch and a bare disclosure still end on the band's gutter.
    const disclosure = canvas.getByRole('button', { name: 'Show diagnostics' });
    const center = (element: Element) => {
      const box = element.getBoundingClientRect();
      return (box.left + box.right) / 2;
    };
    for (const add of adds) {
      await expect(center(ink(add))).toBeCloseTo(center(ink(disclosure)), 1);
    }
    // Regression, settings screenshot: row menus sat 8px inside the plus/minus rail.
    // On a pointer the menu mark and section action must share the same vertical line.
    if (pointer) {
      const menu = canvas.getByRole('button', { name: 'Actions for tower' });
      await expect(center(ink(menu))).toBeCloseTo(center(ink(adds[0])), 1);
    }
    const rail = ink(toggle).getBoundingClientRect().right;
    await expect(ink(disclosure).getBoundingClientRect().right).toBeCloseTo(rail, 1);
    for (const action of [...adds, toggle]) {
      const reach = getComputedStyle(action, '::after');
      const height = action.getBoundingClientRect().height;
      await expect(
        height +
          Math.max(0, -parseFloat(reach.top) || 0) +
          Math.max(0, -parseFloat(reach.bottom) || 0),
      ).toBeGreaterThanOrEqual(pointer ? 28 : 44);
    }
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-checked', 'true');
    await userEvent.click(toggle);
    await expect(toggle).toHaveAttribute('aria-checked', 'false');
    await userEvent.click(canvas.getByRole('button', { name: 'Show diagnostics' }));
    await expect(canvas.getByRole('button', { name: 'Hide diagnostics' })).toHaveAttribute(
      'aria-expanded',
      'true',
    );
  },
};

export const HeaderRhythmPointer: Story = {
  ...HeaderRhythm,
  globals: { viewport: { value: 'desktop', isRotated: false } },
};

/** Overflow stays reachable without desktop scrollbar thumbs or reserved lanes. */
export const DesktopOverflow: Story = {
  globals: { viewport: { value: 'desktop', isRotated: false } },
  args: {
    title: 'Application',
    children: (
      <SettingsPanel title="Theme">
        <div className="grid grid-cols-1 gap-px bg-dialog-edge">
          {THEMES.map((theme, index) => (
            <ChoiceCell key={theme.id} title={theme.label} isSelected={index === 0} isLeaf />
          ))}
        </div>
      </SettingsPanel>
    ),
  },
  decorators: [
    (Story) => (
      <div className="grid h-80 max-w-lg">
        <Story />
      </div>
    ),
  ],
  play: async ({ canvas }) => {
    const scroller = canvas.getByRole('heading', { name: 'Theme' }).closest('section')!.parentElement!;
    // Regression: settings still showed desktop scrollbars after the sidebar hid its own.
    await expect(getComputedStyle(scroller).scrollbarWidth).toBe('none');
    await expect(getComputedStyle(scroller, '::-webkit-scrollbar').display).toBe('none');
    await expect(getComputedStyle(scroller).scrollbarGutter).toBe('auto');
    await expect(getComputedStyle(scroller).overflowY).toBe('auto');
    await expect(scroller.scrollHeight).toBeGreaterThan(scroller.clientHeight);
    scroller.scrollTo({ top: 80, behavior: 'instant' });
    await expect(scroller.scrollTop).toBe(80);
  },
};

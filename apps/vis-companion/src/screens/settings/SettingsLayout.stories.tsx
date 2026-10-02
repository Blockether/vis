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
import { SettingsPanel } from './SettingsLayout';

const body = (
  <SettingsPanel title="Theme">
    <div className="grid grid-cols-1 gap-px bg-dialog-edge">
      <ChoiceCell title="Vis Light" isSelected isLeaf />
      <ChoiceCell title="Vis Dark" isSelected={false} isLeaf />
    </div>
  </SettingsPanel>
);
const meta = {
  title: 'Screens/Settings panels',
  component: SettingsPanel,
  parameters: { layout: 'padded' },
  render: function Render(args) {
    const [open, setOpen] = useState(args.disclosure?.isOpen ?? false);
    return (
      <SettingsPanel
        {...args}
        disclosure={
          args.disclosure && {
            ...args.disclosure,
            isOpen: open,
            onToggle: () => setOpen((value) => !value),
          }
        }
      />
    );
  },
} satisfies Meta<typeof SettingsPanel>;
export default meta;
type Story = StoryObj<typeof meta>;
export const Default: Story = { args: { title: 'This device', children: body } };
export const DiagnosticsFooter: Story = {
  args: {
    title: 'This device',
    children: (
      <>
        {body}
        <DiagnosticsPanel isOpen onToggle={() => {}} />
      </>
    ),
  },
  play: async ({ canvas }) => {
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
    await expect(
      canvas.getByRole('switch', { name: 'Notifications from visgw: off' }),
    ).toBeVisible();
  },
};

/** Section marks center on the row rail; a switch still ends on the gutter. */
export const HeaderRhythm: Story = {
  args: { title: 'Machines', children: null },
  render: function Render(args) {
    const [notify, setNotify] = useState(false);
    const [diagnostics, setDiagnostics] = useState(false);
    return (
      <SettingsPanel
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
      </SettingsPanel>
    );
  },
  play: async ({ canvas }) => {
    const toggle = canvas.getByRole('switch', { name: 'Notifications from visgw: off' });
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
  tags: ['!test'],
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
};

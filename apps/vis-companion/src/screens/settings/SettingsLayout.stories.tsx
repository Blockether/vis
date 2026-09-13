import type { Meta, StoryObj } from '@storybook/react-vite';
import { useState } from 'react';
import { expect } from 'storybook/test';
import { ChoiceCell } from '../../components/ui';
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

import type { Meta, StoryObj } from '@storybook/react-vite';
import { ChoiceCell } from '../../components/ui';
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

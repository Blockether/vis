import { useState } from 'react';
import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent } from 'storybook/test';
import { DiagnosticsPanel } from './DiagnosticsPanel';

/**
 * THE APPLICATION COLUMN'S FOLDED LAST BAND.
 *
 * Resting, it is one band and a chevron; pressed, six facts pair into three compact
 * rows above the one way out. The open body is what this gallery owns — the export
 * verb's busy and banner states live behind the platform hand-off and are
 * pinned in `SettingsScreen.diagnostics.test.tsx`, beside the mock that stands
 * in for it.
 */
const meta = {
  title: 'Screens/Diagnostics panel',
  component: DiagnosticsPanel,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof DiagnosticsPanel>;

export default meta;
type Story = StoryObj<typeof meta>;

function InteractiveDiagnosticsPanel() {
  const [isOpen, setOpen] = useState(false);
  return (
    <DiagnosticsPanel
      isOpen={isOpen}
      onToggle={() => setOpen((open) => !open)}
    />
  );
}

/** Resting: one band, nothing painted under it; pressing anywhere on it opens it. */
export const Resting: Story = {
  args: { isOpen: false, onToggle: () => {} },
  render: () => <InteractiveDiagnosticsPanel />,
};

/** The named band, not only its chevron, opens the diagnostics. */
export const BandPress: Story = {
  args: { isOpen: false, onToggle: () => {} },
  render: () => <InteractiveDiagnosticsPanel />,
  play: async ({ canvas }) => {
    await userEvent.click(canvas.getByRole('heading', { name: 'Diagnostics' }));
    await expect(canvas.getByRole('button', { name: 'Hide diagnostics' })).toBeVisible();
    await expect(canvas.getByRole('button', { name: 'Export app logs' })).toBeVisible();
  },
};

/** Open, phone: six facts keep their words while sharing three compact rows. */
export const Open: Story = {
  args: { isOpen: true, onToggle: () => {} },
};

/** Open, desktop: the same compact matrix follows the machine-panel rhythm. */
export const OpenDesktop: Story = {
  args: { isOpen: true, onToggle: () => {} },
  parameters: { viewport: { defaultViewport: 'desktop' } },
};

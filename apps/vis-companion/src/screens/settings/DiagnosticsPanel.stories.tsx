import type { Meta, StoryObj } from '@storybook/react-vite';
import { DiagnosticsPanel } from './DiagnosticsPanel';

/**
 * THE APPLICATION COLUMN'S FOLDED LAST BAND.
 *
 * Resting, it is one band and a chevron; pressed, the six facts and the one
 * way out stand under it. The open body is what this gallery owns — the export
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

/** Resting: one band, nothing painted under it. */
export const Resting: Story = {
  args: { isOpen: false, onToggle: () => {} },
};

/** Open, phone: every value holds the trailing edge, and the commit earns its width. */
export const Open: Story = {
  args: { isOpen: true, onToggle: () => {} },
};

/** Open, desktop: the same rows on the dialog's pointer rhythm, no tighter. */
export const OpenDesktop: Story = {
  args: { isOpen: true, onToggle: () => {} },
  parameters: { viewport: { defaultViewport: 'desktop' } },
};

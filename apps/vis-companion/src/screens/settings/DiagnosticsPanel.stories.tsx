import type { Meta, StoryObj } from '@storybook/react-vite';
import { DiagnosticsPanel } from './DiagnosticsPanel';

/**
 * THE APPLICATION COLUMN'S LAST PANEL: six machine facts and the one way out.
 *
 * The resting paint is what this gallery owns — the export verb's busy and
 * banner states live behind the platform hand-off and are pinned in
 * `SettingsScreen.diagnostics.test.tsx`, beside the mock that stands in for it.
 */
const meta = {
  title: 'Screens/Diagnostics panel',
  component: DiagnosticsPanel,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof DiagnosticsPanel>;

export default meta;
type Story = StoryObj<typeof meta>;

/** Phone: every value holds the trailing edge, and the commit earns its width. */
export const Phone: Story = {};

/** Desktop: the same rows on the dialog's pointer rhythm, no tighter. */
export const Desktop: Story = {
  parameters: { viewport: { defaultViewport: 'desktop' } },
};

import type { Meta, StoryObj } from '@storybook/react-vite';
import { TABLE_BLOCK } from '../dev/story-data';
import { DataTable } from './DataTable';

/**
 * A CSV ARTIFACT IS DATA, NOT A PICTURE.
 *
 * `attach` emits a `vis-table` fence and both surfaces paint it as a real grid.
 * The fixture is wider than a phone on purpose: what this control is asked about
 * is the horizontal scroll, the numeric columns' right alignment, and whether the
 * header stays legible once a column is sorted.
 *
 * `compact` is the transcript's copy — a few rows inline under the turn that made
 * it — and `fill` is the same table opened as an artifact, which owns the body and
 * pages inside it.
 */
const meta = {
  title: 'Session/Artifacts/Table',
  component: DataTable,
  parameters: { layout: 'padded' },
} satisfies Meta<typeof DataTable>;

export default meta;

type Story = StoryObj<typeof meta>;

/** Opened as an artifact: it takes the body and pages inside it. */
export const Opened: Story = {
  args: { body: TABLE_BLOCK, compact: false, fill: true },
  decorators: [
    (Story) => (
      <div className="flex h-[520px] flex-col">
        <Story />
      </div>
    ),
  ],
};

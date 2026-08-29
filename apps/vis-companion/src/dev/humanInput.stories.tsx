import type { Meta, StoryObj } from '@storybook/react-vite';
import { HumanInputSheetVariant } from './humanInputVariants';

/**
 * THE STATES THAT CAN BREAK THE ONE SHEET WE HAVE.
 *
 * Not a comparison of skins: every entry here is the SHIPPED `HumanInputSheet`
 * under a different request, so what is photographed is the sheet itself and not
 * a look-alike that drifts from it. The requests live in `humanInputVariants.tsx`
 * beside the suite that already reads them, which is why a state is a fixture and
 * never a mockup.
 *
 * The sheet portals to `document.body`, so a story fills the frame rather than
 * sitting inside the gallery's own stacking context.
 */
const meta = {
  title: 'Vocabulary/Human input',
  component: HumanInputSheetVariant,
} satisfies Meta<typeof HumanInputSheetVariant>;

export default meta;

type Story = StoryObj<typeof meta>;

/** A decision with a reason and two answers. */
export const Approve: Story = { args: { state: 'approve' } };
/** One question, nothing else on the sheet. */
export const Minimal: Story = { args: { state: 'minimal' } };
/** Enough fields to scroll: the answer bar must stay reachable. */
export const Long: Story = { args: { state: 'long' } };
/** No way out but answering. */
export const Uncancellable: Story = { args: { state: 'uncancellable' } };
/** The engine refused the answer: field errors and a sheet-level one at once. */
export const Rejected: Story = { args: { state: 'rejected' } };
/** A bounded number, dragged. */
export const Slider: Story = { args: { state: 'slider' } };
/** A code, digit by digit, with a second field that failed validation. */
export const Otp: Story = { args: { state: 'otp' } };
/** Fields that belong together, and one of them wrong. */
export const Grouped: Story = { args: { state: 'grouped' } };

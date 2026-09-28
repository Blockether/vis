import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, userEvent, within } from 'storybook/test';
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
/** Regression #282: authored newlines remain separate rows. */
export const MarkdownDescription: Story = {
  args: { state: 'markdown' },
  play: async () => {
    const body = within(document.body);
    const labels = ['Environment:', 'Service:', 'Command:'].map((label) => body.getByText(label));
    const paragraph = labels[0].closest('p')!;
    await expect(paragraph.querySelectorAll('br')).toHaveLength(2);
    await expect(paragraph).not.toHaveTextContent('**');
    for (const label of labels) await expect(label.tagName).toBe('STRONG');
    const confirm = body.getByRole('checkbox', { name: 'Proceed with this command' });
    await userEvent.click(confirm);
    await expect(confirm).toHaveAttribute('aria-checked', 'true');
    await expect(body.getByRole('button', { name: 'Run' })).toBeEnabled();
  },
};
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
export const Grouped: Story = {
  args: { state: 'grouped' },
  play: async () => {
    const body = within(document.body);
    const host = body.getByPlaceholderText('db.internal');
    const group = host.closest('fieldset')!;
    const originalWidth = group.style.width;
    // Constrain the actual group, not the viewport: sheets and nested panels
    // can be narrow on desktop too. Restore the normal gallery presentation.
    try {
      group.style.width = '280px';
    } finally {
      group.style.width = originalWidth;
    }
  },
};

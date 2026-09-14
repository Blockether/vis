import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_ARTIFACT_HISTORY, STORY_ARTIFACTS, STORY_INERT_CLIENT } from '../dev/story-data';
import { ArtifactsSheet } from './ArtifactsSheet';

/** The session's produced files, indexed without eagerly fetching their bytes. */
const meta = {
  title: 'Components/Artifacts sheet',
  component: ArtifactsSheet,
  args: {
    client: STORY_INERT_CLIENT,
    sid: 'session-story',
    artifacts: STORY_ARTIFACTS,
    onClose: () => {},
  },
} satisfies Meta<typeof ArtifactsSheet>;

export default meta;
type Story = StoryObj<typeof meta>;

const close = fn();

/** Three non-previewable files prove the index is not secretly an image gallery. */
export const Files: Story = {
  args: { onClose: close },
  play: async ({ args, canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    const filters = page.getByRole('group', { name: 'Filter artifacts by kind' });
    const buttons = within(filters).getAllByRole('button');
    const pointer = matchMedia('(min-width: 640px) and (pointer: fine)').matches;
    for (const [index, button] of buttons.entries()) {
      const box = button.getBoundingClientRect();
      await expect(box.height).toBe(pointer ? 28 : 32);
      if (index) {
        await expect(
          box.left - buttons[index - 1].getBoundingClientRect().right,
        ).toBeGreaterThanOrEqual(8);
      }
      if (!pointer && box.right <= filters.getBoundingClientRect().right) {
        for (const y of [box.top - 5, box.bottom + 5]) {
          await expect(
            button.contains(button.ownerDocument.elementFromPoint(box.left + box.width / 2, y)),
          ).toBe(true);
        }
      }
    }
    await userEvent.click(page.getByRole('button', { name: 'Close artifacts' }));
    await expect(args.onClose).toHaveBeenCalledOnce();
  },
};

/** A rewritten artifact keeps its history control inside the newest tile. */
export const History: Story = { args: { artifacts: [STORY_ARTIFACT_HISTORY] } };

/** A filter with no matches says so inside the same full sheet. */
export const Empty: Story = { args: { artifacts: [] } };

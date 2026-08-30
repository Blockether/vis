import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_ARTIFACTS, STORY_INERT_CLIENT } from '../dev/story-data';
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
    await userEvent.click(page.getByRole('button', { name: 'Close artifacts' }));
    await expect(args.onClose).toHaveBeenCalledOnce();
  },
};

/** A filter with no matches says so inside the same full sheet. */
export const Empty: Story = { args: { artifacts: [] } };

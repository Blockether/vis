import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent, within } from 'storybook/test';

import { STORY_ARTIFACT_HISTORY, STORY_ARTIFACTS, STORY_INERT_CLIENT } from '../dev/story-data';
import type { SessionArtifact } from '../lib/artifacts';
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

const note: SessionArtifact = {
  key: 'i1:0',
  kind: 'doc',
  name: 'plan.md',
  media: 'MD',
  mediaType: 'text/markdown',
  size: 30,
  sizeLabel: '30 B',
  turn: 1,
  iterationId: 'i1',
  index: 0,
  version: 1,
};

/** Opening a document keeps its band flush with the top of the full-screen surface. */
export const OpenedDocument: Story = {
  args: {
    client: {
      attachmentUrl: async () => 'blob:plan',
      attachmentBlob: async () =>
        new Blob(['# Plan\n\nRead without a margin.'], { type: 'text/markdown' }),
      retainAttachment: () => () => {},
    } as unknown as typeof STORY_INERT_CLIENT,
    artifacts: [note],
    initialArtifact: note,
  },
  play: async ({ canvasElement }) => {
    const page = within(canvasElement.ownerDocument.body);
    await page.findByRole('heading', { name: 'Plan', level: 1 });
    const detail = page.getByRole('dialog', { name: 'plan.md' });
    const header = detail.querySelector('header')!;
    await expect(header.getBoundingClientRect().top).toBe(detail.getBoundingClientRect().top);
    await expect(getComputedStyle(detail).paddingTop).toBe('0px');
    const body = detail.lastElementChild!;
    await expect(body.getBoundingClientRect().top).toBe(header.getBoundingClientRect().bottom);
    await expect(body.getBoundingClientRect().bottom).toBe(detail.getBoundingClientRect().bottom);

    await userEvent.click(page.getByRole('button', { name: 'Close plan.md' }));
    await expect(page.queryByRole('dialog', { name: 'plan.md' })).toBeNull();
    const sheet = page.getByRole('region', { name: 'Artifacts produced by the model' });
    await expect(sheet.querySelector('header')!.getBoundingClientRect().top).toBe(
      sheet.getBoundingClientRect().top,
    );
    await expect(getComputedStyle(sheet).paddingTop).toBe('0px');
  },
};

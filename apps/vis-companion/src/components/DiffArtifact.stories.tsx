import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn } from 'storybook/test';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/diff.json';
import { DiffArtifact } from './DiffArtifact';
import { OverlayScreen } from './DocArtifact';

const meta = {
  title: 'Components/Diff artifact',
  component: DiffArtifact,
  parameters: { layout: 'fullscreen' },
  args: {
    source: new Blob([JSON.stringify(fixture)]),
    chrome: ({ actions, note, body }) => (
      <OverlayScreen title="Session search changes" subtitle={note} actions={actions} onClose={fn()}>
        {body}
      </OverlayScreen>
    ),
  },
} satisfies Meta<typeof DiffArtifact>;
export default meta;
type Story = StoryObj<typeof meta>;

/** A diff snapshot is read-only: the patch, without review controls. */
export const ReadOnly: Story = {
  play: async ({ canvas }) => {
    await canvas.findByText('+(search title {:archived true})', { selector: 'p' });
    await expect(
      canvas.queryByRole('button', { name: 'Comment on the whole document' }),
    ).not.toBeInTheDocument();
  },
};
export const Empty: Story = {
  args: { source: new Blob([JSON.stringify({ ...fixture, patch: '' })]) },
};
export const Invalid: Story = {
  args: { source: new Blob(['invalid snapshot']) },
};

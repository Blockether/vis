import type { Meta, StoryObj } from '@storybook/react-vite';
import { fn } from 'storybook/test';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/diff.json';
import { DiffArtifact } from './DiffArtifact';
import { OverlayScreen } from './DocArtifact';

const meta = {
  title: 'Session/Artifacts/Diff',
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
};

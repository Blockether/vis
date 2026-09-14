import type { Meta, StoryObj } from '@storybook/react-vite';
import { expect, fn, userEvent } from 'storybook/test';
import fixture from '../../../../packages/vis-contract/resources/vis-contract/fixtures/diff.json';
import type { ComponentProps } from 'react';
import { DiffArtifact } from './DiffArtifact';
import { OverlayScreen } from './ui';

const client = {
  base: 'http://127.0.0.1',
  saveArtifactText: fn(async () => ({ version: 4 })),
  submitTurn: fn(async () => {}),
} as unknown as ComponentProps<typeof DiffArtifact>['client'];
const meta = {
  title: 'Components/Diff review',
  component: DiffArtifact,
  parameters: { layout: 'fullscreen' },
  args: {
    client,
    sid: 'diff-story',
    iterationId: 'i1',
    name: 'DIFF-session-search.json',
    version: 3,
    source: new Blob([JSON.stringify({ ...fixture, comments: [] })]),
    commentable: true,
    chrome: ({ actions, note, body }) => (
      <OverlayScreen
        title="Session search changes"
        subtitle={note}
        actions={actions}
        onClose={fn()}
      >
        {body}
      </OverlayScreen>
    ),
  },
} satisfies Meta<typeof DiffArtifact>;
export default meta;
type Story = StoryObj<typeof meta>;

export const Review: Story = {
  play: async ({ canvas }) => {
    await userEvent.click(
      await canvas.findByRole('button', {
        name: 'Comment on the whole document',
      }),
    );
    await userEvent.type(
      canvas.getByRole('textbox', { name: 'Comment' }),
      'Keep archived sessions optional.',
    );
    await userEvent.click(canvas.getByRole('button', { name: 'Add comment' }));
    await expect(
      canvas.queryByRole('button', { name: 'Approve and start' }),
    ).not.toBeInTheDocument();
    await expect(canvas.queryByRole('button', { name: 'Save changes' })).not.toBeInTheDocument();
    await userEvent.click(canvas.getByRole('button', { name: 'Send for revision' }));
    await expect(canvas.getByRole('status')).toHaveTextContent('Revision requested for v4');
  },
};
export const Commented: Story = {
  args: { source: new Blob([JSON.stringify(fixture)]) },
};
export const ReadOnly: Story = {
  args: { commentable: false, source: new Blob([JSON.stringify(fixture)]) },
  play: async ({ canvas }) => {
    await canvas.findByText('+(search title {:archived true})', {
      selector: 'p',
    });
    await expect(
      canvas.queryByRole('button', { name: 'Comment on the whole document' }),
    ).not.toBeInTheDocument();
  },
};
export const Empty: Story = {
  args: {
    source: new Blob([JSON.stringify({ ...fixture, patch: '', comments: [] })]),
  },
};
export const Invalid: Story = {
  args: { source: new Blob(['invalid snapshot']) },
};
